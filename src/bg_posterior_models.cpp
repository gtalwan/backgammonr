// Posterior sampling and posterior-summary kernels for Thompson workflows.
#include <Rcpp.h>

#include <algorithm>
#include <cmath>
#include <limits>
#include <numeric>
#include <string>
#include <vector>

namespace {

// One compact per-action sufficient-stat bundle keeps the exported kernels
// independent from the higher-level R table shapes.
struct ActionStats {
  int allocation_count{0};
  int wins{0};
  int losses{0};
  int single_loss{0};
  int gammon_loss{0};
  int backgammon_loss{0};
  int unresolved{0};
  int single_win{0};
  int gammon_win{0};
  int backgammon_win{0};
  double reward_sum{0.0};
  double reward_sum_sq{0.0};
};

// Posterior summaries are built row-wise here, then assembled into a data
// frame for the R layer.
struct PosteriorSummaryRow {
  double estimate{NA_REAL};
  double posterior_sd{NA_REAL};
  double lower_95{NA_REAL};
  double upper_95{NA_REAL};
  double prob_best{NA_REAL};
  double expected_regret{NA_REAL};
  double alpha{NA_REAL};
  double beta{NA_REAL};
};

double clamp_unit_interval(const double x) {
  if (!R_finite(x)) {
    return x;
  }
  if (x < 0.0) {
    return 0.0;
  }
  if (x > 1.0) {
    return 1.0;
  }
  return x;
}

double safe_sample_variance(const ActionStats& stats) {
  if (stats.allocation_count <= 1) {
    return NA_REAL;
  }

  const double n = static_cast<double>(stats.allocation_count);
  const double raw = (stats.reward_sum_sq - ((stats.reward_sum * stats.reward_sum) / n)) / (n - 1.0);
  return std::max(0.0, raw);
}

double safe_empirical_mean(const ActionStats& stats, const double fallback) {
  if (stats.allocation_count <= 0) {
    return fallback;
  }
  return stats.reward_sum / static_cast<double>(stats.allocation_count);
}

double list_number_or_default(const Rcpp::List& x, const char* name, const double fallback) {
  if (!x.containsElementNamed(name)) {
    return fallback;
  }
  const Rcpp::NumericVector value = x[name];
  if (value.size() < 1 || !R_finite(value[0])) {
    return fallback;
  }
  return value[0];
}

Rcpp::NumericVector list_numeric_or_default(
    const Rcpp::List& x,
    const char* name,
    const Rcpp::NumericVector& fallback) {
  if (!x.containsElementNamed(name)) {
    return fallback;
  }
  const Rcpp::NumericVector value = x[name];
  if (value.size() == 0) {
    return fallback;
  }
  return value;
}

std::vector<ActionStats> materialize_stats(
    const Rcpp::IntegerVector& allocation_count,
    const Rcpp::IntegerVector& wins,
    const Rcpp::IntegerVector& losses,
    const Rcpp::IntegerVector& single_loss,
    const Rcpp::IntegerVector& gammon_loss,
    const Rcpp::IntegerVector& backgammon_loss,
    const Rcpp::IntegerVector& unresolved,
    const Rcpp::IntegerVector& single_win,
    const Rcpp::IntegerVector& gammon_win,
    const Rcpp::IntegerVector& backgammon_win,
    const Rcpp::NumericVector& reward_sum,
    const Rcpp::NumericVector& reward_sum_sq) {
  const int n_actions = allocation_count.size();
  if (wins.size() != n_actions ||
      losses.size() != n_actions ||
      single_loss.size() != n_actions ||
      gammon_loss.size() != n_actions ||
      backgammon_loss.size() != n_actions ||
      unresolved.size() != n_actions ||
      single_win.size() != n_actions ||
      gammon_win.size() != n_actions ||
      backgammon_win.size() != n_actions ||
      reward_sum.size() != n_actions ||
      reward_sum_sq.size() != n_actions) {
    Rcpp::stop("Posterior-kernel inputs must all have the same length.");
  }

  std::vector<ActionStats> out(static_cast<std::size_t>(n_actions));
  for (int i = 0; i < n_actions; ++i) {
    out[static_cast<std::size_t>(i)] = ActionStats{
      allocation_count[i],
      wins[i],
      losses[i],
      single_loss[i],
      gammon_loss[i],
      backgammon_loss[i],
      unresolved[i],
      single_win[i],
      gammon_win[i],
      backgammon_win[i],
      reward_sum[i],
      reward_sum_sq[i]
    };
  }
  return out;
}

// Preserve the scored-outcome representation here so the Dirichlet path can
// stay domain-faithful without extra R-side reshaping.
Rcpp::NumericVector scored_category_counts(const ActionStats& stats) {
  return Rcpp::NumericVector::create(
    static_cast<double>(stats.single_loss),
    static_cast<double>(stats.gammon_loss),
    static_cast<double>(stats.backgammon_loss),
    static_cast<double>(stats.unresolved),
    static_cast<double>(stats.single_win),
    static_cast<double>(stats.gammon_win),
    static_cast<double>(stats.backgammon_win)
  );
}

// Binary-style models treat unresolved outcomes as fractional success/failure
// according to the configured unresolved payoff.
void effective_binary_counts(
    const ActionStats& stats,
    const double unresolved_value,
    double& success,
    double& failure) {
  success = static_cast<double>(stats.wins) + unresolved_value * static_cast<double>(stats.unresolved);
  failure = static_cast<double>(stats.losses) + (1.0 - unresolved_value) * static_cast<double>(stats.unresolved);
}

// Shared conjugate update for the normal-inverse-gamma family used by both the
// direct NIG sampler and the Student-t marginal sampler.
void normal_inverse_gamma_posterior(
    const ActionStats& stats,
    const Rcpp::List& prior,
    double& mean_n,
    double& kappa_n,
    double& shape_n,
    double& scale_n) {
  const double mean0 = list_number_or_default(prior, "mean", 0.5);
  const double kappa0 = list_number_or_default(prior, "kappa", 1.0);
  const double shape0 = list_number_or_default(prior, "shape", 2.5);
  const double scale0 = list_number_or_default(prior, "scale", 0.125);

  if (stats.allocation_count <= 0) {
    mean_n = mean0;
    kappa_n = kappa0;
    shape_n = shape0;
    scale_n = scale0;
    return;
  }

  const double n = static_cast<double>(stats.allocation_count);
  const double xbar = stats.reward_sum / n;
  const double centered_ss = std::max(0.0, stats.reward_sum_sq - n * xbar * xbar);
  kappa_n = kappa0 + n;
  mean_n = ((kappa0 * mean0) + (n * xbar)) / kappa_n;
  shape_n = shape0 + 0.5 * n;
  scale_n = scale0 + 0.5 * centered_ss + (kappa0 * n * (xbar - mean0) * (xbar - mean0)) / (2.0 * kappa_n);
}

double dirichlet_linear_variance(
    const Rcpp::NumericVector& alpha,
    const Rcpp::NumericVector& payoff) {
  const double total = std::accumulate(alpha.begin(), alpha.end(), 0.0);
  if (!(total > 0.0)) {
    return NA_REAL;
  }
  double weighted_sum = 0.0;
  double weighted_square = 0.0;
  for (int i = 0; i < alpha.size(); ++i) {
    weighted_sum += alpha[i] * payoff[i];
    weighted_square += alpha[i] * payoff[i] * payoff[i];
  }
  const double variance = (total * weighted_square - (weighted_sum * weighted_sum)) /
    (total * total * (total + 1.0));
  return std::max(0.0, variance);
}

Rcpp::NumericVector categorical_alpha_prior(const Rcpp::List& prior) {
  return list_numeric_or_default(
    prior,
    "alpha",
    Rcpp::NumericVector::create(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
  );
}

Rcpp::NumericVector categorical_payoff_map(const Rcpp::List& prior, const double unresolved_value) {
  if (!prior.containsElementNamed("payoff")) {
    const Rcpp::NumericVector alpha = categorical_alpha_prior(prior);
    if (alpha.size() == 3) {
      return Rcpp::NumericVector::create(0.0, unresolved_value, 1.0);
    }
  }
  return list_numeric_or_default(
    prior,
    "payoff",
    Rcpp::NumericVector::create(1.0 / 3.0, 1.0 / 6.0, 0.0, unresolved_value, 2.0 / 3.0, 5.0 / 6.0, 1.0)
  );
}

Rcpp::NumericVector posterior_categorical_alpha(const ActionStats& stats, const Rcpp::List& prior) {
  const Rcpp::NumericVector alpha0 = categorical_alpha_prior(prior);
  if (alpha0.size() == 3) {
    return Rcpp::NumericVector::create(
      alpha0[0] + stats.losses,
      alpha0[1] + stats.unresolved,
      alpha0[2] + stats.wins
    );
  }
  if (alpha0.size() != 7) {
    Rcpp::stop("Dirichlet prior must contain three or seven positive entries.");
  }
  const Rcpp::NumericVector counts = scored_category_counts(stats);
  Rcpp::NumericVector out(alpha0.size());
  for (int i = 0; i < alpha0.size(); ++i) {
    out[i] = alpha0[i] + counts[i];
  }
  return out;
}

std::vector<int> ordered_index(const Rcpp::IntegerVector& x) {
  std::vector<int> ord(static_cast<std::size_t>(x.size()));
  std::iota(ord.begin(), ord.end(), 0);
  std::stable_sort(
    ord.begin(),
    ord.end(),
    [&](const int lhs, const int rhs) {
      return x[lhs] < x[rhs];
    }
  );
  return ord;
}

double trapezoid_area_sorted(
    const Rcpp::IntegerVector& x,
    const Rcpp::NumericVector& y,
    const std::vector<int>& ord) {
  if (ord.empty()) {
    return NA_REAL;
  }

  double area = 0.0;
  int prev_idx = -1;
  for (int pos = 0; pos < static_cast<int>(ord.size()); ++pos) {
    const int idx = ord[static_cast<std::size_t>(pos)];
    if (!R_finite(y[idx])) {
      continue;
    }
    if (prev_idx >= 0) {
      const double dx = static_cast<double>(x[idx] - x[prev_idx]);
      if (dx > 0.0) {
        area += dx * (y[idx] + y[prev_idx]) / 2.0;
      }
    }
    prev_idx = idx;
  }

  if (prev_idx < 0) {
    return NA_REAL;
  }
  if (area == 0.0) {
    return y[prev_idx];
  }
  return area;
}

double sampled_bootstrap_mean(
    const ActionStats& stats,
    const std::string& reward_model,
    const double unresolved_value,
    const Rcpp::List& prior,
    const double smoothing) {
  if (stats.allocation_count <= 0) {
    if (reward_model == "win_loss") {
      return 0.5;
    }
    if (reward_model == "categorical_outcome") {
      const Rcpp::NumericVector alpha0 = categorical_alpha_prior(prior);
      const Rcpp::NumericVector payoff = categorical_payoff_map(prior, unresolved_value);
      return std::inner_product(alpha0.begin(), alpha0.end(), payoff.begin(), 0.0) /
        std::accumulate(alpha0.begin(), alpha0.end(), 0.0);
    }
    return (1.0 + unresolved_value) / 3.0;
  }

  const double n = static_cast<double>(stats.allocation_count);
  if (reward_model == "win_loss") {
    double success = 0.0;
    double failure = 0.0;
    effective_binary_counts(stats, unresolved_value, success, failure);
    const double denom = n + 2.0 * smoothing;
    const double p_success = denom > 0.0
      ? (success + smoothing) / denom
      : 0.5;
    const double boot_success = R::rbinom(stats.allocation_count, p_success);
    return boot_success / n;
  }

  if (reward_model == "categorical_outcome") {
    const Rcpp::NumericVector payoff = categorical_payoff_map(prior, unresolved_value);
    Rcpp::NumericVector counts = payoff.size() == 3
      ? Rcpp::NumericVector::create(
          static_cast<double>(stats.losses),
          static_cast<double>(stats.unresolved),
          static_cast<double>(stats.wins))
      : scored_category_counts(stats);
    const double denom = n + static_cast<double>(counts.size()) * smoothing;
    Rcpp::NumericVector probs(counts.size(), 1.0 / counts.size());
    if (denom > 0.0) {
      for (int i = 0; i < counts.size(); ++i) {
        probs[i] = (counts[i] + smoothing) / denom;
      }
    }
    double total = 0.0;
    double allocated = 0.0;
    int remaining = stats.allocation_count;
    for (int i = 0; i < probs.size(); ++i) {
      const double tail_prob = std::accumulate(probs.begin() + i, probs.end(), 0.0);
      const double cond_prob = tail_prob > 0.0 ? probs[i] / tail_prob : 1.0;
      const double draw_count = (i + 1 == probs.size()) ? remaining : R::rbinom(remaining, cond_prob);
      total += draw_count * payoff[i];
      remaining -= static_cast<int>(draw_count);
      allocated += draw_count;
    }
    return allocated > 0.0 ? total / allocated : 0.5;
  }

  const double loss = static_cast<double>(stats.losses);
  const double unr = static_cast<double>(stats.unresolved);
  const double win = static_cast<double>(stats.wins);
  const double denom = n + 3.0 * smoothing;
  double p_loss = 1.0 / 3.0;
  double p_unr = 1.0 / 3.0;
  double p_win = 1.0 / 3.0;
  if (denom > 0.0) {
    p_loss = (loss + smoothing) / denom;
    p_unr = (unr + smoothing) / denom;
    p_win = (win + smoothing) / denom;
  }
  const double boot_loss = R::rbinom(stats.allocation_count, p_loss);
  const int remaining = stats.allocation_count - static_cast<int>(boot_loss);
  const double conditional_unr = (p_unr + p_win) > 0.0 ? p_unr / (p_unr + p_win) : 0.5;
  const double boot_unr = remaining > 0 ? R::rbinom(remaining, conditional_unr) : 0.0;
  const double boot_win = remaining - boot_unr;
  return (boot_win + unresolved_value * boot_unr) / n;
}

double sample_posterior_value(
    const ActionStats& stats,
    const std::string& reward_model,
    const std::string& posterior_model,
    const double unresolved_value,
    const Rcpp::List& prior) {
  // Exact or pseudo-conjugate beta updates for binary / bounded scalar paths.
  if (posterior_model == "beta_bernoulli" || posterior_model == "beta_pseudo") {
    double success = 0.0;
    double failure = 0.0;
    if (posterior_model == "beta_bernoulli") {
      effective_binary_counts(stats, unresolved_value, success, failure);
    } else {
      success = stats.reward_sum;
      failure = static_cast<double>(stats.allocation_count) - stats.reward_sum;
    }
    const double alpha = list_number_or_default(prior, "alpha", 1.0) + success;
    const double beta = list_number_or_default(prior, "beta", 1.0) + failure;
    return R::rbeta(alpha, beta);
  }

  // Dirichlet sampling for categorical scored outcomes, then a payoff-map
  // projection back to one scalar move value.
  if (posterior_model == "dirichlet_multinomial") {
    const Rcpp::NumericVector alpha = posterior_categorical_alpha(stats, prior);
    const Rcpp::NumericVector payoff = categorical_payoff_map(prior, unresolved_value);
    if (alpha.size() != payoff.size()) {
      Rcpp::stop("Dirichlet prior alpha and payoff vectors must have the same length.");
    }
    Rcpp::NumericVector gamma(alpha.size());
    double total = 0.0;
    for (int i = 0; i < alpha.size(); ++i) {
      gamma[i] = R::rgamma(alpha[i], 1.0);
      total += gamma[i];
    }
    if (!(total > 0.0)) {
      return std::accumulate(payoff.begin(), payoff.end(), 0.0) / static_cast<double>(payoff.size());
    }
    return std::inner_product(gamma.begin(), gamma.end(), payoff.begin(), 0.0) / total;
  }

  // Approximate scalar models keep the Thompson interface the same while
  // changing the posterior family under the hood.
  if (posterior_model == "gaussian_approx") {
    const double mean0 = list_number_or_default(prior, "mean", 0.5);
    const double weight0 = list_number_or_default(prior, "weight", 1.0);
    const double variance_floor = list_number_or_default(prior, "variance_floor", 0.125);
    const double empirical_mean = safe_empirical_mean(stats, mean0);
    double empirical_var = safe_sample_variance(stats);
    if (!R_finite(empirical_var)) {
      empirical_var = std::max(variance_floor, mean0 * (1.0 - mean0));
    } else {
      empirical_var = std::max(empirical_var, variance_floor);
    }
    const double total_weight = weight0 + static_cast<double>(stats.allocation_count);
    const double post_mean = ((weight0 * mean0) + (stats.allocation_count * empirical_mean)) / total_weight;
    const double post_sd = std::sqrt(empirical_var / total_weight);
    return clamp_unit_interval(R::rnorm(post_mean, post_sd));
  }

  if (posterior_model == "normal_inverse_gamma") {
    double mean_n = 0.5;
    double kappa_n = 1.0;
    double shape_n = 2.5;
    double scale_n = 0.125;
    normal_inverse_gamma_posterior(stats, prior, mean_n, kappa_n, shape_n, scale_n);
    const double gamma_draw = R::rgamma(shape_n, 1.0 / scale_n);
    const double sigma2 = 1.0 / gamma_draw;
    return clamp_unit_interval(R::rnorm(mean_n, std::sqrt(sigma2 / kappa_n)));
  }

  if (posterior_model == "student_t_marginal") {
    double mean_n = 0.5;
    double kappa_n = 1.0;
    double shape_n = 2.5;
    double scale_n = 0.125;
    normal_inverse_gamma_posterior(stats, prior, mean_n, kappa_n, shape_n, scale_n);
    const double df = 2.0 * shape_n;
    const double scale = std::sqrt(scale_n / (shape_n * kappa_n));
    return clamp_unit_interval(mean_n + scale * R::rt(df));
  }

  if (posterior_model == "bootstrap") {
    const double smoothing = list_number_or_default(prior, "smoothing", 0.0);
    return sampled_bootstrap_mean(stats, reward_model, unresolved_value, prior, smoothing);
  }

  Rcpp::stop("Unsupported posterior model.");
}

// Summaries use analytic formulas where available and draw-based quantiles
// otherwise, so the R layer can expose one regular summary table.
PosteriorSummaryRow summarize_action(
    const ActionStats& stats,
    const std::string& reward_model,
    const std::string& posterior_model,
    const double unresolved_value,
    const Rcpp::List& prior,
    const Rcpp::NumericMatrix& draw_matrix,
    const int action_index,
    const Rcpp::NumericVector& prob_best,
    const Rcpp::NumericVector& expected_regret) {
  PosteriorSummaryRow row;
  row.prob_best = prob_best[action_index];
  row.expected_regret = expected_regret[action_index];

  if (posterior_model == "beta_bernoulli" || posterior_model == "beta_pseudo") {
    double success = 0.0;
    double failure = 0.0;
    if (posterior_model == "beta_bernoulli") {
      effective_binary_counts(stats, unresolved_value, success, failure);
    } else {
      success = stats.reward_sum;
      failure = static_cast<double>(stats.allocation_count) - stats.reward_sum;
    }
    row.alpha = list_number_or_default(prior, "alpha", 1.0) + success;
    row.beta = list_number_or_default(prior, "beta", 1.0) + failure;
    const double total = row.alpha + row.beta;
    row.estimate = row.alpha / total;
    row.posterior_sd = std::sqrt((row.alpha * row.beta) / (total * total * (total + 1.0)));
    row.lower_95 = R::qbeta(0.025, row.alpha, row.beta, 1, 0);
    row.upper_95 = R::qbeta(0.975, row.alpha, row.beta, 1, 0);
    return row;
  }

  if (posterior_model == "dirichlet_multinomial") {
    const Rcpp::NumericVector alpha = posterior_categorical_alpha(stats, prior);
    const Rcpp::NumericVector payoff = categorical_payoff_map(prior, unresolved_value);
    const double total = std::accumulate(alpha.begin(), alpha.end(), 0.0);
    row.estimate = std::inner_product(alpha.begin(), alpha.end(), payoff.begin(), 0.0) / total;
    row.posterior_sd = std::sqrt(dirichlet_linear_variance(alpha, payoff));
  } else if (posterior_model == "gaussian_approx") {
    const double mean0 = list_number_or_default(prior, "mean", 0.5);
    const double weight0 = list_number_or_default(prior, "weight", 1.0);
    const double variance_floor = list_number_or_default(prior, "variance_floor", 0.125);
    const double empirical_mean = safe_empirical_mean(stats, mean0);
    double empirical_var = safe_sample_variance(stats);
    if (!R_finite(empirical_var)) {
      empirical_var = std::max(variance_floor, mean0 * (1.0 - mean0));
    } else {
      empirical_var = std::max(empirical_var, variance_floor);
    }
    const double total_weight = weight0 + static_cast<double>(stats.allocation_count);
    row.estimate = ((weight0 * mean0) + (stats.allocation_count * empirical_mean)) / total_weight;
    row.posterior_sd = std::sqrt(empirical_var / total_weight);
  } else if (posterior_model == "normal_inverse_gamma" || posterior_model == "student_t_marginal") {
    double mean_n = 0.5;
    double kappa_n = 1.0;
    double shape_n = 2.5;
    double scale_n = 0.125;
    normal_inverse_gamma_posterior(stats, prior, mean_n, kappa_n, shape_n, scale_n);
    row.estimate = mean_n;
    row.posterior_sd = shape_n > 1.0
      ? std::sqrt(scale_n / ((shape_n - 1.0) * kappa_n))
      : NA_REAL;
  } else {
    const int n_draws = draw_matrix.nrow();
    double sum = 0.0;
    double sum_sq = 0.0;
    for (int draw = 0; draw < n_draws; ++draw) {
      const double value = draw_matrix(draw, action_index);
      sum += value;
      sum_sq += value * value;
    }
    row.estimate = sum / static_cast<double>(n_draws);
    row.posterior_sd = n_draws > 1
      ? std::sqrt(std::max(0.0, (sum_sq - (sum * sum) / n_draws) / (n_draws - 1.0)))
      : 0.0;
  }

  std::vector<double> draws(draw_matrix.nrow());
  for (int draw = 0; draw < draw_matrix.nrow(); ++draw) {
    draws[static_cast<std::size_t>(draw)] = draw_matrix(draw, action_index);
  }
  std::sort(draws.begin(), draws.end());
  const int lower_idx = std::max(0, static_cast<int>(std::floor(0.025 * static_cast<double>(draws.size() - 1))));
  const int upper_idx = std::max(0, static_cast<int>(std::floor(0.975 * static_cast<double>(draws.size() - 1))));
  row.lower_95 = draws[static_cast<std::size_t>(lower_idx)];
  row.upper_95 = draws[static_cast<std::size_t>(upper_idx)];
  return row;
}

}  // namespace

// [[Rcpp::export]]
Rcpp::NumericMatrix bg_cpp_posterior_sample_values(
    const Rcpp::IntegerVector& allocation_count,
    const Rcpp::IntegerVector& wins,
    const Rcpp::IntegerVector& losses,
    const Rcpp::IntegerVector& single_loss,
    const Rcpp::IntegerVector& gammon_loss,
    const Rcpp::IntegerVector& backgammon_loss,
    const Rcpp::IntegerVector& unresolved,
    const Rcpp::IntegerVector& single_win,
    const Rcpp::IntegerVector& gammon_win,
    const Rcpp::IntegerVector& backgammon_win,
    const Rcpp::NumericVector& reward_sum,
    const Rcpp::NumericVector& reward_sum_sq,
    const std::string& reward_model,
    const std::string& posterior_model,
    const double unresolved_value,
    const Rcpp::List& posterior_prior,
    const int draws) {
  if (draws < 1) {
    Rcpp::stop("`draws` must be at least 1.");
  }

  Rcpp::RNGScope scope;
  const std::vector<ActionStats> stats = materialize_stats(
    allocation_count,
    wins,
    losses,
    single_loss,
    gammon_loss,
    backgammon_loss,
    unresolved,
    single_win,
    gammon_win,
    backgammon_win,
    reward_sum,
    reward_sum_sq
  );
  const int n_actions = allocation_count.size();
  Rcpp::NumericMatrix out(draws, n_actions);
  // Return the raw draw matrix so the R policy layer can implement several TS
  // variants without repeated stat reconstruction.
  for (int action = 0; action < n_actions; ++action) {
    for (int draw = 0; draw < draws; ++draw) {
      out(draw, action) = sample_posterior_value(
        stats[static_cast<std::size_t>(action)],
        reward_model,
        posterior_model,
        unresolved_value,
        posterior_prior
      );
    }
  }
  return out;
}

// [[Rcpp::export]]
Rcpp::DataFrame bg_cpp_posterior_summary(
    const Rcpp::IntegerVector& allocation_count,
    const Rcpp::IntegerVector& wins,
    const Rcpp::IntegerVector& losses,
    const Rcpp::IntegerVector& single_loss,
    const Rcpp::IntegerVector& gammon_loss,
    const Rcpp::IntegerVector& backgammon_loss,
    const Rcpp::IntegerVector& unresolved,
    const Rcpp::IntegerVector& single_win,
    const Rcpp::IntegerVector& gammon_win,
    const Rcpp::IntegerVector& backgammon_win,
    const Rcpp::NumericVector& reward_sum,
    const Rcpp::NumericVector& reward_sum_sq,
    const std::string& reward_model,
    const std::string& posterior_model,
    const double unresolved_value,
    const Rcpp::List& posterior_prior,
    const int draws) {
  if (draws < 2) {
    Rcpp::stop("`draws` must be at least 2 for posterior summaries.");
  }

  Rcpp::RNGScope scope;
  const std::vector<ActionStats> stats = materialize_stats(
    allocation_count,
    wins,
    losses,
    single_loss,
    gammon_loss,
    backgammon_loss,
    unresolved,
    single_win,
    gammon_win,
    backgammon_win,
    reward_sum,
    reward_sum_sq
  );
  const int n_actions = allocation_count.size();
  Rcpp::NumericMatrix draw_matrix = bg_cpp_posterior_sample_values(
    allocation_count,
    wins,
    losses,
    single_loss,
    gammon_loss,
    backgammon_loss,
    unresolved,
    single_win,
    gammon_win,
    backgammon_win,
    reward_sum,
    reward_sum_sq,
    reward_model,
    posterior_model,
    unresolved_value,
    posterior_prior,
    draws
  );

  Rcpp::NumericVector prob_best(n_actions);
  Rcpp::NumericVector expected_regret(n_actions);
  // Probability-best and model-relative expected regret are both Monte Carlo
  // summaries of the same draw matrix.
  for (int draw = 0; draw < draw_matrix.nrow(); ++draw) {
    double best_value = draw_matrix(draw, 0);
    int best_index = 0;
    for (int action = 1; action < n_actions; ++action) {
      if (draw_matrix(draw, action) > best_value) {
        best_value = draw_matrix(draw, action);
        best_index = action;
      }
    }
    prob_best[best_index] += 1.0;
    for (int action = 0; action < n_actions; ++action) {
      expected_regret[action] += (best_value - draw_matrix(draw, action));
    }
  }
  prob_best = prob_best / static_cast<double>(draw_matrix.nrow());
  expected_regret = expected_regret / static_cast<double>(draw_matrix.nrow());

  Rcpp::NumericVector estimate(n_actions);
  Rcpp::NumericVector posterior_sd(n_actions);
  Rcpp::NumericVector lower_95(n_actions);
  Rcpp::NumericVector upper_95(n_actions);
  Rcpp::NumericVector alpha(n_actions, NA_REAL);
  Rcpp::NumericVector beta(n_actions, NA_REAL);

  for (int action = 0; action < n_actions; ++action) {
    const PosteriorSummaryRow row = summarize_action(
      stats[static_cast<std::size_t>(action)],
      reward_model,
      posterior_model,
      unresolved_value,
      posterior_prior,
      draw_matrix,
      action,
      prob_best,
      expected_regret
    );
    estimate[action] = row.estimate;
    posterior_sd[action] = row.posterior_sd;
    lower_95[action] = row.lower_95;
    upper_95[action] = row.upper_95;
    alpha[action] = row.alpha;
    beta[action] = row.beta;
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("estimate") = estimate,
    Rcpp::Named("posterior_sd") = posterior_sd,
    Rcpp::Named("lower_95") = lower_95,
    Rcpp::Named("upper_95") = upper_95,
    Rcpp::Named("model_relative_prob_best") = prob_best,
    Rcpp::Named("model_relative_expected_regret") = expected_regret,
    Rcpp::Named("alpha") = alpha,
    Rcpp::Named("beta") = beta
  );
}

// [[Rcpp::export]]
Rcpp::DataFrame bg_cpp_reference_summary(
    const Rcpp::IntegerVector& allocation_count,
    const Rcpp::IntegerVector& unresolved,
    const Rcpp::NumericVector& reward_sum,
    const Rcpp::NumericVector& reward_sum_sq,
    const double prior_alpha,
    const double prior_beta) {
  const int n_actions = allocation_count.size();
  if (unresolved.size() != n_actions ||
      reward_sum.size() != n_actions ||
      reward_sum_sq.size() != n_actions) {
    Rcpp::stop("Reference-summary inputs must all have the same length.");
  }

  Rcpp::NumericVector reference_mean(n_actions, NA_REAL);
  Rcpp::NumericVector sample_variance(n_actions, NA_REAL);
  Rcpp::NumericVector reference_se(n_actions, NA_REAL);
  Rcpp::NumericVector reference_mc_lower_95(n_actions, NA_REAL);
  Rcpp::NumericVector reference_mc_upper_95(n_actions, NA_REAL);
  Rcpp::CharacterVector reference_interval_type(n_actions, "mc_normal_approx");
  Rcpp::NumericVector reference_alpha(n_actions, NA_REAL);
  Rcpp::NumericVector reference_beta(n_actions, NA_REAL);
  Rcpp::NumericVector unresolved_fraction(n_actions, NA_REAL);

  // Proxy-reference summaries are simple Monte Carlo summaries of realized
  // rollout rewards, not posterior intervals.
  for (int i = 0; i < n_actions; ++i) {
    const double n = static_cast<double>(allocation_count[i]);
    if (n > 0.0) {
      reference_mean[i] = reward_sum[i] / n;
      reference_alpha[i] = prior_alpha + reward_sum[i];
      reference_beta[i] = prior_beta + (n - reward_sum[i]);
      unresolved_fraction[i] = static_cast<double>(unresolved[i]) / n;
      if (n > 1.0) {
        const double raw = (reward_sum_sq[i] - ((reward_sum[i] * reward_sum[i]) / n)) / (n - 1.0);
        sample_variance[i] = std::max(0.0, raw);
      }
      const double variance_for_se = R_finite(sample_variance[i]) ? sample_variance[i] : 0.0;
      reference_se[i] = std::sqrt(std::max(0.0, variance_for_se) / n);
      reference_mc_lower_95[i] = clamp_unit_interval(reference_mean[i] - 1.96 * reference_se[i]);
      reference_mc_upper_95[i] = clamp_unit_interval(reference_mean[i] + 1.96 * reference_se[i]);
    }
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("reference_mean") = reference_mean,
    Rcpp::Named("sample_variance") = sample_variance,
    Rcpp::Named("reference_se") = reference_se,
    Rcpp::Named("reference_mc_lower_95") = reference_mc_lower_95,
    Rcpp::Named("reference_mc_upper_95") = reference_mc_upper_95,
    Rcpp::Named("reference_interval_type") = reference_interval_type,
    Rcpp::Named("reference_alpha") = reference_alpha,
    Rcpp::Named("reference_beta") = reference_beta,
    Rcpp::Named("unresolved_fraction") = unresolved_fraction
  );
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_eval_path_metrics(
    const Rcpp::IntegerVector& checkpoint,
    const Rcpp::NumericVector& runtime_seconds,
    const Rcpp::LogicalVector& top1_match,
    const Rcpp::LogicalVector& epsilon_optimal,
    const Rcpp::NumericVector& simple_regret,
    const Rcpp::NumericVector& recommended_prob_best) {
  const int n = checkpoint.size();
  if (runtime_seconds.size() != n ||
      top1_match.size() != n ||
      epsilon_optimal.size() != n ||
      simple_regret.size() != n ||
      recommended_prob_best.size() != n) {
    Rcpp::stop("Path-metric inputs must all have the same length.");
  }
  if (n < 1) {
    Rcpp::stop("Path metrics require at least one checkpoint.");
  }

  const std::vector<int> ord = ordered_index(checkpoint);
  double first_budget_top1_match = NA_REAL;
  double first_budget_epsilon_optimal = NA_REAL;
  double first_runtime_top1_match = NA_REAL;
  double first_runtime_epsilon_optimal = NA_REAL;
  double mean_brier_top1 = NA_REAL;
  double max_runtime_seconds = NA_REAL;
  int max_checkpoint = checkpoint[ord.back()];
  int brier_n = 0;
  double brier_sum = 0.0;

  Rcpp::NumericVector top1_numeric(n, NA_REAL);
  for (int pos = 0; pos < n; ++pos) {
    const int idx = ord[static_cast<std::size_t>(pos)];
    if (top1_match[idx] != NA_LOGICAL) {
      top1_numeric[idx] = top1_match[idx] ? 1.0 : 0.0;
      if (!R_finite(first_budget_top1_match) && top1_match[idx]) {
        first_budget_top1_match = static_cast<double>(checkpoint[idx]);
        first_runtime_top1_match = runtime_seconds[idx];
      }
    }
    if (epsilon_optimal[idx] != NA_LOGICAL && !R_finite(first_budget_epsilon_optimal) && epsilon_optimal[idx]) {
      first_budget_epsilon_optimal = static_cast<double>(checkpoint[idx]);
      first_runtime_epsilon_optimal = runtime_seconds[idx];
    }
    if (R_finite(runtime_seconds[idx])) {
      max_runtime_seconds = runtime_seconds[idx];
    }
    if (top1_match[idx] != NA_LOGICAL && R_finite(recommended_prob_best[idx])) {
      const double outcome = top1_match[idx] ? 1.0 : 0.0;
      const double diff = clamp_unit_interval(recommended_prob_best[idx]) - outcome;
      brier_sum += diff * diff;
      brier_n += 1;
    }
  }

  if (brier_n > 0) {
    mean_brier_top1 = brier_sum / static_cast<double>(brier_n);
  }

  return Rcpp::List::create(
    Rcpp::Named("first_budget_top1_match") = first_budget_top1_match,
    Rcpp::Named("first_budget_epsilon_optimal") = first_budget_epsilon_optimal,
    Rcpp::Named("first_runtime_top1_match") = first_runtime_top1_match,
    Rcpp::Named("first_runtime_epsilon_optimal") = first_runtime_epsilon_optimal,
    Rcpp::Named("auc_top1_match") = trapezoid_area_sorted(checkpoint, top1_numeric, ord),
    Rcpp::Named("auc_simple_regret") = trapezoid_area_sorted(checkpoint, simple_regret, ord),
    Rcpp::Named("mean_brier_top1") = mean_brier_top1,
    Rcpp::Named("n_checkpoints") = n,
    Rcpp::Named("max_checkpoint") = max_checkpoint,
    Rcpp::Named("max_runtime_seconds") = max_runtime_seconds
  );
}

// [[Rcpp::export]]
Rcpp::DataFrame bg_cpp_calibration_summary(
    const Rcpp::NumericVector& predicted_prob,
    const Rcpp::NumericVector& observed_top1,
    const int bins) {
  const int n = predicted_prob.size();
  if (observed_top1.size() != n) {
    Rcpp::stop("Calibration inputs must have the same length.");
  }
  if (bins < 1) {
    Rcpp::stop("`bins` must be at least 1.");
  }

  Rcpp::IntegerVector calibration_bin(bins);
  Rcpp::NumericVector bin_lower(bins);
  Rcpp::NumericVector bin_upper(bins);
  Rcpp::IntegerVector count(bins);
  Rcpp::NumericVector sum_predicted(bins);
  Rcpp::NumericVector sum_observed(bins);
  Rcpp::NumericVector sum_brier(bins);
  int valid_n = 0;

  for (int i = 0; i < bins; ++i) {
    calibration_bin[i] = i + 1;
    bin_lower[i] = static_cast<double>(i) / static_cast<double>(bins);
    bin_upper[i] = static_cast<double>(i + 1) / static_cast<double>(bins);
  }

  for (int i = 0; i < n; ++i) {
    if (!R_finite(predicted_prob[i]) || !R_finite(observed_top1[i])) {
      continue;
    }
    const double p = clamp_unit_interval(predicted_prob[i]);
    const double y = observed_top1[i] > 0.0 ? 1.0 : 0.0;
    int bin_id = static_cast<int>(std::floor(p * static_cast<double>(bins)));
    if (bin_id >= bins) {
      bin_id = bins - 1;
    }
    count[bin_id] += 1;
    sum_predicted[bin_id] += p;
    sum_observed[bin_id] += y;
    const double diff = p - y;
    sum_brier[bin_id] += diff * diff;
    valid_n += 1;
  }

  Rcpp::NumericVector mean_predicted_prob_best(bins, NA_REAL);
  Rcpp::NumericVector observed_top1_rate(bins, NA_REAL);
  Rcpp::NumericVector mean_brier_top1(bins, NA_REAL);
  Rcpp::NumericVector calibration_gap(bins, NA_REAL);
  Rcpp::NumericVector ece_component(bins, NA_REAL);

  for (int i = 0; i < bins; ++i) {
    if (count[i] <= 0) {
      continue;
    }
    const double denom = static_cast<double>(count[i]);
    mean_predicted_prob_best[i] = sum_predicted[i] / denom;
    observed_top1_rate[i] = sum_observed[i] / denom;
    mean_brier_top1[i] = sum_brier[i] / denom;
    calibration_gap[i] = observed_top1_rate[i] - mean_predicted_prob_best[i];
    ece_component[i] = valid_n > 0
      ? std::abs(calibration_gap[i]) * (denom / static_cast<double>(valid_n))
      : NA_REAL;
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("calibration_bin") = calibration_bin,
    Rcpp::Named("bin_lower") = bin_lower,
    Rcpp::Named("bin_upper") = bin_upper,
    Rcpp::Named("n") = count,
    Rcpp::Named("mean_predicted_prob_best") = mean_predicted_prob_best,
    Rcpp::Named("observed_top1_rate") = observed_top1_rate,
    Rcpp::Named("mean_brier_top1") = mean_brier_top1,
    Rcpp::Named("calibration_gap") = calibration_gap,
    Rcpp::Named("ece_component") = ece_component
  );
}

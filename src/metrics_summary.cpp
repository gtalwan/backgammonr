// Posterior draw assembly and summary tables.
//
// Purpose:
// - materialize posterior-draw matrices for explicit posterior models;
// - turn those draws into probability-best, regret, interval, and calibration
//   summaries; and
// - summarize empirical proxy-reference rollout tables without confusing them
//   with posterior objects.
//
// Inputs:
// - per-action sufficient statistics from rollout allocation runs; or
// - posterior draw matrices already sampled from the chosen model family.
//
// Outputs:
// - one consistent R-facing summary schema across posterior families; and
// - compact path-level and calibration summaries used by diagnostics/studies.

#include "posterior_core.h"

#include <algorithm>
#include <cmath>
#include <vector>

namespace {

void fill_draw_quantiles(
    const Rcpp::NumericMatrix& draw_matrix,
    const int action_index,
    double& lower_95,
    double& upper_95) {
  // Copy one column out before sorting so the caller can keep the original
  // draw matrix intact for probability-best and regret summaries.
  std::vector<double> draws(static_cast<std::size_t>(draw_matrix.nrow()));
  for (int draw = 0; draw < draw_matrix.nrow(); ++draw) {
    draws[static_cast<std::size_t>(draw)] = draw_matrix(draw, action_index);
  }
  std::sort(draws.begin(), draws.end());
  // Use simple empirical quantile indices. The summaries are explanatory
  // diagnostics rather than a contract for one specialized quantile estimator.
  const int lower_idx = std::max(0, static_cast<int>(std::floor(0.025 * static_cast<double>(draws.size() - 1))));
  const int upper_idx = std::max(0, static_cast<int>(std::floor(0.975 * static_cast<double>(draws.size() - 1))));
  lower_95 = draws[static_cast<std::size_t>(lower_idx)];
  upper_95 = draws[static_cast<std::size_t>(upper_idx)];
}

backgammonr::posterior::PosteriorSummaryRow summarize_draw_only_action(
    const Rcpp::NumericMatrix& draw_matrix,
    const int action_index) {
  // Bootstrap and other draw-only paths do not have a closed-form posterior
  // summary, so estimate mean / sd / quantiles directly from the sampled
  // posterior draw column.
  backgammonr::posterior::PosteriorSummaryRow row;
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
  fill_draw_quantiles(draw_matrix, action_index, row.lower_95, row.upper_95);
  return row;
}

}  // namespace

namespace backgammonr {
namespace posterior {

double sample_posterior_value(
    const ActionStats& stats,
    const std::string& reward_model,
    const std::string& posterior_model,
    const double unresolved_value,
    const Rcpp::List& prior) {
  if (posterior_model == "beta_bernoulli" || posterior_model == "beta_pseudo") {
    return sample_beta_family_value(stats, posterior_model, unresolved_value, prior);
  }

  if (posterior_model == "dirichlet_multinomial") {
    return sample_dirichlet_value(stats, unresolved_value, prior);
  }

  if (posterior_model == "gaussian_approx" ||
      posterior_model == "normal_inverse_gamma" ||
      posterior_model == "student_t_marginal") {
    return sample_scalar_model_value(stats, posterior_model, unresolved_value, prior);
  }

  if (posterior_model == "bootstrap") {
    return sample_bootstrap_value(stats, reward_model, unresolved_value, prior);
  }

  Rcpp::stop("Unsupported posterior model.");
}

Rcpp::NumericMatrix posterior_draw_matrix(
    const std::vector<ActionStats>& stats,
    const std::string& reward_model,
    const std::string& posterior_model,
    const double unresolved_value,
    const Rcpp::List& posterior_prior,
    const int draws) {
  if (draws < 1) {
    Rcpp::stop("`draws` must be at least 1.");
  }

  const int n_actions = static_cast<int>(stats.size());
  Rcpp::NumericMatrix out(draws, n_actions);
  // Draws are laid out row = posterior replication, col = action so later code
  // can compute probability-best and regret with one pass over each draw.
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

void posterior_draw_metrics(
    const Rcpp::NumericMatrix& draw_matrix,
    Rcpp::NumericVector& prob_best,
    Rcpp::NumericVector& expected_regret) {
  // For each posterior replication, identify the sampled winner and accumulate
  // regret relative to that draw's best sampled value. This keeps the
  // probability-best and expected-regret summaries consistent across posterior
  // families.
  const int n_actions = draw_matrix.ncol();
  prob_best = Rcpp::NumericVector(n_actions);
  expected_regret = Rcpp::NumericVector(n_actions);

  for (int draw = 0; draw < draw_matrix.nrow(); ++draw) {
    double best_value = draw_matrix(draw, 0);
    int best_index = 0;
    for (int action = 1; action < n_actions; ++action) {
      // Deterministic first-max tie-breaking keeps probability-best tallies
      // reproducible when two sampled actions land on the same draw value.
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
}

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
  // Family-specific summaries differ in whether they are analytic or
  // draw-based, but the exported row schema is uniform across families.
  PosteriorSummaryRow row;
  row.prob_best = prob_best[action_index];
  row.expected_regret = expected_regret[action_index];

  if (posterior_model == "beta_bernoulli" || posterior_model == "beta_pseudo") {
    PosteriorSummaryRow analytic = summarize_beta_family(stats, posterior_model, unresolved_value, prior);
    analytic.prob_best = row.prob_best;
    analytic.expected_regret = row.expected_regret;
    return analytic;
  }

  if (posterior_model == "dirichlet_multinomial") {
    row = summarize_dirichlet_family(stats, unresolved_value, prior);
    row.prob_best = prob_best[action_index];
    row.expected_regret = expected_regret[action_index];
    fill_draw_quantiles(draw_matrix, action_index, row.lower_95, row.upper_95);
    return row;
  }

  if (posterior_model == "gaussian_approx" ||
      posterior_model == "normal_inverse_gamma" ||
      posterior_model == "student_t_marginal") {
    row = summarize_scalar_model(stats, posterior_model, prior);
    row.prob_best = prob_best[action_index];
    row.expected_regret = expected_regret[action_index];
    fill_draw_quantiles(draw_matrix, action_index, row.lower_95, row.upper_95);
    return row;
  }

  if (posterior_model == "bootstrap") {
    row = summarize_draw_only_action(draw_matrix, action_index);
    row.prob_best = prob_best[action_index];
    row.expected_regret = expected_regret[action_index];
    return row;
  }

  (void) reward_model;
  Rcpp::stop("Unsupported posterior model.");
}

Rcpp::DataFrame reference_summary(
    const Rcpp::IntegerVector& allocation_count,
    const Rcpp::IntegerVector& unresolved,
    const Rcpp::NumericVector& reward_sum,
    const Rcpp::NumericVector& reward_sum_sq,
    const double prior_alpha,
    const double prior_beta) {
  // Proxy-reference summaries are Monte Carlo summaries of empirical rollout
  // means. The alpha/beta columns are carried along only so the scalar TS layer
  // can reuse the same sufficient-stat representation.
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

  for (int i = 0; i < n_actions; ++i) {
    const double n = static_cast<double>(allocation_count[i]);
    if (n > 0.0) {
      // Empirical reference mean = observed average rollout reward for this
      // candidate under the declared truth-building environment.
      reference_mean[i] = reward_sum[i] / n;
      // Alpha/beta are carried along only as a convenient scalar summary for
      // downstream code; they are metadata here, not the definition of truth.
      reference_alpha[i] = prior_alpha + reward_sum[i];
      reference_beta[i] = prior_beta + (n - reward_sum[i]);
      unresolved_fraction[i] = static_cast<double>(unresolved[i]) / n;
      if (n > 1.0) {
        // Unbiased sample variance of bounded rollout rewards.
        const double raw = (reward_sum_sq[i] - ((reward_sum[i] * reward_sum[i]) / n)) / (n - 1.0);
        sample_variance[i] = std::max(0.0, raw);
      }
      const double variance_for_se = R_finite(sample_variance[i]) ? sample_variance[i] : 0.0;
      // Normal-approximation Monte Carlo intervals are intentionally framed as
      // MC precision diagnostics, not posterior credible intervals.
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

Rcpp::List eval_path_metrics(
    const Rcpp::IntegerVector& checkpoint,
    const Rcpp::NumericVector& runtime_seconds,
    const Rcpp::LogicalVector& top1_match,
    const Rcpp::LogicalVector& epsilon_optimal,
    const Rcpp::NumericVector& simple_regret,
    const Rcpp::NumericVector& recommended_prob_best) {
  // Path metrics treat checkpoints as an ordered budget path, not as IID rows.
  // The returned AUC and "first budget correct" summaries are therefore
  // checkpoint-path diagnostics for one run.
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
  const int max_checkpoint = checkpoint[ord.back()];
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
      // Brier score compares the run's stated probability-best confidence to
      // the realized top-1 correctness at that checkpoint.
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

Rcpp::DataFrame calibration_summary(
    const Rcpp::NumericVector& predicted_prob,
    const Rcpp::NumericVector& observed_top1,
    const int bins) {
  // Calibration is operational and model-relative here: bin predicted
  // probability-best values and compare them to observed top-1 correctness.
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
    // Clamp predictions into [0, 1] before binning so malformed upstream
    // values do not break calibration summaries.
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
    // ECE contribution = |gap| weighted by this bin's share of valid rows.
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

}  // namespace posterior
}  // namespace backgammonr

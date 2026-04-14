// Dirichlet-multinomial posterior kernels.
//
// Purpose:
// - provide the exact conjugate posterior for multi-category rollout outcomes;
// - support both the collapsed three-category path and the full seven-category
//   scored-outcome path; and
// - project categorical posterior uncertainty back onto one scalar reward scale
//   through an explicit payoff map.
//
// Reward variable:
// - categorical scored rollout outcome, either 3 collapsed categories
//   (loss/unresolved/win) or the full 7-category score class.
//
// Sufficient statistics:
// - category counts reconstructed from rollout outcome tallies.
//
// Status:
// - exact conjugate for the categorical outcome model.
//
// Output contract:
// - `sample_dirichlet_value()` returns one scalar reward draw implied by a
//   Dirichlet draw over the outcome simplex;
// - `summarize_dirichlet_family()` returns posterior mean and posterior SD for
//   that same scalar reward functional.

#include "posterior_core.h"

#include <numeric>

namespace backgammonr {
namespace posterior {

Rcpp::NumericVector scored_category_counts(const ActionStats& stats) {
  // Keep the category order fixed because the payoff map relies on positional
  // alignment with these counts.
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

double dirichlet_linear_variance(
    const Rcpp::NumericVector& alpha,
    const Rcpp::NumericVector& payoff) {
  // Total posterior concentration controls how sharply the categorical mean is
  // identified.
  const double total = std::accumulate(alpha.begin(), alpha.end(), 0.0);
  if (!(total > 0.0)) {
    return NA_REAL;
  }
  double weighted_sum = 0.0;
  double weighted_square = 0.0;
  for (int i = 0; i < alpha.size(); ++i) {
    // First moment of the linear payoff under the posterior Dirichlet mean.
    weighted_sum += alpha[i] * payoff[i];
    // Second moment term needed for the linear functional variance.
    weighted_square += alpha[i] * payoff[i] * payoff[i];
  }
  // Closed-form variance of a linear functional under a Dirichlet posterior.
  const double variance = (total * weighted_square - (weighted_sum * weighted_sum)) /
    (total * total * (total + 1.0));
  return std::max(0.0, variance);
}

Rcpp::NumericVector categorical_alpha_prior(const Rcpp::List& prior) {
  // Default to a symmetric weak prior unless the caller supplied explicit
  // category-level pseudo-counts.
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
      // Three-category collapsed path: loss / unresolved / win.
      return Rcpp::NumericVector::create(0.0, unresolved_value, 1.0);
    }
  }
  // Seven-category scored-outcome path: single/gammon/backgammon loss,
  // unresolved, then single/gammon/backgammon win.
  return list_numeric_or_default(
    prior,
    "payoff",
    Rcpp::NumericVector::create(1.0 / 3.0, 1.0 / 6.0, 0.0, unresolved_value, 2.0 / 3.0, 5.0 / 6.0, 1.0)
  );
}

Rcpp::NumericVector posterior_categorical_alpha(const ActionStats& stats, const Rcpp::List& prior) {
  const Rcpp::NumericVector alpha0 = categorical_alpha_prior(prior);
  if (alpha0.size() == 3) {
    // Collapsed categorical model adds loss, unresolved, and win counts to the
    // corresponding prior pseudo-counts.
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
    // Posterior alpha_i = prior alpha_i + observed category count_i.
    out[i] = alpha0[i] + counts[i];
  }
  return out;
}

double sample_dirichlet_value(
    const ActionStats& stats,
    const double unresolved_value,
    const Rcpp::List& prior) {
  const Rcpp::NumericVector alpha = posterior_categorical_alpha(stats, prior);
  const Rcpp::NumericVector payoff = categorical_payoff_map(prior, unresolved_value);
  if (alpha.size() != payoff.size()) {
    Rcpp::stop("Dirichlet prior alpha and payoff vectors must have the same length.");
  }

  Rcpp::NumericVector gamma(alpha.size());
  double total = 0.0;
  for (int i = 0; i < alpha.size(); ++i) {
    // Sample independent Gamma(alpha_i, 1) variates to form a Dirichlet draw.
    gamma[i] = R::rgamma(alpha[i], 1.0);
    // The sum normalizes the draw back onto the simplex.
    total += gamma[i];
  }
  if (!(total > 0.0)) {
    // Degenerate safeguard: fall back to the simple average payoff if the
    // Gamma draws underflow collectively.
    return std::accumulate(payoff.begin(), payoff.end(), 0.0) / static_cast<double>(payoff.size());
  }
  // The scalar reward draw is the simplex draw projected through the payoff
  // mapping.
  return std::inner_product(gamma.begin(), gamma.end(), payoff.begin(), 0.0) / total;
}

PosteriorSummaryRow summarize_dirichlet_family(
    const ActionStats& stats,
    const double unresolved_value,
    const Rcpp::List& prior) {
  PosteriorSummaryRow row;
  const Rcpp::NumericVector alpha = posterior_categorical_alpha(stats, prior);
  const Rcpp::NumericVector payoff = categorical_payoff_map(prior, unresolved_value);
  const double total = std::accumulate(alpha.begin(), alpha.end(), 0.0);
  // Posterior mean of the scalar reward induced by the categorical payoff map.
  row.estimate = std::inner_product(alpha.begin(), alpha.end(), payoff.begin(), 0.0) / total;
  // Posterior standard deviation from the closed-form linear-functional
  // Dirichlet variance above.
  row.posterior_sd = std::sqrt(dirichlet_linear_variance(alpha, payoff));
  return row;
}

}  // namespace posterior
}  // namespace backgammonr

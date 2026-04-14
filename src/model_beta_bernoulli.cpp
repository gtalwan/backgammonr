// Beta-family posterior kernels.
//
// Purpose:
// - provide the exact Beta-Bernoulli update used for binary win/loss studies;
// - provide the pseudo-Beta approximation used by the scalar legacy stack; and
// - keep the sufficient-stat reduction shared between posterior sampling and
//   posterior summary output.
//
// Reward variable:
// - `beta_bernoulli`: binary win/loss reward, with unresolved outcomes folded
//   in as fractional success/failure mass on [0, 1].
// - `beta_pseudo`: bounded scalar payoff treated as pseudo-Bernoulli mass.
//
// Sufficient statistics:
// - wins, losses, unresolved for `beta_bernoulli`
// - reward_sum and allocation_count for `beta_pseudo`
//
// Status:
// - `beta_bernoulli` is exact conjugate for the binary reward model.
// - `beta_pseudo` is an approximation for bounded scalar payoffs.
//
// Output contract:
// - `sample_beta_family_value()` returns one posterior draw of the mean reward;
// - `summarize_beta_family()` returns analytic posterior moments and a 95%
//   equal-tail interval for plotting and diagnostics.

#include "posterior_core.h"

#include <cmath>

namespace backgammonr {
namespace posterior {

void effective_binary_counts(
    const ActionStats& stats,
    const double unresolved_value,
    double& success,
    double& failure) {
  // For the exact Bernoulli model, wins contribute full success mass.
  // For the pseudo-Beta model, unresolved outcomes are treated as fractional
  // success/failure mass at the configured unresolved reward.
  success = static_cast<double>(stats.wins) + unresolved_value * static_cast<double>(stats.unresolved);
  // The complementary mass is the remaining probability-scale reward budget.
  failure = static_cast<double>(stats.losses) + (1.0 - unresolved_value) * static_cast<double>(stats.unresolved);
}

double sample_beta_family_value(
    const ActionStats& stats,
    const std::string& posterior_model,
    const double unresolved_value,
    const Rcpp::List& prior) {
  double success = 0.0;
  double failure = 0.0;
  if (posterior_model == "beta_bernoulli") {
    // Exact conjugate path for binary reward with unresolved outcomes folded
    // into the binary scale via the unresolved-value convention.
    effective_binary_counts(stats, unresolved_value, success, failure);
  } else if (posterior_model == "beta_pseudo") {
    // Approximate scalar-payoff path: total reward plays the role of
    // cumulative "success" mass and the leftover rollout budget becomes
    // cumulative "failure" mass.
    success = stats.reward_sum;
    failure = static_cast<double>(stats.allocation_count) - stats.reward_sum;
  } else {
    Rcpp::stop("Unsupported beta-family posterior model.");
  }

  // Posterior alpha = prior alpha + success mass.
  const double alpha = list_number_or_default(prior, "alpha", 1.0) + success;
  // Posterior beta = prior beta + failure mass.
  const double beta = list_number_or_default(prior, "beta", 1.0) + failure;
  // Thompson sampling needs one draw from the posterior over the mean reward.
  return R::rbeta(alpha, beta);
}

PosteriorSummaryRow summarize_beta_family(
    const ActionStats& stats,
    const std::string& posterior_model,
    const double unresolved_value,
    const Rcpp::List& prior) {
  PosteriorSummaryRow row;

  double success = 0.0;
  double failure = 0.0;
  if (posterior_model == "beta_bernoulli") {
    // Same sufficient-stat reduction as the draw path above.
    effective_binary_counts(stats, unresolved_value, success, failure);
  } else if (posterior_model == "beta_pseudo") {
    // Same pseudo-count interpretation for bounded scalar payoffs.
    success = stats.reward_sum;
    failure = static_cast<double>(stats.allocation_count) - stats.reward_sum;
  } else {
    Rcpp::stop("Unsupported beta-family posterior model.");
  }

  // Analytic posterior alpha after observing the current sufficient stats.
  row.alpha = list_number_or_default(prior, "alpha", 1.0) + success;
  // Analytic posterior beta after observing the current sufficient stats.
  row.beta = list_number_or_default(prior, "beta", 1.0) + failure;
  // Total pseudo-count mass determines the posterior mean and variance scale.
  const double total = row.alpha + row.beta;
  // Posterior mean of a Beta(alpha, beta) random variable.
  row.estimate = row.alpha / total;
  // Closed-form posterior standard deviation for the Beta mean parameter.
  row.posterior_sd = std::sqrt((row.alpha * row.beta) / (total * total * (total + 1.0)));
  // Equal-tail 95% interval from the exact Beta quantile function.
  row.lower_95 = R::qbeta(0.025, row.alpha, row.beta, 1, 0);
  row.upper_95 = R::qbeta(0.975, row.alpha, row.beta, 1, 0);
  return row;
}

}  // namespace posterior
}  // namespace backgammonr

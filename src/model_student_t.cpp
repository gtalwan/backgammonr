// Scalar-payoff approximate posterior kernels.
//
// Reward variable:
// - bounded scalar rollout payoff on [0, 1].
//
// Sufficient statistics:
// - allocation_count, reward_sum, reward_sum_sq.
//
// Status:
// - `gaussian_approx` is a plug-in approximation.
// - `normal_inverse_gamma` and `student_t_marginal` share one conjugate update
//   for a normal mean with unknown variance, then differ in how they sample or
//   summarize the mean.

#include "posterior_core.h"

#include <cmath>

namespace backgammonr {
namespace posterior {

void normal_inverse_gamma_posterior(
    const ActionStats& stats,
    const Rcpp::List& prior,
    double& mean_n,
    double& kappa_n,
    double& shape_n,
    double& scale_n) {
  // Prior mean of the unknown scalar payoff mean.
  const double mean0 = list_number_or_default(prior, "mean", 0.5);
  // Prior effective sample size on the mean parameter.
  const double kappa0 = list_number_or_default(prior, "kappa", 1.0);
  // Prior inverse-gamma shape for the unknown variance.
  const double shape0 = list_number_or_default(prior, "shape", 2.5);
  // Prior inverse-gamma scale for the unknown variance.
  const double scale0 = list_number_or_default(prior, "scale", 0.125);

  if (stats.allocation_count <= 0) {
    // With no data, posterior = prior.
    mean_n = mean0;
    kappa_n = kappa0;
    shape_n = shape0;
    scale_n = scale0;
    return;
  }

  const double n = static_cast<double>(stats.allocation_count);
  // Sample mean of the observed scalar rewards.
  const double xbar = stats.reward_sum / n;
  // Centered sum of squares from the stored first and second moments.
  const double centered_ss = std::max(0.0, stats.reward_sum_sq - n * xbar * xbar);
  // Posterior effective sample size on the mean.
  kappa_n = kappa0 + n;
  // Posterior mean is the precision-weighted average of prior mean and sample mean.
  mean_n = ((kappa0 * mean0) + (n * xbar)) / kappa_n;
  // Posterior shape adds n / 2 from the Gaussian likelihood.
  shape_n = shape0 + 0.5 * n;
  // Posterior scale combines within-sample dispersion and prior/sample mean disagreement.
  scale_n = scale0 + 0.5 * centered_ss + (kappa0 * n * (xbar - mean0) * (xbar - mean0)) / (2.0 * kappa_n);
}

double sample_scalar_model_value(
    const ActionStats& stats,
    const std::string& posterior_model,
    const double unresolved_value,
    const Rcpp::List& prior) {
  (void) unresolved_value;

  if (posterior_model == "gaussian_approx") {
    // Plug-in Gaussian approximation around the empirical mean.
    const double mean0 = list_number_or_default(prior, "mean", 0.5);
    const double weight0 = list_number_or_default(prior, "weight", 1.0);
    const double variance_floor = list_number_or_default(prior, "variance_floor", 0.125);
    const double empirical_mean = safe_empirical_mean(stats, mean0);
    double empirical_var = safe_sample_variance(stats);
    if (!R_finite(empirical_var)) {
      // With no stable empirical variance, fall back to a bounded-reward-scale default.
      empirical_var = std::max(variance_floor, mean0 * (1.0 - mean0));
    } else {
      // Never let the approximation collapse onto zero variance.
      empirical_var = std::max(empirical_var, variance_floor);
    }
    // Precision-weighted posterior mean under the approximation.
    const double total_weight = weight0 + static_cast<double>(stats.allocation_count);
    const double post_mean = ((weight0 * mean0) + (stats.allocation_count * empirical_mean)) / total_weight;
    // Approximate posterior standard deviation of the mean.
    const double post_sd = std::sqrt(empirical_var / total_weight);
    // Clamp because the true reward support is [0, 1].
    return clamp_unit_interval(R::rnorm(post_mean, post_sd));
  }

  double mean_n = 0.5;
  double kappa_n = 1.0;
  double shape_n = 2.5;
  double scale_n = 0.125;
  normal_inverse_gamma_posterior(stats, prior, mean_n, kappa_n, shape_n, scale_n);

  if (posterior_model == "normal_inverse_gamma") {
    // Draw unknown variance first from its inverse-gamma posterior via the
    // reciprocal Gamma parameterization used by R.
    const double gamma_draw = R::rgamma(shape_n, 1.0 / scale_n);
    const double sigma2 = 1.0 / gamma_draw;
    // Conditional on variance, the mean is Gaussian.
    return clamp_unit_interval(R::rnorm(mean_n, std::sqrt(sigma2 / kappa_n)));
  }

  if (posterior_model == "student_t_marginal") {
    // Marginalizing out the variance yields a Student-t draw for the mean.
    const double df = 2.0 * shape_n;
    const double scale = std::sqrt(scale_n / (shape_n * kappa_n));
    return clamp_unit_interval(mean_n + scale * R::rt(df));
  }

  Rcpp::stop("Unsupported scalar posterior model.");
}

PosteriorSummaryRow summarize_scalar_model(
    const ActionStats& stats,
    const std::string& posterior_model,
    const Rcpp::List& prior) {
  PosteriorSummaryRow row;

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
    // Same approximate posterior mean used by the draw path above.
    row.estimate = ((weight0 * mean0) + (stats.allocation_count * empirical_mean)) / total_weight;
    // Same approximate posterior SD of the mean used for summaries/plots.
    row.posterior_sd = std::sqrt(empirical_var / total_weight);
    return row;
  }

  if (posterior_model == "normal_inverse_gamma" || posterior_model == "student_t_marginal") {
    double mean_n = 0.5;
    double kappa_n = 1.0;
    double shape_n = 2.5;
    double scale_n = 0.125;
    normal_inverse_gamma_posterior(stats, prior, mean_n, kappa_n, shape_n, scale_n);
    // Posterior mean of the unknown payoff mean.
    row.estimate = mean_n;
    // Finite only when the posterior variance of the mean exists.
    row.posterior_sd = shape_n > 1.0
      ? std::sqrt(scale_n / ((shape_n - 1.0) * kappa_n))
      : NA_REAL;
    return row;
  }

  Rcpp::stop("Unsupported scalar posterior model.");
}

}  // namespace posterior
}  // namespace backgammonr

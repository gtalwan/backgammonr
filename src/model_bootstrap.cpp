// Bootstrap posterior kernel.
//
// Reward variable:
// - whichever reward model the caller requested.
//
// Sufficient statistics:
// - same sufficient stats as the corresponding empirical reward model.
//
// Status:
// - nonparametric robustness check, not an exact conjugate posterior.

#include "posterior_core.h"

#include <numeric>

namespace backgammonr {
namespace posterior {

double sample_bootstrap_value(
    const ActionStats& stats,
    const std::string& reward_model,
    const double unresolved_value,
    const Rcpp::List& prior) {
  const double smoothing = list_number_or_default(prior, "smoothing", 0.0);

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

}  // namespace posterior
}  // namespace backgammonr

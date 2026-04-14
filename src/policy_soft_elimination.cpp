// Soft-elimination Thompson helper for the explicit-posterior engine.
//
// ---------------------------------------------------------------------------
// Statistical idea
// ---------------------------------------------------------------------------
// This selector does not choose the next action directly. Instead, it updates
// the "active set" of actions that remain eligible for future Thompson draws.
//
// The screening rule is intentionally conservative:
// - do not screen anything until every currently active action has received at
//   least `min_allocations` rollouts;
// - compute an approximate 95% interval for each active action using
//   posterior mean +/- 1.96 * posterior SD;
// - protect the `keep_top` highest-posterior-mean actions unconditionally; and
// - screen out any unprotected action whose optimistic bound is still below
//   the current leader's conservative lower bound, optionally with a positive
//   extra margin.
//
// After this mask update, ordinary Thompson sampling continues on the
// surviving actions. In other words, this file implements the "cautious
// pruning" part of soft-elimination TS; the TS part lives elsewhere.

#include "posterior_policy.h"

namespace backgammonr {
namespace posterior_policy {

Rcpp::LogicalVector update_posterior_active_set(
    const Rcpp::LogicalVector& active,
    const Rcpp::NumericVector& posterior_mean,
    const Rcpp::NumericVector& posterior_sd,
    const Rcpp::NumericVector& allocation_count,
    const int min_allocations,
    const int keep_top,
    const double margin) {
  // Every vector is defined over the same action coordinate system.
  if (active.size() != posterior_mean.size() ||
      active.size() != posterior_sd.size() ||
      active.size() != allocation_count.size()) {
    Rcpp::stop("Soft-elimination TS requires all vectors to have the same length.");
  }
  if (min_allocations < 1) {
    Rcpp::stop("`min_allocations` must be at least 1.");
  }
  if (keep_top < 1) {
    Rcpp::stop("`keep_top` must be at least 1.");
  }
  if (!std::isfinite(margin) || margin < 0.0) {
    Rcpp::stop("`margin` must be a nonnegative finite scalar.");
  }

  Rcpp::LogicalVector out = Rcpp::clone(active);
  std::vector<int> active_idx;
  active_idx.reserve(active.size());
  for (R_xlen_t i = 0; i < active.size(); ++i) {
    if (Rcpp::as<bool>(active[i])) {
      // Store zero-based indices of the currently active actions so later
      // loops only work over candidates that are still alive.
      active_idx.push_back(static_cast<int>(i));
    }
  }

  if (static_cast<int>(active_idx.size()) <= keep_top) {
    // If the active set is already no larger than the protected set, there is
    // nothing left to screen.
    return out;
  }
  for (const int idx : active_idx) {
    if (allocation_count[idx] < min_allocations) {
      // Soft elimination waits until every surviving action has had a minimum
      // amount of exposure. This avoids screening on extremely thin evidence.
      return out;
    }
  }

  Rcpp::NumericVector lower_95(active.size());
  Rcpp::NumericVector upper_95(active.size());
  for (R_xlen_t i = 0; i < active.size(); ++i) {
    // The rollout rewards are bounded in the research stacks used here, so the
    // interval is clamped to [0, 1] after the normal-approximation expansion.
    //
    // This is a pragmatic screening interval, not a formal coverage guarantee.
    lower_95[i] = std::max(posterior_mean[i] - 1.96 * posterior_sd[i], 0.0);
    upper_95[i] = std::min(posterior_mean[i] + 1.96 * posterior_sd[i], 1.0);
  }

  // The leader threshold is the strongest lower bound among the active set.
  // An action whose upper bound still falls below this threshold cannot
  // plausibly outrank the current leader under this approximation.
  double leader_lower = -std::numeric_limits<double>::infinity();
  for (const int idx : active_idx) {
    if (lower_95[idx] > leader_lower) {
      leader_lower = lower_95[idx];
    }
  }

  std::vector<int> sorted_active = active_idx;
  std::stable_sort(
      sorted_active.begin(),
      sorted_active.end(),
      [&](const int lhs, const int rhs) {
        return posterior_mean[lhs] > posterior_mean[rhs];
      });

  std::vector<bool> keep(active.size(), false);
  for (int i = 0; i < std::min(keep_top, static_cast<int>(sorted_active.size())); ++i) {
    // Always protect a small leader set. This prevents the method from
    // accidentally collapsing to a single arm too early when the top region is
    // still genuinely uncertain.
    keep[sorted_active[static_cast<std::size_t>(i)]] = true;
  }

  for (const int idx : active_idx) {
    if (keep[idx]) {
      continue;
    }
    // Eliminate only when even the action's optimistic bound plus any extra
    // user-supplied safety margin still cannot reach the leader's lower bound.
    if (upper_95[idx] + margin < leader_lower) {
      out[idx] = false;
    }
  }

  return out;
}

}  // namespace posterior_policy
}  // namespace backgammonr

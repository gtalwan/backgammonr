// Forced-exploration Thompson selector for the explicit-posterior engine.
//
// ---------------------------------------------------------------------------
// Statistical idea
// ---------------------------------------------------------------------------
// Forced-exploration TS is mostly ordinary Thompson sampling with a safety
// override:
//
// - if some action has not yet received the minimum required number of
//   rollouts, sample the least-observed action;
// - otherwise, every `forced_every`th step, again sample the least-observed
//   action;
// - on all other steps, let ordinary Thompson sampling choose.
//
// This selector therefore answers only the override question:
//   "Should we force exploration right now, and if so which action gets it?"
//
// If the answer is "no", the function returns `NA_INTEGER` and the caller
// falls back to canonical Thompson selection.

#include "posterior_policy.h"

namespace backgammonr {
namespace posterior_policy {

int choose_posterior_forced_candidate(
    const Rcpp::NumericVector& allocation_count,
    const int spent,
    const int forced_every,
    const int forced_min_allocations) {
  if (allocation_count.size() < 1) {
    Rcpp::stop("Forced-exploration TS cannot choose from an empty candidate set.");
  }
  if (forced_every < 1) {
    Rcpp::stop("`forced_every` must be at least 1.");
  }
  if (forced_min_allocations < 1) {
    Rcpp::stop("`forced_min_allocations` must be at least 1.");
  }

  bool needs_minimum_pass = false;
  for (R_xlen_t i = 0; i < allocation_count.size(); ++i) {
    if (allocation_count[i] < forced_min_allocations) {
      // As long as some action has not reached the minimum exposure target, we
      // stay in the mandatory exploration phase.
      needs_minimum_pass = true;
      break;
    }
  }

  if (!needs_minimum_pass &&
      ((spent + 1) % std::max(1, forced_every) != 0)) {
    // No minimum-exposure problem and not on a scheduled forced step: let the
    // caller proceed with ordinary Thompson sampling instead.
    return NA_INTEGER;
  }

  // Forced steps allocate to the least-sampled action. This is the simplest
  // possible exploration target and directly addresses neglect risk.
  int best_idx = 0;
  double best_count = allocation_count[0];
  for (R_xlen_t i = 1; i < allocation_count.size(); ++i) {
    if (allocation_count[i] < best_count) {
      best_idx = static_cast<int>(i);
      best_count = allocation_count[i];
    }
  }

  // Return one-based action position for the R-side engine.
  return best_idx + 1;
}

}  // namespace posterior_policy
}  // namespace backgammonr

// Equal-allocation policy.
//
// Statistical role:
// Equal allocation is the neutral finite-budget baseline. It does not try to
// infer which moves are promising; it simply spreads rollouts as evenly as
// possible across the collapsed candidate set.
//
// Why this file is tiny:
// The shared engine in alloc_core.cpp owns rollout execution, summary updates,
// and checkpoint diagnostics. Equal allocation therefore only needs to answer
// one question:
//   "Given n candidates and the current step index, which candidate gets the
//    next rollout?"
//
// Answer:
// Round-robin through the candidate positions.

#include "alloc_core.h"
#include "posterior_policy.h"

namespace backgammonr {
namespace allocation {

int choose_equal_candidate(const int n_candidates, const int step) {
  // Deterministic round-robin keeps the baseline reproducible and makes the
  // policy interpretation obvious in plots: every candidate should stay near
  // the same allocation count unless the run ends mid-cycle.
  return n_candidates == 0 ? -1 : (step % n_candidates);
}

}  // namespace allocation

namespace posterior_policy {

int choose_posterior_equal_candidate(
    const Rcpp::IntegerVector& active_idx,
    const int spent) {
  // `active_idx` already stores one-based action positions from the R engine.
  // Equal allocation simply rotates through them in order.
  if (active_idx.size() < 1) {
    Rcpp::stop("Equal allocation cannot choose from an empty active set.");
  }

  // Return the next active action in the round-robin cycle. Because the stored
  // values are already one-based, no further index conversion is needed here.
  return active_idx[(spent % active_idx.size())];
}

}  // namespace posterior_policy
}  // namespace backgammonr

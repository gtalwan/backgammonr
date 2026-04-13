// Equal-allocation policy.
//
// This is the neutral fixed-budget baseline: cycle through the collapsed
// candidate set without using any posterior information.

#include "alloc_core.h"

namespace backgammonr {
namespace allocation {

int choose_equal_candidate(const int n_candidates, const int step) {
  // Round-robin allocation keeps the baseline deterministic given the step.
  return n_candidates == 0 ? -1 : (step % n_candidates);
}

}  // namespace allocation
}  // namespace backgammonr

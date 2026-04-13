// Greedy posterior-mean allocation policy.
//
// This policy is intentionally myopic: always sample the arm with the largest
// current posterior mean. It is useful as a comparator because it highlights
// how badly pure exploitation can behave in fixed-budget identification.

#include "alloc_core.h"

#include <limits>

namespace backgammonr {
namespace allocation {

int choose_greedy_candidate(const std::vector<ActionEvaluationSummary>& summaries) {
  const int n = static_cast<int>(summaries.size());
  if (n == 0) {
    throw std::range_error("Cannot choose from an empty candidate set.");
  }

  int best_index = 0;
  double best_score = -std::numeric_limits<double>::infinity();
  int best_allocations = std::numeric_limits<int>::max();

  for (int i = 0; i < n; ++i) {
    const ActionEvaluationSummary& summary = summaries[i];
    // In the scalar engine the Beta posterior mean is alpha / (alpha + beta).
    const double score = summary.alpha / (summary.alpha + summary.beta);
    if (score_beats_incumbent(score, summary.allocation_count, best_score, best_allocations)) {
      best_index = i;
      best_score = score;
      best_allocations = summary.allocation_count;
    }
  }

  return best_index;
}

}  // namespace allocation
}  // namespace backgammonr

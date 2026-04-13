// UCB allocation policy.
//
// This is a simple fixed-budget comparator, not a theoretically tuned
// best-arm-identification implementation. It uses the scalar engine's Beta
// posterior mean plus an exploration bonus.

#include "alloc_core.h"

#include <cmath>
#include <limits>

namespace backgammonr {
namespace allocation {

int choose_ucb_candidate(
    const std::vector<ActionEvaluationSummary>& summaries,
    const int step,
    const RolloutConfig& config) {
  const int n = static_cast<int>(summaries.size());
  if (n == 0) {
    throw std::range_error("Cannot choose from an empty candidate set.");
  }

  int best_index = 0;
  double best_score = -std::numeric_limits<double>::infinity();
  int best_allocations = std::numeric_limits<int>::max();
  const double ucb_log_term = std::log(static_cast<double>(step) + 2.0);

  for (int i = 0; i < n; ++i) {
    const ActionEvaluationSummary& summary = summaries[i];
    const double posterior_mean = summary.alpha / (summary.alpha + summary.beta);
    const double denom = static_cast<double>(std::max(summary.allocation_count, 1));
    // `ucb_exploration` is the only exploration knob exposed by the scalar
    // engine for this comparator family.
    const double score = posterior_mean +
        config.ucb_exploration * std::sqrt(ucb_log_term / denom);

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

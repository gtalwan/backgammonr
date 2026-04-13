// Thompson-sampling allocation policy.
//
// Canonical one-draw Thompson sampling for the scalar/Beta engine. Every
// action receives one posterior draw and the arm with the largest draw gets
// the next rollout.

#include "alloc_core.h"

#include <limits>

namespace backgammonr {
namespace allocation {

int choose_thompson_candidate(
    const std::vector<ActionEvaluationSummary>& summaries,
    std::mt19937& rng) {
  // `summaries` already contains one posterior state per collapsed action.
  const int n = static_cast<int>(summaries.size());
  if (n == 0) {
    throw std::range_error("Cannot choose from an empty candidate set.");
  }

  // Track the current Thompson winner in local variables so we only keep the
  // best candidate seen so far.
  int best_index = 0;
  // Start below any possible draw so the first draw always initializes the incumbent.
  double best_score = -std::numeric_limits<double>::infinity();
  // Tie-break toward the least-sampled action when the sampled scores match.
  int best_allocations = std::numeric_limits<int>::max();

  for (int i = 0; i < n; ++i) {
    const ActionEvaluationSummary& summary = summaries[i];
    // The scalar engine's Thompson policy is Beta-only; richer posterior
    // families route through the explicit-posterior R workflow instead.
    const double score = sample_beta_distribution(summary.alpha, summary.beta, rng);
    // Use the shared tie-break helper so Thompson, UCB, and greedy all break
    // sampled-score ties the same way.
    if (score_beats_incumbent(score, summary.allocation_count, best_score, best_allocations)) {
      // This action becomes the current Thompson winner.
      best_index = i;
      // Keep the winning sampled score for later comparisons.
      best_score = score;
      // Keep the allocation count for stable tie-breaking.
      best_allocations = summary.allocation_count;
    }
  }

  // Return the position inside the collapsed-action vector.
  return best_index;
}

}  // namespace allocation
}  // namespace backgammonr

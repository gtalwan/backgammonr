// Top-two Thompson-sampling allocation policy.
//
// TTTS reuses Thompson winner draws but spends extra effort on the plausible
// challenger set near the current leader. In this scalar engine the
// `ucb_exploration` field is reused as the TTTS "keep the first winner"
// probability.

#include "alloc_core.h"

#include <cmath>
#include <limits>

namespace backgammonr {
namespace allocation {

int choose_ttts_candidate(
    const std::vector<ActionEvaluationSummary>& summaries,
    const RolloutConfig& config,
    std::mt19937& rng) {
  // TTTS works over the same collapsed-action summaries as canonical TS.
  const int n = static_cast<int>(summaries.size());
  if (n == 0) {
    throw std::range_error("Cannot choose from an empty candidate set.");
  }

  auto draw_thompson_winner = [&](void) -> int {
    // One helper draw performs the ordinary Thompson tournament over all arms.
    int winner = 0;
    double best = -std::numeric_limits<double>::infinity();
    for (int i = 0; i < n; ++i) {
      // Sample one Beta posterior value for this arm.
      const double draw = sample_beta_distribution(summaries[i].alpha, summaries[i].beta, rng);
      // Keep the largest sampled value as the current Thompson winner.
      if (draw > best) {
        best = draw;
        winner = i;
      }
    }
    return winner;
  };

  const int top1 = draw_thompson_winner();
  if (n == 1) {
    // Degenerate one-arm case: TTTS collapses to that single arm.
    return top1;
  }

  double beta = config.ucb_exploration;
  if (!(beta > 0.0 && beta <= 1.0) || !std::isfinite(beta)) {
    // Default TTTS coin bias when the caller did not supply a clean value.
    beta = 0.5;
  }

  std::uniform_real_distribution<double> coin(0.0, 1.0);
  if (coin(rng) <= beta) {
    // With probability beta, TTTS keeps the first Thompson winner.
    return top1;
  }

  for (int attempt = 0; attempt < 64; ++attempt) {
    // Re-draw until a challenger different from the original winner appears.
    const int top2 = draw_thompson_winner();
    if (top2 != top1) {
      // Otherwise it repeatedly redraws until a distinct challenger appears.
      return top2;
    }
  }

  // The redraw loop can fail when the posterior is already extremely
  // concentrated. Fall back to the best posterior mean among the remaining
  // actions so the policy still compares the leader to a plausible challenger.
  int fallback = -1;
  double best_mean = -std::numeric_limits<double>::infinity();
  for (int i = 0; i < n; ++i) {
    if (i == top1) {
      // Skip the already chosen leader when building the challenger fallback.
      continue;
    }
    // Posterior mean provides a stable deterministic challenger ranking.
    const double mean = summaries[i].alpha / (summaries[i].alpha + summaries[i].beta);
    if (mean > best_mean) {
      best_mean = mean;
      fallback = i;
    }
  }
  // If every fallback path fails, return the original Thompson winner.
  return fallback >= 0 ? fallback : top1;
}

}  // namespace allocation
}  // namespace backgammonr

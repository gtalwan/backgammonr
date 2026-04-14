// Top-two Thompson-sampling allocation policy.
//
// Purpose:
// - implement the scalar-engine version of top-two Thompson sampling (TTTS);
// - keep the selection logic isolated from the shared rollout/update loop in
//   alloc_core.cpp; and
// - make the difference from canonical TS explicit: TTTS deliberately revisits
//   the "best versus challenger" comparison rather than always accepting the
//   first Thompson winner.
//
// Statistical meaning:
// - sample one Thompson winner over all arms;
// - with probability beta, keep that winner;
// - otherwise, resample until a distinct challenger appears, so the next
//   rollout is concentrated on an arm that still looks plausibly competitive.
//
// Implementation note:
// In this scalar engine the `ucb_exploration` config field is repurposed as the
// TTTS coin bias. That keeps the native config object small while preserving a
// stable public front door from R.

#include "alloc_core.h"
#include "posterior_policy.h"

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

namespace posterior_policy {

int choose_posterior_ttts_candidate(
    const Rcpp::NumericMatrix& draw_mat,
    const Rcpp::NumericVector& allocation_count,
    double ttts_beta) {
  // In the explicit-posterior path, each row is one complete posterior world.
  // TTTS repeatedly samples such worlds until it has identified:
  // - an initial Thompson winner; and
  // - if needed, a distinct challenger.
  if (draw_mat.ncol() < 1) {
    Rcpp::stop("Top-two TS cannot choose from an empty candidate set.");
  }
  if (draw_mat.ncol() != allocation_count.size()) {
    Rcpp::stop("Top-two TS requires draw and count vectors to align.");
  }

  auto winner_once = [&](void) -> int {
    // Sample one posterior world uniformly from the available rows, then run
    // the shared score-based selector on that row.
    return posterior_pick_index(
      matrix_row(draw_mat, sampled_row_index(draw_mat.nrow())),
      allocation_count);
  };

  const int top1 = winner_once();
  if (draw_mat.ncol() == 1) {
    return top1;
  }

  if (!(ttts_beta > 0.0 && ttts_beta <= 1.0) || !std::isfinite(ttts_beta)) {
    ttts_beta = 0.5;
  }
  if (R::runif(0.0, 1.0) <= ttts_beta) {
    // With probability beta, keep the first Thompson winner exactly as in the
    // textbook TTTS coin flip.
    return top1;
  }

  for (int attempt = 0; attempt < 64; ++attempt) {
    // Otherwise, keep sampling posterior worlds until a distinct challenger
    // emerges. The cap prevents an accidental infinite loop when the posterior
    // is already highly concentrated on one action.
    const int top2 = winner_once();
    if (top2 != top1) {
      return top2;
    }
  }

  // If no distinct challenger appeared, build a deterministic fallback by
  // taking the highest column mean among the remaining actions. This gives the
  // method a stable "runner-up" notion when random redraws are no longer
  // producing one.
  Rcpp::NumericVector posterior_mean = column_means(draw_mat);
  posterior_mean[top1 - 1] = -std::numeric_limits<double>::infinity();
  return posterior_pick_index(
    posterior_mean,
    allocation_count,
    posterior_mean);
}

}  // namespace posterior_policy
}  // namespace backgammonr

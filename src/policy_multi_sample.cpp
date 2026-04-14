// Multi-sample Thompson-sampling selector for the explicit-posterior engine.
//
// ---------------------------------------------------------------------------
// Statistical idea
// ---------------------------------------------------------------------------
// Canonical Thompson sampling chooses the next action from one posterior draw
// per action. Multi-sample TS instead asks for multiple posterior draws per
// action, averages them action-wise, and chooses the action with the largest
// average sampled value.
//
// In symbols, if column `a` of the draw matrix contains:
//   theta_a^(1), ..., theta_a^(M)
// then the selector uses
//   score_a = mean(theta_a^(1), ..., theta_a^(M))
// and allocates to the action with the largest `score_a`.
//
// Intuition:
// - canonical TS follows one sampled posterior world;
// - multi-sample TS follows a small average over several sampled worlds;
// - this smooths out especially lucky or unlucky one-draw shocks.
//
// The shared R-side engine decides how many draws `M` to request. This file
// only converts the resulting draw matrix into one chosen action index.

#include "posterior_policy.h"

namespace backgammonr {
namespace posterior_policy {

int choose_posterior_multi_sample_candidate(
    const Rcpp::NumericMatrix& draw_mat,
    const Rcpp::NumericVector& allocation_count,
    const Rcpp::NumericVector& posterior_mean) {
  // All inputs are column-aligned:
  // - each column of `draw_mat` is one active action;
  // - `allocation_count[col]` is that action's sample count;
  // - `posterior_mean[col]` is that action's deterministic tie-break score.
  if (draw_mat.ncol() < 1) {
    Rcpp::stop("Multi-sample TS cannot choose from an empty candidate set.");
  }
  if (draw_mat.ncol() != allocation_count.size() ||
      draw_mat.ncol() != posterior_mean.size()) {
    Rcpp::stop("Multi-sample TS requires draw, count, and mean vectors to align.");
  }

  // Compute the multi-sample score for each action by averaging down the draw
  // rows. The shared tie-break helper then:
  // - prefers the largest score;
  // - breaks score ties by least allocation count; and
  // - finally falls back to posterior mean if needed.
  return posterior_pick_index(column_means(draw_mat), allocation_count, posterior_mean);
}

}  // namespace posterior_policy
}  // namespace backgammonr

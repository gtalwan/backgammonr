// Thompson-sampling allocation policy.
//
// Purpose:
// - implement the canonical one-draw Thompson selection rule for the fast
//   scalar/Beta allocation engine;
// - keep the policy logic separate from rollout execution, trace capture, and
//   summary updates, all of which live in alloc_core.cpp; and
// - provide the baseline selection rule against which TTTS and the other
//   Thompson-family variants are interpreted.
//
// Statistical meaning:
// - each candidate arm carries a Beta posterior over its scalar reward mean;
// - one posterior draw is taken per arm;
// - the arm with the largest draw gets the next rollout.
//
// Routing note:
// This file is only used by the fast scalar/Beta engine. Richer posterior
// families such as Student-t and Dirichlet route through the explicit
// posterior R path and therefore never call this selector directly.

#include "alloc_core.h"
#include "posterior_policy.h"

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

namespace posterior_policy {

int choose_posterior_thompson_candidate(
    const Rcpp::NumericMatrix& draw_mat,
    const Rcpp::NumericVector& allocation_count,
    const Rcpp::NumericVector& posterior_mean) {
  // Explicit-posterior canonical TS uses the first row of `draw_mat` as one
  // jointly sampled posterior world over the active actions. The surrounding
  // R engine is responsible for ensuring that this matrix contains at least
  // one draw row and that the first row is the one intended for selection.
  if (draw_mat.ncol() < 1) {
    Rcpp::stop("Canonical TS cannot choose from an empty candidate set.");
  }
  if (draw_mat.ncol() != allocation_count.size() ||
      draw_mat.ncol() != posterior_mean.size()) {
    Rcpp::stop("Canonical TS requires draw, count, and mean vectors to align.");
  }

  // Feed that sampled world into the shared selector. The helper chooses the
  // largest sampled value, breaks ties toward the least-sampled action, and
  // then falls back to posterior mean if a deterministic final tie-break is
  // still needed.
  return posterior_pick_index(matrix_row(draw_mat, 0), allocation_count, posterior_mean);
}

}  // namespace posterior_policy
}  // namespace backgammonr

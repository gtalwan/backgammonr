// Top-k-focused Thompson selector for the explicit-posterior engine.
//
// ---------------------------------------------------------------------------
// Statistical idea
// ---------------------------------------------------------------------------
// This method is designed to refine the current contender shortlist rather
// than to behave like pure best-arm Thompson sampling.
//
// Step 1:
//   Rank actions by posterior mean and keep only the current top-k actions.
//
// Step 2:
//   Within that focus set, estimate pairwise ordering uncertainty using the
//   posterior draw matrix. For actions i and j, let
//     p_ij = P(i > j | current posterior draws).
//   The uncertainty of that pair is summarized as
//     p_ij * (1 - p_ij),
//   which is largest when p_ij is near 0.5 and smallest when the ordering is
//   already nearly certain.
//
// Step 3:
//   Allocate to the action involved in the greatest total unresolved pairwise
//   ordering uncertainty inside the current top-k set.
//
// This makes the policy a shortlist-refinement method: it spends budget where
// the internal ordering of the current contenders is still unclear.

#include "posterior_policy.h"

namespace backgammonr {
namespace posterior_policy {

int choose_posterior_top_k_candidate(
    const Rcpp::NumericMatrix& draw_mat,
    const Rcpp::NumericVector& posterior_mean,
    const Rcpp::NumericVector& allocation_count,
    const int focus_top_k) {
  if (draw_mat.ncol() < 1) {
    Rcpp::stop("Top-k TS cannot choose from an empty candidate set.");
  }
  if (draw_mat.ncol() != posterior_mean.size() ||
      draw_mat.ncol() != allocation_count.size()) {
    Rcpp::stop("Top-k TS requires draw, mean, and count vectors to align.");
  }
  if (focus_top_k < 1) {
    Rcpp::stop("`focus_top_k` must be at least 1.");
  }
  if (draw_mat.ncol() <= 1) {
    return 1;
  }

  // Build the current posterior-mean ranking and retain only the focus set.
  // The rest of the actions are temporarily ignored for this selection step.
  std::vector<int> order(draw_mat.ncol());
  std::iota(order.begin(), order.end(), 0);
  std::stable_sort(
      order.begin(),
      order.end(),
      [&](const int lhs, const int rhs) {
        return posterior_mean[lhs] > posterior_mean[rhs];
      });
  order.resize(static_cast<std::size_t>(std::min(focus_top_k, static_cast<int>(order.size()))));

  if (order.size() <= 1U) {
    return order.front() + 1;
  }

  Rcpp::NumericVector uncertainty(draw_mat.ncol());
  for (const int i : order) {
    // For a fixed action i, accumulate how unresolved its pairwise comparisons
    // are against every other action in the focused top-k set.
    double pair_uncertainty_sum = 0.0;
    for (const int j : order) {
      if (i == j) {
        continue;
      }
      double wins_ij = 0.0;
      for (int row = 0; row < draw_mat.nrow(); ++row) {
        // Each posterior draw row is one joint plausible world. Count the
        // fraction of those worlds in which action i outranks action j.
        if (draw_mat(row, i) > draw_mat(row, j)) {
          wins_ij += 1.0;
        }
      }
      const double p_ij = wins_ij / static_cast<double>(draw_mat.nrow());
      // Bernoulli variance p * (1 - p) is used here as a compact uncertainty
      // score: maximal at 0.5, minimal near 0 or 1.
      pair_uncertainty_sum += p_ij * (1.0 - p_ij);
    }
    uncertainty[i] = pair_uncertainty_sum;
  }

  // Use the common selector so pairwise-uncertainty ties are resolved the same
  // way as in the other policies: least-sampled action first, then posterior
  // mean if still tied.
  return posterior_pick_index(
    uncertainty,
    allocation_count,
    posterior_mean);
}

}  // namespace posterior_policy
}  // namespace backgammonr

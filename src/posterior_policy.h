#ifndef BACKGAMMONR_POSTERIOR_POLICY_H
#define BACKGAMMONR_POSTERIOR_POLICY_H

// Explicit-posterior policy helpers.
//
// ---------------------------------------------------------------------------
// Role in the package
// ---------------------------------------------------------------------------
// These helpers are used by the explicit-posterior research engine that lives
// in R/bg_policy_engine.R. By the time control reaches the helpers below, the
// package has already:
//
// 1. identified the currently active actions;
// 2. constructed posterior summaries for those actions; and
// 3. sampled a posterior draw matrix whose columns correspond to actions and
//    whose rows correspond to repeated posterior worlds.
//
// The helpers here therefore do not own rollout execution, statistics updates,
// or checkpoint bookkeeping. They only answer the method-specific decision:
//
//   "Given the current posterior information, which action should receive the
//    next rollout?"
//
// ---------------------------------------------------------------------------
// Why these helpers exist
// ---------------------------------------------------------------------------
// The package exposes several Thompson-family variants whose differences are
// purely in the selection rule. Keeping those rules in separate native files
// makes the package easier to read without duplicating the shared R-side
// sequential engine.
//
// ---------------------------------------------------------------------------
// Indexing convention
// ---------------------------------------------------------------------------
// Internal loops in C++ usually work with zero-based indices, but the selector
// functions exported back to R return one-based action positions whenever they
// are choosing from vectors/matrices aligned with R objects. Comments below
// call this out explicitly because off-by-one confusion is easy here.

#include <Rcpp.h>

#include <algorithm>
#include <cmath>
#include <limits>
#include <numeric>
#include <vector>

namespace backgammonr {
namespace posterior_policy {

// Small tolerance used when comparing floating-point scores. The selectors
// operate on sampled or averaged posterior values, so exact equality is rare,
// but small numerical noise should not create arbitrary winner changes.
inline constexpr double kPosteriorTieTolerance = 1e-12;

inline void validate_selector_inputs(
    const R_xlen_t n_scores,
    const R_xlen_t n_allocations,
    const char* context) {
  // Every selector assumes one allocation count per score-bearing action.
  // Failing fast here makes downstream policy bugs easier to diagnose.
  if (n_scores < 1) {
    Rcpp::stop("%s cannot choose from an empty candidate set.", context);
  }
  if (n_scores != n_allocations) {
    Rcpp::stop("%s requires `scores` and `allocation_count` to have the same length.", context);
  }
}

inline int posterior_pick_index(
    const Rcpp::NumericVector& scores,
    const Rcpp::NumericVector& allocation_count,
    const Rcpp::Nullable<Rcpp::NumericVector>& tie_break = R_NilValue) {
  // Shared winner-selection helper used by several explicit-posterior policy
  // files.
  //
  // Selection logic:
  // 1. find the largest finite score;
  // 2. collect all actions tied (within tolerance) at that best score;
  // 3. among those ties, prefer the least-sampled action;
  // 4. if a caller provided `tie_break`, use it only after score and
  //    allocation ties remain.
  //
  // The return value is one-based so it can be used directly as an R action
  // index.
  validate_selector_inputs(scores.size(), allocation_count.size(), "posterior_pick_index");

  double max_score = -std::numeric_limits<double>::infinity();
  for (R_xlen_t i = 0; i < scores.size(); ++i) {
    const double score = scores[i];
    // Skip missing or non-finite scores instead of letting them contaminate
    // the winner calculation. Policy callers should not emit such values, so
    // ending up with no finite score is treated as an error below.
    if (Rcpp::NumericVector::is_na(score) || !std::isfinite(score)) {
      continue;
    }
    if (score > max_score) {
      max_score = score;
    }
  }

  if (!std::isfinite(max_score)) {
    Rcpp::stop("posterior_pick_index received no finite scores.");
  }

  std::vector<int> best;
  best.reserve(static_cast<std::size_t>(scores.size()));
  for (R_xlen_t i = 0; i < scores.size(); ++i) {
    const double score = scores[i];
    if (Rcpp::NumericVector::is_na(score) || !std::isfinite(score)) {
      continue;
    }
    if (score >= max_score - kPosteriorTieTolerance) {
      best.push_back(static_cast<int>(i));
    }
  }

  if (best.empty()) {
    Rcpp::stop("posterior_pick_index failed to identify any candidate.");
  }
  if (best.size() == 1U) {
    // Convert zero-based C++ storage index to the one-based index expected by
    // the surrounding R engine.
    return best.front() + 1;
  }

  // First tie-break: least allocated action. This favors under-sampled actions
  // when the posterior score itself does not clearly separate candidates.
  double min_allocation = std::numeric_limits<double>::infinity();
  std::vector<int> least_allocated;
  least_allocated.reserve(best.size());
  for (const int idx : best) {
    const double count = allocation_count[idx];
    if (count < min_allocation - kPosteriorTieTolerance) {
      min_allocation = count;
      least_allocated.clear();
      least_allocated.push_back(idx);
    } else if (std::fabs(count - min_allocation) <= kPosteriorTieTolerance) {
      least_allocated.push_back(idx);
    }
  }

  if (least_allocated.size() == 1U || tie_break.isNull()) {
    return least_allocated.front() + 1;
  }

  // Second tie-break: caller-supplied deterministic score, typically posterior
  // mean. This keeps method behavior stable when both sampled score and
  // allocation count fail to separate candidates.
  const Rcpp::NumericVector tie = tie_break.get();
  if (tie.size() != scores.size()) {
    Rcpp::stop("`tie_break` must have the same length as `scores`.");
  }

  int best_idx = least_allocated.front();
  double best_tie = tie[best_idx];
  for (const int idx : least_allocated) {
    if (tie[idx] > best_tie) {
      best_tie = tie[idx];
      best_idx = idx;
    }
  }

  return best_idx + 1;
}

inline int sampled_row_index(const int n_rows) {
  // Sample one posterior world uniformly from the rows of a draw matrix.
  //
  // The explicit-posterior engine interprets each row as a complete jointly
  // sampled set of action values. Choosing a row uniformly is therefore the
  // native analog of "draw one Thompson sample for all active actions".
  if (n_rows <= 0) {
    Rcpp::stop("Cannot sample from an empty posterior draw matrix.");
  }
  const double u = R::runif(0.0, 1.0);
  const int idx = static_cast<int>(std::floor(u * static_cast<double>(n_rows)));
  return std::min(std::max(idx, 0), n_rows - 1);
}

inline Rcpp::NumericVector matrix_row(const Rcpp::NumericMatrix& draw_mat, const int row) {
  // Copy a matrix row into a vector so policy helpers can reuse the common
  // score-based tie-breaking logic in posterior_pick_index().
  Rcpp::NumericVector out(draw_mat.ncol());
  for (int col = 0; col < draw_mat.ncol(); ++col) {
    out[col] = draw_mat(row, col);
  }
  return out;
}

inline Rcpp::NumericVector column_means(const Rcpp::NumericMatrix& draw_mat) {
  // Average posterior draws column-wise.
  //
  // Multi-sample TS uses these column means as its per-action score, which is
  // equivalent to averaging several posterior worlds before choosing.
  Rcpp::NumericVector out(draw_mat.ncol());
  if (draw_mat.nrow() <= 0) {
    return out;
  }
  for (int col = 0; col < draw_mat.ncol(); ++col) {
    double acc = 0.0;
    for (int row = 0; row < draw_mat.nrow(); ++row) {
      acc += draw_mat(row, col);
    }
    out[col] = acc / static_cast<double>(draw_mat.nrow());
  }
  return out;
}

int choose_posterior_equal_candidate(
    const Rcpp::IntegerVector& active_idx,
    int spent);

int choose_posterior_thompson_candidate(
    const Rcpp::NumericMatrix& draw_mat,
    const Rcpp::NumericVector& allocation_count,
    const Rcpp::NumericVector& posterior_mean);

int choose_posterior_ttts_candidate(
    const Rcpp::NumericMatrix& draw_mat,
    const Rcpp::NumericVector& allocation_count,
    double ttts_beta);

int choose_posterior_multi_sample_candidate(
    const Rcpp::NumericMatrix& draw_mat,
    const Rcpp::NumericVector& allocation_count,
    const Rcpp::NumericVector& posterior_mean);

int choose_posterior_forced_candidate(
    const Rcpp::NumericVector& allocation_count,
    int spent,
    int forced_every,
    int forced_min_allocations);

Rcpp::LogicalVector update_posterior_active_set(
    const Rcpp::LogicalVector& active,
    const Rcpp::NumericVector& posterior_mean,
    const Rcpp::NumericVector& posterior_sd,
    const Rcpp::NumericVector& allocation_count,
    int min_allocations,
    int keep_top,
    double margin);

int choose_posterior_top_k_candidate(
    const Rcpp::NumericMatrix& draw_mat,
    const Rcpp::NumericVector& posterior_mean,
    const Rcpp::NumericVector& allocation_count,
    int focus_top_k);

}  // namespace posterior_policy
}  // namespace backgammonr

#endif

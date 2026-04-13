// Shared posterior helpers for explicit posterior models.
//
// The explicit-posterior R workflow keeps most routing logic in R, but it
// delegates three things to C++:
// - materializing one canonical `ActionStats` vector from rollout tallies;
// - low-level numeric helpers that every posterior family reuses; and
// - small summary utilities used by posterior diagnostics and checkpoint-path
//   metrics.

#include "posterior_core.h"

#include <algorithm>
#include <cmath>
#include <limits>
#include <numeric>
#include <vector>

namespace backgammonr {
namespace posterior {

// ---------------------------------------------------------------------------
// Numeric guardrails shared by every posterior family
// ---------------------------------------------------------------------------

double clamp_unit_interval(const double x) {
  if (!R_finite(x)) {
    return x;
  }
  if (x < 0.0) {
    return 0.0;
  }
  if (x > 1.0) {
    return 1.0;
  }
  return x;
}

double safe_sample_variance(const ActionStats& stats) {
  if (stats.allocation_count <= 1) {
    return NA_REAL;
  }

  // Convert the stored first and second raw moments into the unbiased sample
  // variance used by Gaussian / Student-t style approximations.
  const double n = static_cast<double>(stats.allocation_count);
  const double raw = (stats.reward_sum_sq - ((stats.reward_sum * stats.reward_sum) / n)) / (n - 1.0);
  return std::max(0.0, raw);
}

double safe_empirical_mean(const ActionStats& stats, const double fallback) {
  if (stats.allocation_count <= 0) {
    return fallback;
  }
  return stats.reward_sum / static_cast<double>(stats.allocation_count);
}

double list_number_or_default(const Rcpp::List& x, const char* name, const double fallback) {
  if (!x.containsElementNamed(name)) {
    return fallback;
  }
  const Rcpp::NumericVector value = x[name];
  if (value.size() < 1 || !R_finite(value[0])) {
    return fallback;
  }
  return value[0];
}

Rcpp::NumericVector list_numeric_or_default(
    const Rcpp::List& x,
    const char* name,
    const Rcpp::NumericVector& fallback) {
  if (!x.containsElementNamed(name)) {
    return fallback;
  }
  const Rcpp::NumericVector value = x[name];
  if (value.size() == 0) {
    return fallback;
  }
  return value;
}

// ---------------------------------------------------------------------------
// R-to-C++ materialization of action-level sufficient statistics
// ---------------------------------------------------------------------------

std::vector<ActionStats> materialize_stats(
    const Rcpp::IntegerVector& allocation_count,
    const Rcpp::IntegerVector& wins,
    const Rcpp::IntegerVector& losses,
    const Rcpp::IntegerVector& single_loss,
    const Rcpp::IntegerVector& gammon_loss,
    const Rcpp::IntegerVector& backgammon_loss,
    const Rcpp::IntegerVector& unresolved,
    const Rcpp::IntegerVector& single_win,
    const Rcpp::IntegerVector& gammon_win,
    const Rcpp::IntegerVector& backgammon_win,
    const Rcpp::NumericVector& reward_sum,
    const Rcpp::NumericVector& reward_sum_sq) {
  const int n_actions = allocation_count.size();
  if (wins.size() != n_actions ||
      losses.size() != n_actions ||
      single_loss.size() != n_actions ||
      gammon_loss.size() != n_actions ||
      backgammon_loss.size() != n_actions ||
      unresolved.size() != n_actions ||
      single_win.size() != n_actions ||
      gammon_win.size() != n_actions ||
      backgammon_win.size() != n_actions ||
      reward_sum.size() != n_actions ||
      reward_sum_sq.size() != n_actions) {
    Rcpp::stop("Posterior-kernel inputs must all have the same length.");
  }

  std::vector<ActionStats> out(static_cast<std::size_t>(n_actions));
  for (int i = 0; i < n_actions; ++i) {
    out[static_cast<std::size_t>(i)] = ActionStats{
      allocation_count[i],
      wins[i],
      losses[i],
      single_loss[i],
      gammon_loss[i],
      backgammon_loss[i],
      unresolved[i],
      single_win[i],
      gammon_win[i],
      backgammon_win[i],
      reward_sum[i],
      reward_sum_sq[i]
    };
  }
  return out;
}

// ---------------------------------------------------------------------------
// Small shared helpers for path-wise diagnostics
// ---------------------------------------------------------------------------

std::vector<int> ordered_index(const Rcpp::IntegerVector& x) {
  std::vector<int> ord(static_cast<std::size_t>(x.size()));
  std::iota(ord.begin(), ord.end(), 0);
  std::stable_sort(
    ord.begin(),
    ord.end(),
    [&](const int lhs, const int rhs) {
      return x[lhs] < x[rhs];
    }
  );
  return ord;
}

double trapezoid_area_sorted(
    const Rcpp::IntegerVector& x,
    const Rcpp::NumericVector& y,
    const std::vector<int>& ord) {
  if (ord.empty()) {
    return NA_REAL;
  }

  double area = 0.0;
  int prev_idx = -1;
  for (int pos = 0; pos < static_cast<int>(ord.size()); ++pos) {
    const int idx = ord[static_cast<std::size_t>(pos)];
    if (!R_finite(y[idx])) {
      continue;
    }
    if (prev_idx >= 0) {
      const double dx = static_cast<double>(x[idx] - x[prev_idx]);
      if (dx > 0.0) {
        area += dx * (y[idx] + y[prev_idx]) / 2.0;
      }
    }
    prev_idx = idx;
  }

  if (prev_idx < 0) {
    return NA_REAL;
  }
  if (area == 0.0) {
    // Single-checkpoint paths collapse to the one observed value.
    return y[prev_idx];
  }
  return area;
}

}  // namespace posterior
}  // namespace backgammonr

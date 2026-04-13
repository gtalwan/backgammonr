// OCBA-inspired allocation policy.
//
// This file keeps the OCBA-style target-allocation rule separate from the
// rollout loop. The implementation is pragmatic rather than a full
// ranking-and-selection framework: it uses current Beta summaries to compute a
// next-step target profile and then samples the arm with the largest deficit.

#include <RcppArmadillo.h>

#include "alloc_core.h"

#include <cmath>

namespace {

arma::vec ocba_target_allocations(
    const std::vector<backgammonr::ActionEvaluationSummary>& summaries,
    const int next_total_allocations) {
  const int n = static_cast<int>(summaries.size());
  arma::vec target(n, arma::fill::zeros);
  if (n == 0) {
    return target;
  }
  if (n == 1) {
    target[0] = static_cast<double>(next_total_allocations);
    return target;
  }

  arma::vec mu(n);
  arma::vec sigma(n);
  for (int i = 0; i < n; ++i) {
    const double alpha = summaries[i].alpha;
    const double beta = summaries[i].beta;
    mu[i] = alpha / (alpha + beta);
    const double var = (alpha * beta) /
        ((alpha + beta) * (alpha + beta) * (alpha + beta + 1.0));
    sigma[i] = std::sqrt(std::max(var, 1e-12));
  }

  int best = 0;
  for (int i = 1; i < n; ++i) {
    if (mu[i] > mu[best]) {
      best = i;
    }
  }

  arma::vec ratio(n, arma::fill::zeros);
  const double mu_best = mu[best];
  for (int i = 0; i < n; ++i) {
    if (i == best) {
      continue;
    }
    // Small gaps imply large target allocations because these are the actions
    // whose ranking relative to the incumbent remains hardest to resolve.
    const double gap = std::max(mu_best - mu[i], 1e-6);
    ratio[i] = (sigma[i] * sigma[i]) / (gap * gap);
  }

  double sum_term = 0.0;
  for (int i = 0; i < n; ++i) {
    if (i == best) {
      continue;
    }
    sum_term += (ratio[i] * ratio[i]) / std::max(sigma[i] * sigma[i], 1e-12);
  }
  ratio[best] = std::max(sigma[best] * std::sqrt(std::max(sum_term, 1e-12)), 1e-12);

  for (int i = 0; i < n; ++i) {
    ratio[i] = std::max(ratio[i], 1e-12);
  }

  const double ratio_sum = arma::accu(ratio);
  if (ratio_sum <= 0.0 || !std::isfinite(ratio_sum)) {
    target.fill(static_cast<double>(next_total_allocations) / static_cast<double>(n));
    return target;
  }

  target = ratio / ratio_sum * static_cast<double>(next_total_allocations);
  return target;
}

}  // namespace

namespace backgammonr {
namespace allocation {

int choose_ocba_candidate(
    const std::vector<ActionEvaluationSummary>& summaries,
    const int step) {
  const int n = static_cast<int>(summaries.size());
  if (n == 0) {
    throw std::range_error("Cannot choose from an empty candidate set.");
  }

  const arma::vec target = ocba_target_allocations(summaries, step + 1);
  int chosen = 0;
  double best_deficit = target[0] - static_cast<double>(summaries[0].allocation_count);
  for (int i = 1; i < n; ++i) {
    const double deficit = target[i] - static_cast<double>(summaries[i].allocation_count);
    if (deficit > best_deficit + kTieTolerance) {
      chosen = i;
      best_deficit = deficit;
      continue;
    }
    if (std::fabs(deficit - best_deficit) <= kTieTolerance &&
        summaries[i].allocation_count < summaries[chosen].allocation_count) {
      chosen = i;
      best_deficit = deficit;
    }
  }
  return chosen;
}

}  // namespace allocation
}  // namespace backgammonr

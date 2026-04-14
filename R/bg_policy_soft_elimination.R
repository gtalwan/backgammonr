# Soft-elimination Thompson sampling.
#
# Reading guide:
# - Shared explicit posterior loop: R/bg_policy_engine.R
# - Explicit-posterior selector in C++: src/policy_soft_elimination.cpp via
#   bg_cpp_posterior_update_active_set()
# - Posterior draw generation: src/rcpp_entrypoints.cpp and model_*.cpp
#
# This method adds one idea on top of canonical TS: maintain an active set and
# cautiously retire actions whose upper interval looks clearly worse than the
# current leader's lower interval, while keeping a protected top set alive.

# Elimination only screens once every action has at least a minimum amount of
# evidence, and it always protects a small leader set from removal.
bg_posterior_update_active_set <- function(
    active,
    posterior_mean,
    posterior_sd,
    allocation_count,
    min_allocations = 4L,
    keep_top = 2L,
    margin = 0) {
  if (bg_has_native_call("_backgammonr_bg_cpp_posterior_update_active_set")) {
    return(bg_cpp_posterior_update_active_set(
      active = active,
      posterior_mean = posterior_mean,
      posterior_sd = posterior_sd,
      allocation_count = allocation_count,
      min_allocations = min_allocations,
      keep_top = keep_top,
      margin = margin
    ))
  }

  min_allocations <- bg_coerce_integerish(min_allocations, "min_allocations", 1L)
  keep_top <- bg_coerce_integerish(keep_top, "keep_top", 1L)
  if (!is.numeric(margin) || length(margin) != 1L || is.na(margin) || margin < 0) {
    stop("`margin` must be a nonnegative numeric scalar.", call. = FALSE)
  }

  active_idx <- which(active)
  if (length(active_idx) <= keep_top || any(allocation_count[active_idx] < min_allocations)) {
    return(active)
  }

  lower_95 <- pmax(posterior_mean - 1.96 * posterior_sd, 0)
  upper_95 <- pmin(posterior_mean + 1.96 * posterior_sd, 1)
  leader_lower <- max(lower_95[active_idx], na.rm = TRUE)
  keep_idx <- active_idx[order(posterior_mean[active_idx], decreasing = TRUE)][seq_len(min(keep_top, length(active_idx)))]
  eliminated <- setdiff(active_idx[upper_95[active_idx] + margin < leader_lower], keep_idx)
  if (length(eliminated) > 0L) {
    active[eliminated] <- FALSE
  }
  active
}

#' Run soft-elimination Thompson sampling on one problem
#'
#' `bg_soft_elimination_ts_run()` uses posterior intervals to retire clearly
#' dominated arms while keeping a protected top set active.
#'
#' @param problem A `bg_problem` object.
#' @param budget Integer-like simulation budget.
#' @param proxy_reference Optional `bg_reference` object used for regret and
#'   ranking diagnostics.
#' @param checkpoints Optional integer checkpoint vector.
#' @param seed Optional integer-like seed.
#' @param elimination_min_allocations Minimum allocations before elimination can
#'   start.
#' @param elimination_keep_top Number of currently best-looking actions that are
#'   always kept active.
#' @param elimination_margin Additional elimination slack on the interval
#'   comparison scale.
#' @param ... Passed through to the underlying Thompson engine.
#'
#' @return A `bg_ts_run` object.
#' @export
bg_soft_elimination_ts_run <- function(
    problem,
    budget = 256L,
    proxy_reference = NULL,
    checkpoints = NULL,
    seed = NULL,
    elimination_min_allocations = 4L,
    elimination_keep_top = 2L,
    elimination_margin = 0,
    ...) {
  bg_ts_decide(
    problem = problem,
    budget = budget,
    allocation_policy = "soft_elimination_thompson",
    proxy_reference = proxy_reference,
    checkpoints = checkpoints,
    seed = seed,
    elimination_min_allocations = elimination_min_allocations,
    elimination_keep_top = elimination_keep_top,
    elimination_margin = elimination_margin,
    ...
  )
}

# Top-k-focused Thompson sampling.
#
# Reading guide:
# - Shared explicit posterior loop: R/bg_policy_engine.R
# - Explicit-posterior selector in C++: src/policy_top_k.cpp via
#   bg_cpp_posterior_top_k_choice()
# - Posterior draw generation: src/rcpp_entrypoints.cpp and model_*.cpp
#
# This method is easiest to read as a ranking-focused policy. It first narrows
# attention to the current top-k actions by posterior mean, then allocates to
# the action that participates in the most pairwise uncertainty inside that
# focus set.

# Ranking-aware TS focuses on uncertainty inside the near-top set instead of
# only the current leader.
bg_posterior_ranking_choice <- function(draw_mat, posterior_mean, allocation_count, focus_top_k = 3L) {
  if (bg_has_native_call("_backgammonr_bg_cpp_posterior_top_k_choice")) {
    return(bg_cpp_posterior_top_k_choice(
      draw_mat = draw_mat,
      posterior_mean = posterior_mean,
      allocation_count = allocation_count,
      focus_top_k = focus_top_k
    ))
  }

  focus_top_k <- bg_coerce_integerish(focus_top_k, "focus_top_k", 1L)
  if (ncol(draw_mat) <= 1L) {
    return(1L)
  }

  focus_local <- order(posterior_mean, decreasing = TRUE)[seq_len(min(focus_top_k, length(posterior_mean)))]
  if (length(focus_local) <= 1L) {
    return(focus_local[[1L]])
  }

  uncertainty <- numeric(length(posterior_mean))
  for (i in focus_local) {
    pair_uncertainty <- numeric(0L)
    for (j in focus_local) {
      if (identical(i, j)) {
        next
      }
      p_ij <- mean(draw_mat[, i] > draw_mat[, j])
      pair_uncertainty <- c(pair_uncertainty, p_ij * (1 - p_ij))
    }
    uncertainty[[i]] <- sum(pair_uncertainty)
  }

  bg_posterior_pick_index(
    scores = uncertainty,
    allocation_count = allocation_count,
    tie_break = posterior_mean
  )
}

#' Run top-k-focused Thompson sampling on one problem
#'
#' `bg_top_k_ts_run()` biases exploration toward the currently strongest-looking
#' subset of actions rather than the full action set.
#'
#' @param problem A `bg_problem` object.
#' @param budget Integer-like simulation budget.
#' @param proxy_reference Optional `bg_reference` object used for regret and
#'   ranking diagnostics.
#' @param checkpoints Optional integer checkpoint vector.
#' @param seed Optional integer-like seed.
#' @param top_k Number of currently strongest-looking actions to prioritize.
#' @param ranking_draws Number of posterior draws used to stabilize the top-k
#'   selection step.
#' @param ... Passed through to the underlying Thompson engine.
#'
#' @return A `bg_ts_run` object.
#' @export
bg_top_k_ts_run <- function(
    problem,
    budget = 256L,
    proxy_reference = NULL,
    checkpoints = NULL,
    seed = NULL,
    top_k = 3L,
    ranking_draws = 128L,
    ...) {
  bg_ts_decide(
    problem = problem,
    budget = budget,
    allocation_policy = "top_k_thompson",
    proxy_reference = proxy_reference,
    checkpoints = checkpoints,
    seed = seed,
    ranking_top_k = top_k,
    ranking_draws = ranking_draws,
    ...
  )
}

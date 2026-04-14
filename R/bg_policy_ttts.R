# Top-two Thompson sampling.
#
# Reading guide:
# - Fast native scalar/Beta selector: src/policy_ttts.cpp
# - Shared fast native allocation loop: src/alloc_core.cpp
# - Explicit-posterior selector in C++: src/policy_ttts.cpp via
#   bg_cpp_posterior_top_two_choice()
# - Shared explicit posterior loop for richer stacks: R/bg_policy_engine.R
#
# This file keeps the TTTS-specific logic separate because TTTS is easiest to
# understand as "canonical TS plus a deliberate winner-vs-challenger step".

# TTTS is implemented as repeated posterior-winner draws until a distinct
# challenger appears, with a deterministic fallback if repeated draws tie.
bg_posterior_top_two_choice <- function(draw_mat, allocation_count, ttts_beta = 0.5) {
  if (bg_has_native_call("_backgammonr_bg_cpp_posterior_top_two_choice")) {
    return(bg_cpp_posterior_top_two_choice(
      draw_mat = draw_mat,
      allocation_count = allocation_count,
      ttts_beta = ttts_beta
    ))
  }

  winner_once <- function() {
    bg_posterior_pick_index(
      scores = draw_mat[sample.int(nrow(draw_mat), 1L), ],
      allocation_count = allocation_count
    )
  }

  top1 <- winner_once()
  if (ncol(draw_mat) == 1L) {
    return(top1)
  }

  if (!is.numeric(ttts_beta) || length(ttts_beta) != 1L || is.na(ttts_beta) || ttts_beta <= 0 || ttts_beta > 1) {
    ttts_beta <- 0.5
  }
  if (stats::runif(1L) <= ttts_beta) {
    return(top1)
  }

  for (attempt in seq_len(64L)) {
    top2 <- winner_once()
    if (!identical(top2, top1)) {
      return(top2)
    }
  }

  posterior_mean <- colMeans(draw_mat)
  posterior_mean[top1] <- -Inf
  bg_posterior_pick_index(
    scores = posterior_mean,
    allocation_count = allocation_count,
    tie_break = posterior_mean
  )
}

#' Run top-two Thompson sampling on one problem
#'
#' `bg_ttts_run()` is the front door for top-two Thompson sampling on one
#' `bg_problem`.
#'
#' @param problem A `bg_problem` object.
#' @param budget Integer-like simulation budget.
#' @param proxy_reference Optional `bg_reference` object used for regret and
#'   ranking diagnostics.
#' @param checkpoints Optional integer checkpoint vector.
#' @param seed Optional integer-like seed.
#' @param ttts_beta Probability of resampling the current Thompson winner.
#' @param ... Passed through to the underlying Thompson engine.
#'
#' @return A `bg_ts_run` object.
#' @export
bg_ttts_run <- function(
    problem,
    budget = 256L,
    proxy_reference = NULL,
    checkpoints = NULL,
    seed = NULL,
    ttts_beta = 0.5,
    ...) {
  bg_ts_decide(
    problem = problem,
    budget = budget,
    allocation_policy = "top_two_thompson",
    proxy_reference = proxy_reference,
    checkpoints = checkpoints,
    seed = seed,
    ttts_beta = ttts_beta,
    ...
  )
}

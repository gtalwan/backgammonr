# Multi-sample Thompson sampling.
#
# Reading guide:
# - Shared explicit posterior loop: R/bg_policy_engine.R
# - Explicit-posterior selector in C++: src/policy_multi_sample.cpp via
#   bg_cpp_posterior_multi_sample_choice()
# - Posterior draw generation: src/rcpp_entrypoints.cpp and model_*.cpp
#
# This method differs from canonical TS only in the selection rule: instead of
# trusting one posterior draw per action, it averages several draws before
# choosing the next rollout target.

# Explicit-posterior multi-sample selection rule.
#
# The algorithm takes several posterior draws per active action, averages those
# draws action-wise, and then allocates to the action with the largest averaged
# sampled value.
bg_posterior_multi_sample_choice <- function(draw_mat, allocation_count, posterior_mean) {
  if (bg_has_native_call("_backgammonr_bg_cpp_posterior_multi_sample_choice")) {
    return(bg_cpp_posterior_multi_sample_choice(
      draw_mat = draw_mat,
      allocation_count = allocation_count,
      posterior_mean = posterior_mean
    ))
  }

  bg_posterior_pick_index(
    scores = colMeans(draw_mat),
    allocation_count = allocation_count,
    tie_break = posterior_mean
  )
}

#' Run multi-sample Thompson sampling on one problem
#'
#' `bg_multi_sample_ts_run()` averages several posterior draws per action
#' before selecting the next arm. This is an explicitly experimental TS-family
#' variant, but it is supported as part of the cleaned method set.
#'
#' @param problem A `bg_problem` object.
#' @param budget Integer-like simulation budget.
#' @param proxy_reference Optional `bg_reference` object used for regret and
#'   ranking diagnostics.
#' @param checkpoints Optional integer checkpoint vector.
#' @param seed Optional integer-like seed.
#' @param multi_sample_draws Number of posterior draws averaged per action at
#'   each selection step.
#' @param ... Passed through to the underlying Thompson engine.
#'
#' @return A `bg_ts_run` object.
#' @export
bg_multi_sample_ts_run <- function(
    problem,
    budget = 256L,
    proxy_reference = NULL,
    checkpoints = NULL,
    seed = NULL,
    multi_sample_draws = 5L,
    ...) {
  bg_ts_decide(
    problem = problem,
    budget = budget,
    allocation_policy = "multi_sample_thompson",
    proxy_reference = proxy_reference,
    checkpoints = checkpoints,
    seed = seed,
    multi_sample_draws = multi_sample_draws,
    ...
  )
}

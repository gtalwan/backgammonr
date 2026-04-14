# Canonical Thompson sampling.
#
# Reading guide:
# - Fast native scalar/Beta selector: src/policy_ts.cpp
# - Shared fast native allocation loop: src/alloc_core.cpp
# - Explicit-posterior selector in C++: src/policy_ts.cpp via
#   bg_cpp_posterior_thompson_choice()
# - Shared explicit posterior loop for richer stacks: R/bg_policy_engine.R

# Explicit-posterior canonical TS selection rule.
#
# For richer posterior stacks we still need a pure-R policy function that says:
# "take one posterior draw per active action and choose the largest draw, using
# posterior mean as the deterministic tie-break."
bg_posterior_thompson_choice <- function(draw_mat, allocation_count, posterior_mean) {
  if (bg_has_native_call("_backgammonr_bg_cpp_posterior_thompson_choice")) {
    return(bg_cpp_posterior_thompson_choice(
      draw_mat = draw_mat,
      allocation_count = allocation_count,
      posterior_mean = posterior_mean
    ))
  }

  bg_posterior_pick_index(
    scores = draw_mat[1L, ],
    allocation_count = allocation_count,
    tie_break = posterior_mean
  )
}

#' Run canonical Thompson sampling on one problem
#'
#' `bg_ts_run()` is the main front door for canonical sequential Thompson
#' sampling on one `bg_problem`.
#'
#' @param problem A `bg_problem` object.
#' @param budget Integer-like simulation budget.
#' @param proxy_reference Optional `bg_reference` object used for regret and
#'   ranking diagnostics.
#' @param checkpoints Optional integer checkpoint vector.
#' @param seed Optional integer-like seed.
#' @param ... Passed through to the underlying Thompson engine.
#'
#' @return A `bg_ts_run` object.
#' @export
bg_ts_run <- function(
    problem,
    budget = 256L,
    proxy_reference = NULL,
    checkpoints = NULL,
    seed = NULL,
    ...) {
  bg_ts_decide(
    problem = problem,
    budget = budget,
    allocation_policy = "thompson",
    proxy_reference = proxy_reference,
    checkpoints = checkpoints,
    seed = seed,
    ...
  )
}

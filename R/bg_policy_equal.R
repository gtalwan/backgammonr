# Equal-allocation baseline.
#
# Reading guide:
# - Fast native scalar/Beta selector: src/policy_equal.cpp
# - Shared fast native allocation loop: src/alloc_core.cpp
# - Explicit-posterior selector in C++: src/policy_equal.cpp via
#   bg_cpp_posterior_equal_choice()
# - Shared explicit posterior loop for richer stacks: R/bg_policy_engine.R

# Explicit-posterior equal-allocation schedule.
#
# On richer stacks the package still uses a deterministic round-robin schedule
# for the equal baseline. This helper keeps that logic beside the public equal
# front door instead of burying it in the shared engine.
bg_posterior_equal_choice <- function(active_idx, spent) {
  if (bg_has_native_call("_backgammonr_bg_cpp_posterior_equal_choice")) {
    return(bg_cpp_posterior_equal_choice(
      active_idx = active_idx,
      spent = spent
    ))
  }

  active_idx[[((spent %% length(active_idx)) + 1L)]]
}

#' Run equal-allocation baseline on one problem
#'
#' `bg_equal_run()` is the main non-TS baseline kept in the live package
#' surface. On the legacy default stack it uses the historical scalar rollout
#' engine; on richer reward/posterior stacks it uses the explicit posterior
#' engine with a deterministic round-robin allocation rule.
#'
#' @param problem A `bg_problem` object.
#' @param budget Integer-like simulation budget.
#' @param checkpoints Optional checkpoint vector.
#' @param proxy_reference Optional `bg_reference` object.
#' @param ... Passed to the internal rollout runner.
#'
#' @return A `bg_ts_run` object with `allocation_policy = "equal"`.
#' @export
bg_equal_run <- function(
    problem,
    budget = 256L,
    checkpoints = NULL,
    proxy_reference = NULL,
    seed = NULL,
    ...) {
  if (bg_problem_supports_legacy_scalar_engine(problem)) {
    return(bg_run_method_path(
      problem = problem,
      allocation_policy = "equal",
      budget = budget,
      checkpoints = checkpoints,
      reference = proxy_reference,
      seed = seed,
      ...
    ))
  }

  checkpoints <- bg_checkpoint_grid(budget, checkpoints)
  bg_run_posterior_ts(
    problem = problem,
    allocation_policy = "equal",
    budget = budget,
    checkpoints = checkpoints,
    reference = proxy_reference,
    seed = seed,
    ...
  )
}

#' Deprecated alias for [bg_equal_run()]
#'
#' `bg_uniform_run()` is retained for backward compatibility only. New code
#' should call [bg_equal_run()] directly.
#'
#' @inheritParams bg_equal_run
#' @return A `bg_ts_run` object with `allocation_policy = "equal"`.
#' @export
bg_uniform_run <- function(
    problem,
    budget = 256L,
    checkpoints = NULL,
    proxy_reference = NULL,
    seed = NULL,
    ...) {
  .Deprecated("bg_equal_run")
  bg_equal_run(
    problem = problem,
    budget = budget,
    checkpoints = checkpoints,
    proxy_reference = proxy_reference,
    seed = seed,
    ...
  )
}

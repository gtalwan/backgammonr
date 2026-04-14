# Forced-exploration Thompson sampling.
#
# Reading guide:
# - Shared explicit posterior loop: R/bg_policy_engine.R
# - Explicit-posterior selector in C++: src/policy_forced_exploration.cpp via
#   bg_cpp_posterior_forced_choice()
# - Posterior draw generation: src/rcpp_entrypoints.cpp and model_*.cpp
#
# This method keeps canonical TS as the default behavior, but adds scheduled or
# minimum-allocation exploration steps so no action is ignored too early.

# Forced-exploration override for the explicit-posterior engine.
#
# Return an index inside the local active set when the method wants to force a
# least-sampled exploration step. Return `NA_integer_` when the caller should
# fall back to ordinary Thompson selection.
bg_posterior_forced_choice <- function(
    allocation_count,
    spent,
    forced_every = 8L,
    forced_min_allocations = 2L) {
  if (bg_has_native_call("_backgammonr_bg_cpp_posterior_forced_choice")) {
    return(bg_cpp_posterior_forced_choice(
      allocation_count = allocation_count,
      spent = spent,
      forced_every = forced_every,
      forced_min_allocations = forced_min_allocations
    ))
  }

  forced_every <- bg_coerce_integerish(forced_every, "forced_every", 1L)
  forced_min_allocations <- bg_coerce_integerish(forced_min_allocations, "forced_min_allocations", 1L)

  if (any(allocation_count < forced_min_allocations) ||
      ((spent + 1L) %% max(1L, forced_every) == 0L)) {
    return(which.min(allocation_count))
  }

  NA_integer_
}

#' Run forced-exploration Thompson sampling on one problem
#'
#' `bg_forced_exploration_ts_run()` interleaves Thompson decisions with regular
#' least-sampled exploration steps.
#'
#' @param problem A `bg_problem` object.
#' @param budget Integer-like simulation budget.
#' @param proxy_reference Optional `bg_reference` object used for regret and
#'   ranking diagnostics.
#' @param checkpoints Optional integer checkpoint vector.
#' @param seed Optional integer-like seed.
#' @param forced_every Force one exploration step every `forced_every`
#'   allocations.
#' @param forced_min_allocations Ensure every action reaches at least this many
#'   allocations before pure Thompson steps dominate.
#' @param ... Passed through to the underlying Thompson engine.
#'
#' @return A `bg_ts_run` object.
#' @export
bg_forced_exploration_ts_run <- function(
    problem,
    budget = 256L,
    proxy_reference = NULL,
    checkpoints = NULL,
    seed = NULL,
    forced_every = 8L,
    forced_min_allocations = 2L,
    ...) {
  bg_ts_decide(
    problem = problem,
    budget = budget,
    allocation_policy = "forced_exploration_thompson",
    proxy_reference = proxy_reference,
    checkpoints = checkpoints,
    seed = seed,
    forced_every = forced_every,
    forced_min_allocations = forced_min_allocations,
    ...
  )
}

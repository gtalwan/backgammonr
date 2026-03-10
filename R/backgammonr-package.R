#' backgammonr: Finite-budget rollout allocation for backgammon
#'
#' `backgammonr` is a research-oriented statistical computing package for
#' finite-budget rollout allocation under simulation noise, using backgammon as
#' a stochastic testbed.
#'
#' The package centers on this decision problem:
#'
#' - for one decision instance `(state, realized_roll)`, legal actions
#'   `A_d = {1, ..., K}`, and rollout rewards in `[0, 1]`, can Thompson
#'   sampling allocate a fixed simulation budget `N` efficiently enough to
#'   identify the rollout-model best action?
#'
#' In package terms, a *rollout* is one Monte Carlo continuation from a chosen
#' action. Repeated rollouts estimate action values under a specified rollout
#' policy and turn the move-choice problem into a ranking-and-selection task.
#'
#' The package combines:
#'
#' - a C++ backgammon engine for legal move generation and state transitions;
#' - Thompson-family allocation (`thompson`, `ttts`) as the conceptual center;
#' - baseline allocation methods (`equal`, `greedy`, `ucb`, `ocba`) for
#'   controlled comparison;
#' - reference-estimation, study, benchmark, and reporting tools for proxy PCS,
#'   simple regret, value-estimation error, and runtime tradeoffs.
#'
#' @section Main workflow:
#' A typical user workflow is:
#'
#' - construct a board and realized roll;
#' - enumerate legal actions;
#' - evaluate the action set under a finite budget;
#' - build a higher-budget reference estimate;
#' - compare the finite-budget recommendation to the reference;
#' - summarize stability across budgets or benchmark cases.
#'
#' @section Central exported functions:
#' Important user-facing entry points include:
#'
#' - `evaluate_actions_thompson()` and `evaluate_actions_ttts()`;
#' - `approximate_action_reference()` and `certify_reference_truth()`;
#' - `compare_thompson_to_reference()`;
#' - `study_budget_tradeoff()` and `study_variance_controls()`;
#' - `benchmark_thompson()` and `summarize_thompson_benchmark()`.
#'
#' Core loops use `Rcpp`/`RcppArmadillo` with batched C++ simulation calls for
#' high-throughput research workflows.
#'
#' @keywords internal
#' @useDynLib backgammonr, .registration = TRUE
#' @importFrom Rcpp evalCpp
"_PACKAGE"

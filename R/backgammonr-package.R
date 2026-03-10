#' backgammonr: Finite-budget rollout allocation with Thompson sampling
#'
#' `backgammonr` is a statistical computing package for studying finite-budget
#' rollout allocation under simulation noise, using backgammon as a stochastic
#' testbed.
#'
#' The package centers on this decision problem:
#'
#' - for one decision instance `(state, realized_roll)`, legal actions
#'   `A_d = {1, ..., K}`, and rollout rewards in `[0,1]`, can Thompson sampling
#'   efficiently allocate a fixed budget `N` to identify the rollout-model best
#'   action?
#'
#' In package terms, a *rollout* is one Monte Carlo continuation from a chosen
#' action. Repeated rollouts estimate action values under a specified rollout
#' model.
#'
#' The package combines:
#'
#' - a C++ backgammon engine for legal move generation and state transitions;
#' - Thompson-family allocation (`thompson`, `ttts`) as central methods;
#' - baseline allocation methods (`equal`, `greedy`, `ucb`, `ocba`) for
#'   comparison;
#' - reference-estimation and benchmark tools for proxy PCS, simple regret, MSE,
#'   and runtime tradeoffs.
#'
#' Core loops use `Rcpp`/`RcppArmadillo` with batched C++ simulation calls for
#' high-throughput research workflows.
#'
#' @keywords internal
#' @useDynLib backgammonr, .registration = TRUE
#' @importFrom Rcpp evalCpp
"_PACKAGE"

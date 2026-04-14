# Package-level documentation and namespace setup for backgammonr.
#' backgammonr: Thompson sampling for finite-budget backgammon decisions
#'
#' `backgammonr` is a Thompson-sampling research toolkit for fixed-budget
#' best-action identification under Monte Carlo noise, with backgammon as the
#' stochastic laboratory rather than the primary product identity.
#'
#' The canonical package object is one local decision problem:
#'
#' - one board state;
#' - one realized dice roll;
#' - `K` legal root actions;
#' - rollout-model values `mu_i = E[Y_i | simulation_policy, truncation_rule,
#'   payoff_mapping]`;
#' - a fixed simulation budget `N`;
#' - an allocation policy that decides how to spend that budget.
#'
#' In package terms, a *rollout* is one simulated continuation after committing
#' to a root action. Sequential Thompson sampling is the canonical allocation
#' policy for this budgeted ranking-and-selection problem. Comparator methods
#' remain available, but primarily to help interpret Thompson behavior.
#'
#' @section Scientific distinctions:
#' The package explicitly distinguishes:
#'
#' - rollout-model value: the estimand defined by the rollout continuation
#'   policy, truncation rule, and payoff mapping;
#' - proxy reference: a high-budget Monte Carlo estimate used as a practical
#'   reference for regret and ranking diagnostics;
#' - true backgammon strength / game-theoretic truth: not available and never
#'   implied by the package outputs.
#'
#' Posterior quantities are therefore approximate and model-relative, not claims
#' of exact truth.
#'
#' The package now exposes an explicit reward/posterior model layer. The
#' default front door is the scalar-payoff baseline
#' `reward_model = "scalar_payoff"` with `posterior_model = "beta_pseudo"`,
#' while coherent alternatives such as `win_loss + beta_bernoulli` and
#' `categorical_outcome + dirichlet_multinomial` are available for direct
#' comparison.
#'
#' @section TS-first workflow:
#' Recommended entry points for the public interface are:
#'
#' - `bg_problem()` to define one state-plus-roll decision problem;
#' - `bg_reference()`, `bg_truth_state()`, and the `bg_opening_*()` helpers to
#'   build or reuse proxy references for the 21 opening rolls;
#' - `bg_study_save()` / `bg_study_load()` to keep expensive study objects
#'   reusable;
#' - `bg_ts_run()` and `bg_ttts_run()` for the main TS analysis loop;
#' - `bg_equal_run()` for the main non-TS baseline;
#' - `bg_multi_sample_ts_run()`, `bg_soft_elimination_ts_run()`,
#'   `bg_forced_exploration_ts_run()`, and `bg_top_k_ts_run()` for the
#'   supported TS-family variants;
#' - `bg_truth_certify()` to screen whether a proxy truth is separated enough
#'   to support strong comparative claims;
#' - `bg_sanity_lab()` to sanity-check TS and TTTS on small non-backgammon
#'   cases with known truth;
#' - `bg_compare_algorithms()` to compare Thompson, TTTS, and equal allocation,
#'   with legacy scalar comparators available only when requested;
#' - `bg_ts_diagnostics()`, `bg_eval_reference_aware()`, and
#'   `plot_bg_budget_curve()` to summarize finite-budget performance;
#' - `bg_compare_posteriors()` and `bg_compare_reward_models()` for the
#'   compact model-comparison workflows.
#'
#' Legacy helpers remain in the source tree for compatibility and internal
#' reuse, but they are no longer part of the package narrative. The intended
#' workflow runs through the curated `bg_*` interface.
#'
#' For repo-local orientation and real example outputs, start with the
#' numbered walkthrough scripts under `Presentation/`, especially
#' `Presentation/00_start_here.R`.
#'
#' Core loops use `Rcpp`, `RcppArmadillo`, and `RcppParallel` to keep canonical
#' sequential Thompson semantics intact while accelerating proxy-reference
#' generation and repeated-study workflows.
#'
#' @keywords internal
#' @useDynLib backgammonr, .registration = TRUE
#' @importFrom Rcpp evalCpp
"_PACKAGE"

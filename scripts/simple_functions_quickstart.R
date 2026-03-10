#!/usr/bin/env Rscript

# Simple Functions Quickstart
# ---------------------------
# Runs a compact Thompson-centered workflow with small, readable outputs.
#
# Usage:
#   Rscript scripts/simple_functions_quickstart.R
#   Rscript scripts/simple_functions_quickstart.R --quick

args <- commandArgs(trailingOnly = TRUE)
quick <- "--quick" %in% args

required_root <- c("DESCRIPTION", "R", "scripts")
missing_root <- required_root[!file.exists(required_root)]
if (length(missing_root) > 0L) {
  stop(
    "Run this script from package root. Missing path(s): ",
    paste(missing_root, collapse = ", "),
    call. = FALSE
  )
}

load_package <- function() {
  if (requireNamespace("pkgload", quietly = TRUE)) {
    loaded <- tryCatch(
      {
        pkgload::load_all(".", quiet = TRUE)
        TRUE
      },
      error = function(e) {
        message("note: pkgload::load_all failed (", conditionMessage(e), "); trying installed package.")
        FALSE
      }
    )
    if (isTRUE(loaded)) {
      return(invisible(TRUE))
    }
  }

  if (requireNamespace("backgammonr", quietly = TRUE)) {
    suppressPackageStartupMessages(library(backgammonr, character.only = TRUE))
    return(invisible(TRUE))
  }

  stop(
    "Could not load package via `pkgload::load_all()` or `library(backgammonr)`.",
    call. = FALSE
  )
}

show_cols <- function(df, cols) {
  df[, intersect(cols, names(df)), drop = FALSE]
}

load_package()

rollout_policy <- "random"
max_rollout_turns <- if (quick) 180L else 250L
finite_budget <- if (quick) 400L else 1200L
reference_budget <- if (quick) 2000L else 12000L
seed <- 11L

cat("\n=== Parameters ===\n")
print(data.frame(
  rollout_policy = rollout_policy,
  max_rollout_turns = max_rollout_turns,
  finite_budget = finite_budget,
  reference_budget = reference_budget,
  seed = seed,
  quick = quick,
  stringsAsFactors = FALSE
), row.names = FALSE)

board <- bg_initial_board()
roll <- bg_roll(1L, 6L)

cat("\n=== Board + Roll ===\n")
bg_print_board(board)
print(roll)

legal <- generate_legal_moves(board, roll)
legal_view <- summarize_legal_moves(legal, max_candidates = 8L, print_table = FALSE)

cat("\n=== Legal Moves (Compact) ===\n")
print(show_cols(legal_view, c("candidate_index", "move_label", "n_steps")), row.names = FALSE)

th <- evaluate_actions_thompson(
  board = board,
  roll = roll,
  total_budget = finite_budget,
  rollout_policy = rollout_policy,
  max_rollout_turns = max_rollout_turns,
  fast_diagnostics = FALSE,
  seed = seed
)

cat("\n=== Thompson Summary ===\n")
print(show_cols(
  summary(th),
  c(
    "method",
    "total_budget",
    "n_candidates",
    "recommended_move_label",
    "recommended_estimate",
    "recommended_prob_best",
    "recommended_expected_regret",
    "recommended_allocation_count",
    "runtime_seconds"
  )
), row.names = FALSE)

cat("\n=== Thompson Top Actions (Compact) ===\n")
print(show_cols(
  compare_action_posteriors(th, top_n = 6L, diagnostics = FALSE),
  c("move_label", "recommended", "allocation_count", "estimate", "uncertainty_sd", "prob_best", "exp_regret")
), row.names = FALSE)

ref <- approximate_action_reference(
  board = board,
  roll = roll,
  truth_budget = reference_budget,
  rollout_policy = rollout_policy,
  max_rollout_turns = max_rollout_turns,
  seed = seed
)

cert <- certify_reference_truth(reference = ref)

cat("\n=== Reference Certificate ===\n")
print(show_cols(
  cert$certificate,
  c(
    "reference_best_move_label",
    "reference_second_move_label",
    "top_two_gap_estimate",
    "top_two_gap_lower_95",
    "top_two_gap_upper_95",
    "certified",
    "difficulty_label"
  )
), row.names = FALSE)

cmp <- compare_thompson_to_reference(
  board = board,
  roll = roll,
  method = "thompson",
  total_budget = finite_budget,
  reference = ref,
  reference_certificate = cert,
  rollout_policy = rollout_policy,
  max_rollout_turns = max_rollout_turns,
  seed = seed + 1L
)

cat("\n=== Thompson vs Reference (Summary) ===\n")
print(show_cols(
  cmp$summary,
  c(
    "method",
    "total_budget",
    "reference_budget",
    "proxy_pcs",
    "simple_regret",
    "mse",
    "finite_runtime_seconds",
    "reference_runtime_seconds",
    "difficulty_label",
    "reference_certified"
  )
), row.names = FALSE)

cmp_actions <- cmp$action_table[order(-cmp$action_table$finite_allocation_count), , drop = FALSE]
cat("\n=== Thompson vs Reference (Top Actions by Allocation) ===\n")
print(show_cols(
  utils::head(cmp_actions, 6L),
  c(
    "move_label",
    "finite_recommended",
    "reference_best",
    "finite_allocation_count",
    "finite_estimate",
    "reference_estimate",
    "abs_error"
  )
), row.names = FALSE)

cat("\nDone. Compact quickstart completed.\n")

#!/usr/bin/env Rscript

# Vignette Example Runner
# -----------------------
# Executes a compact, readable subset of the package workflow and writes
# artifacts under `vignettes/_example_outputs/`.
#
# Design goal:
# - avoid giant raw dumps,
# - favor one-row summaries and small readable tables,
# - mirror the new vignette sequence.
#
# Usage:
#   Rscript scripts/run_vignette_examples.R
#   Rscript scripts/run_vignette_examples.R --quick

args <- commandArgs(trailingOnly = TRUE)
quick <- "--quick" %in% args

required_root <- c("DESCRIPTION", "R", "vignettes", "scripts")
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
    "Could not load package via `pkgload::load_all()` or `library(backgammonr)`.\n",
    "Install the package or run in an environment with build tools.",
    call. = FALSE
  )
}

load_package()

out_dir <- file.path("vignettes", "_example_outputs")
if (dir.exists(out_dir)) {
  unlink(out_dir, recursive = TRUE, force = TRUE)
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

write_txt <- function(file_name, expr) {
  path <- file.path(out_dir, file_name)
  out <- capture.output(expr)
  writeLines(out, path, useBytes = TRUE)
  message("wrote: ", path)
}

write_csv <- function(file_name, df) {
  path <- file.path(out_dir, file_name)
  utils::write.csv(df, path, row.names = FALSE)
  message("wrote: ", path)
}

save_plot <- function(file_name, expr, width = 1200, height = 780, res = 130) {
  path <- file.path(out_dir, file_name)
  grDevices::png(path, width = width, height = height, res = res)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(expr)
  grDevices::dev.off()
  on.exit(NULL, add = FALSE)
  message("wrote: ", path)
}

show_cols <- function(df, cols) {
  df[, intersect(cols, names(df)), drop = FALSE]
}

budget_small <- if (quick) 160L else 512L
budget_medium <- if (quick) 320L else 1200L
budget_reference <- if (quick) 2000L else 12000L
budget_grid <- if (quick) c(96L, 192L, 384L) else c(128L, 256L, 512L, 1024L, 2048L)
max_turns <- if (quick) 140L else 250L
n_matchup_games <- if (quick) 80L else 400L
benchmark_budgets <- if (quick) c(128L, 512L) else c(256L, 1024L, 4096L)
benchmark_reference_budget <- if (quick) 3000L else 12000L

board <- bg_initial_board()
roll <- bg_roll(1L, 6L)
legal <- generate_legal_moves(board, roll)

# 1) Motivation / basics
write_txt("01_decision_instance.txt", {
  cat("Local decision instance:\n\n")
  cat(explain_position(board, roll), "\n\n")
  bg_print_board(board)
  cat("\n")
  print(roll)
})

write_csv(
  "02_legal_moves.csv",
  summarize_legal_moves(legal, max_candidates = 10L, print_table = FALSE)
)

move1 <- legal[[1L]]
board_after <- apply_move(board, move1)
write_txt("03_apply_move_example.txt", {
  cat("Chosen move label:\n")
  cat(paste(as.character(move1), collapse = ", "), "\n\n")
  cat("BEFORE\n")
  bg_print_board(board)
  cat("\nAFTER\n")
  bg_print_board(board_after)
})

g <- simulate_game(
  state = board,
  policy_white = "random",
  policy_black = "random",
  max_turns = 80L,
  seed = 1L
)
single_game_overview <- data.frame(
  player_1 = g$player1_selection,
  player_2 = g$player2_selection,
  n_turns = g$n_turns,
  game_over = g$game_over,
  winner = g$winner,
  turn_limit_reached = g$turn_limit_reached,
  stringsAsFactors = FALSE
)
single_game_history <- show_cols(
  utils::head(g$history, 8L),
  c("turn", "player", "selection", "die1", "die2", "n_legal_moves", "chosen_n_steps", "game_over", "winner")
)
write_csv("04_single_game_overview.csv", single_game_overview)
write_csv("05_single_game_history_head.csv", single_game_history)

matchup_random <- simulate_matchup(
  player1 = "random",
  player2 = "random",
  n_games = n_matchup_games,
  board = board,
  max_turns = max_turns,
  seed = 2L
)
write_csv(
  "06_random_matchup_summary.csv",
  show_cols(
    summary(matchup_random),
    c("n_games", "completed_games", "player1_wins", "player2_wins", "player1_win_rate", "player2_win_rate", "mean_turns")
  )
)

# 2) Thompson evaluation on one position
th <- evaluate_actions_thompson(
  board = board,
  roll = roll,
  total_budget = budget_medium,
  rollout_policy = "random",
  max_rollout_turns = max_turns,
  fast_diagnostics = FALSE,
  seed = 11L
)

write_csv("07_thompson_summary.csv", summary(th))
write_csv(
  "08_thompson_top_actions.csv",
  show_cols(
    compare_action_posteriors(th, top_n = 8L),
    c("move_label", "recommended", "allocation_count", "estimate", "uncertainty_sd", "prob_best", "exp_regret")
  )
)
write_txt("09_thompson_explanation.txt", cat(explain_move_evaluation(th), "\n"))

method_cmp <- compare_methods_on_position(
  board = board,
  roll = roll,
  methods = c("thompson", "equal", "ucb", "ocba", "greedy"),
  total_budget = budget_medium,
  rollout_policy = "random",
  max_rollout_turns = max_turns,
  fast_diagnostics = FALSE,
  seed = 21L
)
write_csv(
  "10_method_comparison.csv",
  show_cols(
    method_cmp$summary,
    c("method", "recommended_move_label", "recommended_estimate", "recommended_prob_best",
      "recommended_expected_regret", "recommended_allocation_count", "runtime_seconds")
  )
)

# 3) Trace and finite-vs-reference comparison
trace <- trace_thompson_allocation(
  board = board,
  roll = roll,
  method = "thompson",
  total_budget = budget_medium,
  rollout_policy = "random",
  max_rollout_turns = max_turns,
  trace_every = if (quick) 20L else 40L,
  seed = 31L
)
write_csv(
  "11_thompson_trace_checkpoints.csv",
  show_cols(
    trace$checkpoint_summary,
    c("checkpoint", "selected_move_label", "leader_move_label", "leader_estimate",
      "leader_posterior_sd", "leader_allocation_count")
  )
)
save_plot("11a_trace_allocation_count.png", plot_thompson_convergence(trace, metric = "allocation_count"))
save_plot("11b_trace_estimate.png", plot_thompson_convergence(trace, metric = "estimate"))

ref <- approximate_action_reference(
  board = board,
  roll = roll,
  truth_budget = budget_reference,
  rollout_policy = "random",
  max_rollout_turns = max_turns,
  seed = 41L
)
cert <- certify_reference_truth(reference = ref)
cmp <- compare_thompson_to_reference(
  board = board,
  roll = roll,
  method = "thompson",
  total_budget = budget_small,
  reference = ref,
  reference_certificate = cert,
  rollout_policy = "random",
  max_rollout_turns = max_turns,
  seed = 42L
)

write_csv("12_reference_certificate.csv", cert$certificate)
write_csv("13_thompson_vs_reference_summary.csv", cmp$summary)
write_csv(
  "14_thompson_vs_reference_actions.csv",
  show_cols(
    cmp$action_table[order(-cmp$action_table$finite_allocation_count), , drop = FALSE],
    c("move_label", "finite_recommended", "reference_best", "finite_allocation_count",
      "finite_estimate", "reference_estimate", "abs_error")
  )
)
save_plot("14a_reference_error_vs_allocation.png", {
  d <- cmp$action_table[order(cmp$action_table$finite_allocation_count, decreasing = TRUE), , drop = FALSE]
  graphics::plot(
    d$finite_allocation_count,
    d$abs_error,
    pch = 19,
    col = ifelse(d$finite_recommended, "#2E86AB", "#444444"),
    xlab = "Finite allocation count",
    ylab = "Absolute error vs reference",
    main = "Finite-vs-reference error concentration"
  )
  graphics::grid()
})

trade <- study_budget_tradeoff(
  board = board,
  roll = roll,
  method = "thompson",
  budgets = budget_grid,
  truth_budget = budget_reference,
  rollout_policy = "random",
  max_rollout_turns = max_turns,
  fast_diagnostics = FALSE,
  seed = 51L
)
write_csv(
  "15_budget_tradeoff.csv",
  show_cols(
    trade$results,
    c("total_budget", "chosen_move_label", "truth_best_move_label", "correct_selection",
      "simple_regret", "mse", "runtime_seconds")
  )
)
save_plot("15a_budget_tradeoff_curves.png", {
  d <- trade$results[order(trade$results$total_budget), , drop = FALSE]
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  graphics::plot(
    d$total_budget,
    d$simple_regret,
    type = "b",
    pch = 19,
    xlab = "Total budget",
    ylab = "Simple regret",
    main = "Budget vs simple regret"
  )
  graphics::plot(
    d$total_budget,
    d$mse,
    type = "b",
    pch = 19,
    xlab = "Total budget",
    ylab = "MSE vs reference",
    main = "Budget vs MSE"
  )
})

var_study <- study_variance_controls(
  board = board,
  roll = roll,
  method = "thompson",
  total_budget = budget_medium,
  dice_modes = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
  crn_values = c(FALSE, TRUE),
  truth_budget = budget_reference,
  rollout_policy = "random",
  max_rollout_turns = max_turns,
  fast_diagnostics = FALSE,
  seed = 56L
)
write_csv(
  "16_variance_controls.csv",
  show_cols(
    var_study$results,
    c("dice_mode", "crn", "chosen_move_label", "correct_selection",
      "simple_regret", "mse", "runtime_seconds")
  )
)

cases <- list(
  bg_benchmark_case(bg_initial_board(), bg_roll(1L, 6L), case_id = "init_1_6"),
  bg_benchmark_case(bg_initial_board(), bg_roll(3L, 2L), case_id = "init_3_2"),
  bg_benchmark_case(bg_initial_board(), bg_roll(5L, 4L), case_id = "init_5_4"),
  bg_benchmark_case(bg_initial_board(), bg_roll(2L, 1L), case_id = "init_2_1"),
  bg_benchmark_case(bg_initial_board(), bg_roll(6L, 6L), case_id = "init_6_6"),
  bg_benchmark_case(bg_initial_board(), bg_roll(4L, 3L), case_id = "init_4_3")
)
if (quick) {
  cases <- cases[1:3]
}

bm <- benchmark_thompson(
  cases = cases,
  budgets = benchmark_budgets,
  baselines = c("equal", "ucb", "ocba", "greedy"),
  include_ttts = TRUE,
  reference_budget = benchmark_reference_budget,
  rollout_policy = "random",
  max_rollout_turns = max_turns,
  seed = 59L
)
focus <- summarize_thompson_benchmark(bm)

write_csv(
  "17_benchmark_thompson_summary.csv",
  show_cols(
    focus$thompson,
    c("method", "total_budget", "probability_correct_selection", "mean_simple_regret",
      "mean_mse", "mean_runtime_seconds", "difficulty_label")
  )
)
write_csv(
  "18_benchmark_relative_to_thompson.csv",
  show_cols(
    focus$relative_to_thompson,
    c("baseline_method", "total_budget", "thompson_advantage_pcs",
      "thompson_advantage_regret", "thompson_advantage_mse", "thompson_runtime_ratio")
  )
)
if (nrow(focus$by_difficulty) > 0L) {
  write_csv(
    "19_benchmark_by_difficulty.csv",
    show_cols(
      focus$by_difficulty,
      c("method", "difficulty_label", "n_cases", "proxy_pcs",
        "mean_simple_regret", "mean_mse", "mean_runtime_seconds")
    )
  )
}
save_plot("19a_plot_thompson_vs_baselines_pcs.png", plot_thompson_vs_baselines(bm, metric = "correct_selection_rate"))
save_plot("19b_plot_thompson_vs_baselines_regret.png", plot_thompson_vs_baselines(bm, metric = "mean_simple_regret"))
save_plot("19c_plot_thompson_vs_baselines_runtime.png", plot_thompson_vs_baselines(bm, metric = "mean_runtime_seconds"))

# 4) Thompson as a player in many full games
thompson_matchup <- simulate_matchup(
  player1 = "thompson_rollout",
  player2 = "random",
  n_games = n_matchup_games,
  board = board,
  max_turns = max_turns,
  rollout_budget = if (quick) 8L else 16L,
  rollout_policy = "random",
  max_rollout_turns = max_turns,
  seed = 61L
)

write_csv(
  "20_thompson_matchup_summary.csv",
  show_cols(
    summary(thompson_matchup),
    c("n_games", "completed_games", "player1_wins", "player2_wins", "player1_win_rate", "player2_win_rate", "mean_turns")
  )
)
write_csv(
  "21_thompson_matchup_games_head.csv",
  show_cols(
    utils::head(thompson_matchup$games, 10L),
    c("game_id", "winner_label", "n_turns", "game_over", "turn_limit_reached")
  )
)

index_path <- file.path(out_dir, "README.txt")
index_lines <- c(
  "Example artifacts for the canonical vignette sequence.",
  "",
  sprintf("quick_mode: %s", quick),
  sprintf("generated_at: %s", as.character(Sys.time())),
  "",
  "Files:"
)
files <- sort(list.files(out_dir))
index_lines <- c(index_lines, paste0("- ", files))
writeLines(index_lines, index_path, useBytes = TRUE)
message("wrote: ", index_path)
message("example runner completed successfully.")

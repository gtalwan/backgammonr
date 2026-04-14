# 09_game_mechanics.R
#
# Purpose:
# - walk carefully through the backgammon engine layer;
# - show how a board and roll become legal move sequences and collapsed actions;
# - apply one concrete opening move to the board; and
# - show what a turn result and short game result look like.
#
# Main package functions used here:
# - bg_initial_board()                  [R/bg_engine_api.R]
# - bg_roll()                           [R/bg_engine_api.R]
# - bg_opening_problem()                [R/bg_truth.R]
# - bg_legal_moves()                    [R/bg_engine_api.R]
# - bg_apply_move_sequence()            [R/bg_engine_api.R]
# - bg_board_features()                 [R/bg_engine_api.R]
# - bg_move_features()                  [R/bg_engine_api.R]
# - bg_play_turn()                      [R/bg_engine_api.R]
# - bg_play_game()                      [R/bg_engine_api.R]
# - bg_truth_certify()                  [R/bg_truth.R]
#
# Relevant native files:
# - src/bg_board.cpp
# - src/bg_movegen.cpp
# - src/bg_game.cpp

common_path <- if (file.exists("DESCRIPTION")) {
  file.path("Presentation", "presentation_common.R")
} else {
  "presentation_common.R"
}
source(common_path)

repo_root <- presentation_repo_root()
presentation_load_package(repo_root)
library(ggplot2)

roll_label <- "1-6"
dice_values <- as.integer(strsplit(roll_label, "-", fixed = TRUE)[[1L]])
opening_roll <- bg_roll(dice_values[[1L]], dice_values[[2L]])
initial_board <- bg_initial_board()
player_to_move <- initial_board$turn

# `bg_opening_problem()` is the main front door from engine state to local
# decision problem. It bundles the board, realized roll, legal move sequences,
# and collapsed candidate-action table in one object.
opening_problem <- bg_opening_problem(opening_roll)
feature_bundle_before <- bg_board_features(opening_problem, player = player_to_move)
candidate_table <- opening_problem$candidate_table[
  ,
  c(
    "candidate_index",
    "move_label",
    "n_equivalent_sequences",
    "n_steps",
    "n_hits",
    "n_bar_entries",
    "n_bear_off",
    "total_step_distance"
  )
]

# The preserved master truth is only used here to identify one concrete move to
# apply. The script still centers on the engine mechanics rather than the
# projected statistical models.
master_truth <- presentation_load_opening_truth(repo_root, roll_label)
truth_cert <- bg_truth_certify(master_truth)
best_move_label <- truth_cert$best_move_label[[1L]]
best_index <- match(best_move_label, opening_problem$candidate_table$move_label)
best_move <- opening_problem$candidate_table$move[[best_index]]

board_after_best <- bg_apply_move_sequence(initial_board, best_move)
feature_bundle_after <- bg_board_features(board_after_best, player = player_to_move)

board_feature_delta <- data.frame(
  feature = names(feature_bundle_before$board_features),
  before = as.numeric(feature_bundle_before$board_features[1, ]),
  after = as.numeric(feature_bundle_after$board_features[1, ]),
  delta = as.numeric(feature_bundle_after$board_features[1, ] - feature_bundle_before$board_features[1, ]),
  stringsAsFactors = FALSE
)

move_sequence_label <- function(move_sequence) {
  paste(
    vapply(
      move_sequence$steps,
      function(step) paste0(step$from, "->", step$to),
      character(1L)
    ),
    collapse = ", "
  )
}

turn_result <- bg_play_turn(initial_board, roll = opening_roll, selection = "first")
turn_result_summary <- data.frame(
  opening_roll = roll_label,
  selection = turn_result$selection,
  n_legal_moves = turn_result$n_legal_moves,
  chosen_move_label = move_sequence_label(turn_result$chosen_move),
  turn_passed = turn_result$turn_passed,
  game_over = turn_result$game_over,
  winner = turn_result$winner,
  stringsAsFactors = FALSE
)

short_game <- bg_play_game(
  board = bg_initial_board(),
  max_turns = 12L,
  selection = "first",
  seed = 1L
)

game_history <- short_game$history
game_history$player_label <- ifelse(game_history$player == 1L, "Player 1", "Player -1")

initial_board_lines <- capture.output(bg_print_board(initial_board))
after_board_lines <- capture.output(bg_print_board(board_after_best))
presentation_save_lines(initial_board_lines, repo_root, "09_initial_board_ascii")
presentation_save_lines(after_board_lines, repo_root, "09_board_after_truth_best_ascii")

candidate_table$is_truth_best <- candidate_table$move_label == best_move_label
candidate_table$is_truth_best <- factor(
  candidate_table$is_truth_best,
  levels = c(FALSE, TRUE),
  labels = c("Other", "Truth-best")
)

# This plot shows the collapsing step from legal move sequences to candidate
# actions. Most opening moves have multiple equivalent legal sequences, but the
# algorithm layer works on the collapsed candidates.
candidate_collapse_plot <- ggplot(
  candidate_table,
  aes(
    x = n_equivalent_sequences,
    y = reorder(move_label, n_equivalent_sequences),
    fill = is_truth_best
  )
) +
  geom_col(color = "#1F1F1F", linewidth = 0.2) +
  scale_fill_manual(values = c("Other" = "#C9D2D8", "Truth-best" = "#D55E00")) +
  labs(
    title = paste("Collapsed candidate actions for opening", roll_label),
    subtitle = "The highlighted bar is the truth-best move from the preserved master cache.",
    x = "Equivalent legal move sequences",
    y = NULL,
    fill = "Truth-best"
  ) +
  bg_plot_theme_research()

delta_plot_data <- subset(board_feature_delta, delta != 0)

# This delta plot is the most concrete "what changed on the board?" visual in
# the file. It shows how one move alters the local structure immediately.
board_delta_plot <- ggplot(
  delta_plot_data,
  aes(x = reorder(feature, delta), y = delta, fill = delta > 0)
) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = c(`TRUE` = "#0072B2", `FALSE` = "#D55E00")) +
  labs(
    title = paste("Board-feature change after the truth-best move on", roll_label),
    subtitle = "Positive values mean the feature increased after the move was applied.",
    x = NULL,
    y = "After minus before"
  ) +
  bg_plot_theme_research()

# This short game-history plot is not a quality benchmark. It simply shows what
# the turn-level game output looks like and how the engine records each turn.
game_history_plot <- ggplot(
  game_history,
  aes(x = turn, y = n_legal_moves, color = player_label)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Short game history under the simple 'first legal move' policy",
    subtitle = "This is a mechanics object demonstration, not a recommendation study.",
    x = "Turn",
    y = "Number of legal moves",
    color = "Player"
  ) +
  bg_plot_theme_research()

mechanics_summary <- data.frame(
  opening_roll = roll_label,
  n_legal_move_sequences = length(opening_problem$legal_moves),
  n_collapsed_candidates = nrow(candidate_table),
  truth_best_move_label = best_move_label,
  short_game_turns = short_game$n_turns,
  short_game_game_over = short_game$game_over,
  short_game_winner = short_game$winner,
  stringsAsFactors = FALSE
)

presentation_save_table(mechanics_summary, repo_root, "09_game_mechanics_summary")
presentation_save_table(candidate_table, repo_root, "09_game_mechanics_candidate_table")
presentation_save_table(board_feature_delta, repo_root, "09_game_mechanics_board_feature_delta")
presentation_save_table(turn_result_summary, repo_root, "09_game_mechanics_turn_result")
presentation_save_table(game_history, repo_root, "09_game_mechanics_short_game_history")
presentation_save_plot(candidate_collapse_plot, repo_root, "09_game_mechanics_candidate_collapse", width = 10, height = 7)
presentation_save_plot(board_delta_plot, repo_root, "09_game_mechanics_board_delta", width = 10, height = 6)
presentation_save_plot(game_history_plot, repo_root, "09_game_mechanics_short_game_history", width = 10, height = 6)

# This one-row summary is the quickest orientation table for the file.
print(mechanics_summary)

# Printing the boards keeps the mechanics layer tangible. These are the exact
# board objects before the move and after the truth-best move is applied.
print(initial_board)
print(board_after_best)

# These tables show the candidate actions, the immediate board-state change from
# the truth-best move, and the shape of a one-turn engine result.
print(candidate_table)
print(board_feature_delta)
print(turn_result_summary)
print(utils::head(game_history, 12L))

print(candidate_collapse_plot)
print(board_delta_plot)
print(game_history_plot)

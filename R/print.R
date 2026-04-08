# Print and formatting methods for board, roll, move, and study objects.
#' Print a backgammon board
#'
#' @param x A `bg_board` object.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.bg_board <- function(x, ...) {
  info <- bg_inspect_board(x)
  occupied_n <- sum(info$points$n_checkers > 0L)
  cat("<bg_board>\n", sep = "")
  cat("occupied points: ", occupied_n, "\n", sep = "")
  cat(format(x, ...), sep = "\n")

  invisible(x)
}

#' Print a backgammon dice roll
#'
#' @param x A `bg_roll` object.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.bg_roll <- function(x, ...) {
  if (!is_bg_roll(x)) {
    stop("`x` must inherit from class 'bg_roll'.", call. = FALSE)
  }

  cat("<bg_roll>\n", sep = "")
  cat("dice:     ", paste(x$dice, collapse = " "), "\n", sep = "")
  cat("double:   ", if (isTRUE(x$is_double)) "yes" else "no", "\n", sep = "")
  cat("expanded: ", paste(x$expanded, collapse = " "), "\n", sep = "")

  invisible(x)
}

#' Print a backgammon move step
#'
#' @param x A `bg_move_step` object.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.bg_move_step <- function(x, ...) {
  if (!is_bg_move_step(x)) {
    stop("`x` must inherit from class 'bg_move_step'.", call. = FALSE)
  }

  cat("<bg_move_step>\n", sep = "")
  cat("from: ", x$from, "\n", sep = "")
  cat("to:   ", x$to, "\n", sep = "")
  cat("die:  ", x$die, "\n", sep = "")
  cat("hit:  ", if (isTRUE(x$hit)) "yes" else "no", "\n", sep = "")

  invisible(x)
}

#' Print a backgammon move sequence
#'
#' @param x A `bg_move_sequence` object.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.bg_move_sequence <- function(x, ...) {
  if (!is_bg_move_sequence(x)) {
    stop("`x` must inherit from class 'bg_move_sequence'.", call. = FALSE)
  }

  cat("<bg_move_sequence>\n", sep = "")
  cat("player:   ", if (x$player == 1L) "player_1" else "player_2", "\n", sep = "")
  cat("n_steps:  ", x$n_steps, "\n", sep = "")
  cat("dice_used:", if (length(x$dice_used) == 0L) " <none>" else paste(" ", paste(x$dice_used, collapse = " "), sep = ""), "\n", sep = "")

  if (is.null(x$roll)) {
    cat("roll:     <none>\n", sep = "")
  } else {
    cat("roll:     ", paste(x$roll$dice, collapse = " "), "\n", sep = "")
  }

  if (length(x$steps) == 0L) {
    cat("steps:    <none>\n", sep = "")
  } else {
    step_df <- data.frame(
      idx = seq_along(x$steps),
      from = vapply(x$steps, function(step) step$from, integer(1L)),
      to = vapply(x$steps, function(step) step$to, integer(1L)),
      die = vapply(x$steps, function(step) step$die, integer(1L)),
      hit = vapply(x$steps, function(step) step$hit, logical(1L)),
      stringsAsFactors = FALSE
    )
    print(step_df, row.names = FALSE)
  }

  invisible(x)
}

#' Print a turn result
#'
#' @param x A `bg_turn_result` object.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.bg_turn_result <- function(x, ...) {
  if (!is_bg_turn_result(x)) {
    stop("`x` must inherit from class 'bg_turn_result'.", call. = FALSE)
  }

  cat("<bg_turn_result>\n", sep = "")
  cat("player:       ", if (x$player == 1L) "player_1" else "player_2", "\n", sep = "")
  cat("selection:    ", x$selection, "\n", sep = "")
  cat("roll:         ", paste(x$roll$dice, collapse = " "), "\n", sep = "")
  cat("n_legal_moves:", x$n_legal_moves, "\n", sep = "")
  cat("turn_passed:  ", if (isTRUE(x$turn_passed)) "yes" else "no", "\n", sep = "")
  cat("game_over:    ", if (isTRUE(x$game_over)) "yes" else "no", "\n", sep = "")
  cat("winner:       ", x$winner, "\n", sep = "")

  invisible(x)
}

#' Print a game result
#'
#' @param x A `bg_game_result` object.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.bg_game_result <- function(x, ...) {
  if (!is_bg_game_result(x)) {
    stop("`x` must inherit from class 'bg_game_result'.", call. = FALSE)
  }

  cat("<bg_game_result>\n", sep = "")
  cat("player_1_selection:    ", x$player1_selection, "\n", sep = "")
  cat("player_2_selection:    ", x$player2_selection, "\n", sep = "")
  cat("n_turns:               ", x$n_turns, "\n", sep = "")
  cat("game_over:             ", if (isTRUE(x$game_over)) "yes" else "no", "\n", sep = "")
  cat("winner:                ", x$winner, "\n", sep = "")
  cat("turn_limit_reached:    ", if (isTRUE(x$turn_limit_reached)) "yes" else "no", "\n", sep = "")
  cat("roll_sequence_exhausted:", if (isTRUE(x$roll_sequence_exhausted)) "yes" else "no", "\n", sep = "")

  if (nrow(x$history) > 0L) {
    print(x$history, row.names = FALSE)
  }

  invisible(x)
}

#' Print a matchup-simulation result
#'
#' @param x A `bg_matchup_simulation` object.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.bg_matchup_simulation <- function(x, ...) {
  if (!is_bg_matchup_simulation(x)) {
    stop("`x` must inherit from class 'bg_matchup_simulation'.", call. = FALSE)
  }

  cat("<bg_matchup_simulation>\n", sep = "")
  cat("player_1: ", x$settings$player1_selection, "\n", sep = "")
  cat("player_2: ", x$settings$player2_selection, "\n", sep = "")
  cat("n_games:  ", x$settings$n_games, "\n", sep = "")
  cat("max_turns:", x$settings$max_turns, "\n", sep = "")
  cat("scripted: ", if (isTRUE(x$settings$used_scripted_rolls)) "yes" else "no", "\n", sep = "")

  if (bg_is_rollout_family_selection(x$settings$player1_selection) ||
      bg_is_rollout_family_selection(x$settings$player2_selection)) {
    cat("rollout_budget: ", x$settings$rollout_budget, "\n", sep = "")
    cat("rollout_policy: ", x$settings$rollout_policy, "\n", sep = "")
    cat("max_rollout_turns: ", x$settings$max_rollout_turns, "\n", sep = "")
  }

  if (!is.null(x$summary)) {
    cat("summary:\n", sep = "")
    print(x$summary, row.names = FALSE)
  } else {
    cat("games (first rows):\n", sep = "")
    print(utils::head(x$games), row.names = FALSE)
  }

  invisible(x)
}

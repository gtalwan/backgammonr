# Matchup and simulation helpers for repeated game experiments.
bg_match_simulation_archetype <- function(selection) {
  match.arg(
    selection,
    choices = c(
      "random",
      "aggressive",
      "defensive",
      "rollout",
      "equal_rollout",
      "greedy_rollout",
      "ucb_rollout",
      "ocba_rollout",
      "thompson_rollout",
      "ttts_rollout"
    )
  )
}

bg_new_matchup_simulation <- function(x, include_summary = TRUE) {
  x$initial_board <- bg_new_board(x$initial_board)
  x$games <- as.data.frame(x$games, stringsAsFactors = FALSE)
  x$summary <- if (isTRUE(include_summary)) {
    as.data.frame(x$summary, stringsAsFactors = FALSE)
  } else {
    NULL
  }
  x$settings <- list(
    player1_selection = as.character(x$settings$player1_selection[[1L]]),
    player2_selection = as.character(x$settings$player2_selection[[1L]]),
    n_games = as.integer(x$settings$n_games[[1L]]),
    max_turns = as.integer(x$settings$max_turns[[1L]]),
    used_scripted_rolls = isTRUE(x$settings$used_scripted_rolls[[1L]]),
    rollout_budget = as.integer(x$settings$rollout_budget[[1L]]),
    rollout_policy = as.character(x$settings$rollout_policy[[1L]]),
    max_rollout_turns = as.integer(x$settings$max_rollout_turns[[1L]])
  )
  structure(x, class = "bg_matchup_simulation")
}

#' Test whether an object is a matchup-simulation result
#'
#' @param x An object.
#'
#' @return `TRUE` if `x` inherits from class `"bg_matchup_simulation"`.
#' @export
is_bg_matchup_simulation <- function(x) {
  inherits(x, "bg_matchup_simulation")
}

#' Simulate repeated games between two archetypes
#'
#' Simulates many games between two specified archetypes and returns a compact,
#' R-friendly result containing one row per game plus optional summary
#' statistics.
#'
#' The currently supported archetypes are:
#'
#' - `"random"`
#' - `"aggressive"`
#' - `"defensive"`
#' - `"rollout"`
#' - `"ocba_rollout"`
#' - `"thompson_rollout"`
#' - `"ttts_rollout"`
#'
#' The heavy replication loop runs in C++. Only compact per-game summaries are
#' returned, rather than full turn-by-turn histories, so this function is more
#' suitable for repeated simulation than calling [bg_play_game()] in a loop.
#'
#' When either player is in the rollout family (`"rollout"` or
#' `"thompson_rollout"`/`"ttts_rollout"`/`"ocba_rollout"`), the same rollout configuration is used by every
#' rollout-based player in the matchup.
#'
#' @param player1 Archetype for player 1.
#' @param player2 Archetype for player 2.
#' @param n_games Integer-like scalar giving the number of games to simulate.
#' @param board A `bg_board` object giving the initial position for each game.
#' @param max_turns Integer-like scalar giving the maximum number of turns per
#'   game.
#' @param roll_sequence Optional list of rolls or a single `bg_roll` object.
#'   When supplied, the same scripted roll sequence is used for every
#'   replication.
#' @param seed Optional integer-like scalar. When `roll_sequence` is `NULL`,
#'   this controls reproducible dice rolling. When either player uses a
#'   stochastic policy, it also controls reproducible move selection.
#' @param include_summary Logical scalar. If `TRUE`, a one-row summary data frame
#'   is included in the result.
#' @param rollout_budget Integer-like scalar giving the fixed total rollout budget
#'   when either player is rollout-based.
#' @param rollout_policy Baseline policy used inside rollout playouts.
#' @param max_rollout_turns Integer-like scalar giving the maximum number of
#'   turns for each rollout playout.
#'
#' @return An object of class `bg_matchup_simulation` with components:
#'   - `games`: a data frame with one row per game;
#'   - `summary`: a one-row summary data frame, or `NULL`;
#'   - `settings`: a list of simulation settings;
#'   - `initial_board`: the starting board used for each replication.
#'
#' @export
#'
#' @examples
#' points <- integer(24)
#' points[8] <- 1L
#' points[17] <- -1L
#' board <- bg_board(points = points, off = c(14L, 14L), turn = 1L)
#'
#' sim <- simulate_matchup(
#'   player1 = "thompson_rollout",
#'   player2 = "random",
#'   n_games = 3L,
#'   board = board,
#'   max_turns = 20L,
#'   rollout_budget = 4L,
#'   seed = 123
#' )
#'
#' sim$games
#' sim$summary
simulate_matchup <- function(
    player1 = c("random", "aggressive", "defensive", "rollout", "equal_rollout", "greedy_rollout", "ucb_rollout", "ocba_rollout", "thompson_rollout", "ttts_rollout"),
    player2 = c("random", "aggressive", "defensive", "rollout", "equal_rollout", "greedy_rollout", "ucb_rollout", "ocba_rollout", "thompson_rollout", "ttts_rollout"),
    n_games = 100L,
    board = bg_initial_board(),
    max_turns = 1000L,
    roll_sequence = NULL,
    seed = NULL,
    include_summary = TRUE,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L) {
  if (!is_bg_board(board)) {
    stop("`board` must inherit from class 'bg_board'.", call. = FALSE)
  }

  player1 <- bg_match_simulation_archetype(player1)
  player2 <- bg_match_simulation_archetype(player2)
  bg_validate_board(board)

  n_games <- bg_coerce_integerish(n_games, "n_games", 1L)
  if (n_games < 1L) {
    stop("`n_games` must be at least 1.", call. = FALSE)
  }

  max_turns <- bg_coerce_integerish(max_turns, "max_turns", 1L)
  if (max_turns < 0L) {
    stop("`max_turns` must be nonnegative.", call. = FALSE)
  }

  bg_assert_scalar_flag(include_summary, "include_summary")

  seed_args <- bg_normalize_seed_args(seed)
  uses_rollout <- bg_is_rollout_family_selection(player1) || bg_is_rollout_family_selection(player2)
  rollout_args <- if (uses_rollout) {
    bg_normalize_rollout_args(
      rollout_budget = rollout_budget,
      rollout_policy = rollout_policy,
      max_rollout_turns = max_rollout_turns
    )
  } else {
    NULL
  }

  if (is.null(roll_sequence)) {
    out <- if (uses_rollout) {
      bg_cpp_simulate_matchup_random_rollout(
        unclass(board),
        n_games,
        max_turns,
        player1,
        player2,
        rollout_args$rollout_budget,
        rollout_args$rollout_policy,
        rollout_args$max_rollout_turns,
        seed_args$seed,
        seed_args$use_seed
      )
    } else {
      bg_cpp_simulate_matchup_random(
        unclass(board),
        n_games,
        max_turns,
        player1,
        player2,
        seed_args$seed,
        seed_args$use_seed
      )
    }
  } else {
    rolls <- bg_normalize_roll_sequence(roll_sequence)
    out <- if (uses_rollout) {
      bg_cpp_simulate_matchup_scripted_rollout(
        unclass(board),
        rolls,
        n_games,
        max_turns,
        player1,
        player2,
        rollout_args$rollout_budget,
        rollout_args$rollout_policy,
        rollout_args$max_rollout_turns,
        seed_args$seed,
        seed_args$use_seed
      )
    } else {
      bg_cpp_simulate_matchup_scripted(
        unclass(board),
        rolls,
        n_games,
        max_turns,
        player1,
        player2,
        seed_args$seed,
        seed_args$use_seed
      )
    }
  }

  bg_new_matchup_simulation(out, include_summary = include_summary)
}

#' Summarize a matchup-simulation result
#'
#' @param object A `bg_matchup_simulation` object.
#' @param ... Unused.
#'
#' @return A one-row summary data frame, or `NULL`.
#' @export
summary.bg_matchup_simulation <- function(object, ...) {
  object$summary
}

# Turn-level and game-level simulation wrappers around the C++ engine.
bg_match_engine_selection <- function(selection) {
  match.arg(
    selection,
    choices = c(
      "first",
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

bg_match_matchup_selection <- function(selection) {
  bg_match_engine_selection(selection)
}

bg_is_rollout_family_selection <- function(selection) {
  selection %in% c("rollout", "equal_rollout", "greedy_rollout", "ucb_rollout", "ocba_rollout", "thompson_rollout", "ttts_rollout")
}

bg_normalize_seed_args <- function(seed) {
  if (is.null(seed)) {
    return(list(seed = 0L, use_seed = FALSE))
  }

  seed <- bg_coerce_integerish(seed, "seed", 1L)
  if (seed < 0L) {
    stop("`seed` must be nonnegative when supplied.", call. = FALSE)
  }

  list(seed = seed, use_seed = TRUE)
}

bg_new_turn_result <- function(x) {
  x$board_before <- bg_new_board(x$board_before)
  x$board_after <- bg_new_board(x$board_after)
  x$roll <- bg_new_roll(x$roll)
  x$legal_moves <- lapply(x$legal_moves, bg_new_move_sequence)
  x$chosen_move <- bg_wrap_move_sequence_output(x$chosen_move)
  structure(x, class = "bg_turn_result")
}

bg_turns_to_history <- function(turns) {
  if (length(turns) == 0L) {
    return(data.frame(
      turn = integer(0L),
      player = integer(0L),
      selection = character(0L),
      die1 = integer(0L),
      die2 = integer(0L),
      n_legal_moves = integer(0L),
      turn_passed = logical(0L),
      chosen_n_steps = integer(0L),
      game_over = logical(0L),
      winner = integer(0L),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    turn = seq_along(turns),
    player = vapply(turns, function(x) x$player, integer(1L)),
    selection = vapply(turns, function(x) x$selection, character(1L)),
    die1 = vapply(turns, function(x) x$roll$dice[1L], integer(1L)),
    die2 = vapply(turns, function(x) x$roll$dice[2L], integer(1L)),
    n_legal_moves = vapply(turns, function(x) x$n_legal_moves, integer(1L)),
    turn_passed = vapply(turns, function(x) x$turn_passed, logical(1L)),
    chosen_n_steps = vapply(
      turns,
      function(x) if (is.null(x$chosen_move)) 0L else x$chosen_move$n_steps,
      integer(1L)
    ),
    game_over = vapply(turns, function(x) x$game_over, logical(1L)),
    winner = vapply(turns, function(x) x$winner, integer(1L)),
    stringsAsFactors = FALSE
  )
}

bg_new_game_result <- function(x) {
  x$initial_board <- bg_new_board(x$initial_board)
  x$final_board <- bg_new_board(x$final_board)
  x$turns <- lapply(x$turns, bg_new_turn_result)
  x$history <- bg_turns_to_history(x$turns)
  structure(x, class = "bg_game_result")
}

#' Test whether an object is a turn result
#'
#' @param x An object.
#'
#' @return `TRUE` if `x` inherits from class `"bg_turn_result"`.
#' @export
is_bg_turn_result <- function(x) {
  inherits(x, "bg_turn_result")
}

#' Test whether an object is a game result
#'
#' @param x An object.
#'
#' @return `TRUE` if `x` inherits from class `"bg_game_result"`.
#' @export
is_bg_game_result <- function(x) {
  inherits(x, "bg_game_result")
}

#' Apply a chosen move sequence to a board
#'
#' Applies a full-turn move sequence to a board and returns the resulting board.
#' The sequence must include a `roll` field so full-turn legality can be checked
#' against the legal-move generator.
#'
#' @param board A `bg_board` object.
#' @param move_sequence A `bg_move_sequence` object.
#'
#' @return A `bg_board` object representing the board after the move sequence is
#'   applied. The returned board has `turn` switched to the opposing player.
#' @export
bg_apply_move_sequence <- function(board, move_sequence) {
  if (!is_bg_board(board)) {
    stop("`board` must inherit from class 'bg_board'.", call. = FALSE)
  }

  if (!is_bg_move_sequence(move_sequence)) {
    stop("`move_sequence` must inherit from class 'bg_move_sequence'.", call. = FALSE)
  }

  bg_validate_board(board)
  bg_new_board(bg_cpp_apply_move_sequence(unclass(board), unclass(move_sequence)))
}

bg_normalize_roll_sequence <- function(roll_sequence) {
  if (is_bg_roll(roll_sequence)) {
    return(list(unclass(roll_sequence)))
  }

  if (!is.list(roll_sequence)) {
    stop("`roll_sequence` must be a list of rolls or a single `bg_roll` object.", call. = FALSE)
  }

  lapply(roll_sequence, function(x) unclass(bg_as_roll(x)))
}

#' Play a single turn
#'
#' Rolls dice if needed, generates all legal full-turn move sequences, selects a
#' move, applies it, and returns a structured turn record.
#'
#' Supported selection methods are:
#'
#' - `"first"`: choose the first legal move sequence returned by the engine;
#' - `"random"`: choose uniformly at random from the legal move set;
#' - `"aggressive"`: score resulting boards with the aggressive heuristic and
#'   choose the highest-scoring move;
#' - `"defensive"`: score resulting boards with the defensive heuristic and
#'   choose the highest-scoring move;
#' - `"rollout"`: estimate candidate win probabilities by uniform Monte Carlo
#'   rollout and choose the move with the highest estimated win rate;
#' - `"thompson_rollout"`: adaptively allocate rollout budget across legal
#'   moves with Thompson sampling and choose the move with the highest
#'   posterior mean.
#' - `"ttts_rollout"`: adaptively allocate rollout budget using top-two
#'   Thompson sampling aimed at fixed-budget best-action identification.
#' - `"ocba_rollout"`: adaptively allocate rollout budget using an
#'   OCBA-inspired targeting rule.
#'
#' When `selection` is in the rollout family, unresolved rollout games count as
#' half a win in the reported diagnostics and contribute a fractional posterior
#' update for Thompson sampling.
#'
#' @param board A `bg_board` object.
#' @param roll Optional `bg_roll` object or roll-like list. If `NULL`, dice are
#'   rolled in C++.
#' @param selection Move-selection rule.
#' @param seed Optional integer-like scalar. When `roll = NULL`, this controls
#'   reproducible dice rolling. When `selection` uses randomness, it also
#'   controls reproducible move choice.
#' @param rollout_budget Integer-like scalar giving the fixed total rollout budget
#'   when `selection` is rollout-based.
#' @param rollout_policy Baseline policy used inside rollout playouts.
#' @param max_rollout_turns Integer-like scalar giving the maximum number of
#'   turns for each rollout playout.
#'
#' @return A `bg_turn_result` object.
#' @export
#'
#' @examples
#' points <- integer(24)
#' points[8] <- 1L
#' points[17] <- -1L
#' board <- bg_board(points = points, off = c(14L, 14L), turn = 1L)
#' bg_play_turn(
#'   board,
#'   roll = bg_roll(3, 2),
#'   selection = "rollout",
#'   rollout_budget = 8L,
#'   seed = 123
#' )
bg_play_turn <- function(
    board,
    roll = NULL,
    selection = c("first", "random", "aggressive", "defensive", "rollout", "equal_rollout", "greedy_rollout", "ucb_rollout", "ocba_rollout", "thompson_rollout", "ttts_rollout"),
    seed = NULL,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L) {
  if (!is_bg_board(board)) {
    stop("`board` must inherit from class 'bg_board'.", call. = FALSE)
  }

  selection <- bg_match_engine_selection(selection)
  bg_validate_board(board)
  seed_args <- bg_normalize_seed_args(seed)
  rollout_args <- if (bg_is_rollout_family_selection(selection)) {
    bg_normalize_rollout_args(
      rollout_budget = rollout_budget,
      rollout_policy = rollout_policy,
      max_rollout_turns = max_rollout_turns
    )
  } else {
    NULL
  }

  if (is.null(roll)) {
    out <- if (bg_is_rollout_family_selection(selection)) {
      bg_cpp_play_turn_random_rollout(
        unclass(board),
        selection,
        rollout_args$rollout_budget,
        rollout_args$rollout_policy,
        rollout_args$max_rollout_turns,
        seed_args$seed,
        seed_args$use_seed
      )
    } else {
      bg_cpp_play_turn_random(unclass(board), selection, seed_args$seed, seed_args$use_seed)
    }
  } else {
    roll <- unclass(bg_as_roll(roll))
    out <- if (bg_is_rollout_family_selection(selection)) {
      bg_cpp_play_turn_with_roll_rollout(
        unclass(board),
        roll,
        selection,
        rollout_args$rollout_budget,
        rollout_args$rollout_policy,
        rollout_args$max_rollout_turns,
        seed_args$seed,
        seed_args$use_seed
      )
    } else {
      bg_cpp_play_turn_with_roll(
        unclass(board),
        roll,
        selection,
        seed_args$seed,
        seed_args$use_seed
      )
    }
  }

  bg_new_turn_result(out)
}

#' Play a full game with one selection rule for both players
#'
#' Alternates turns, rolls dice or consumes a scripted roll sequence, generates
#' legal moves, selects a move, and stops when the game ends or a stopping
#' condition is reached.
#'
#' Supported selection methods are the same as in [bg_play_turn()].
#'
#' @param board A `bg_board` object giving the initial position.
#' @param roll_sequence Optional list of rolls or a single `bg_roll` object.
#' @param max_turns Integer-like scalar giving the maximum number of turns to
#'   play.
#' @param selection Move-selection rule used by both players.
#' @param seed Optional integer-like scalar. When `roll_sequence` is `NULL`,
#'   this controls reproducible dice rolling. When `selection` uses randomness,
#'   it also controls reproducible move choice.
#' @param rollout_budget Integer-like scalar giving the fixed total rollout budget
#'   when `selection` is rollout-based.
#' @param rollout_policy Baseline policy used inside rollout playouts.
#' @param max_rollout_turns Integer-like scalar giving the maximum number of
#'   turns for each rollout playout.
#'
#' @return A `bg_game_result` object with nested turn history and a compact
#'   `history` data frame.
#' @export
bg_play_game <- function(
    board = bg_initial_board(),
    roll_sequence = NULL,
    max_turns = 1000L,
    selection = c("first", "random", "aggressive", "defensive", "rollout", "equal_rollout", "greedy_rollout", "ucb_rollout", "ocba_rollout", "thompson_rollout", "ttts_rollout"),
    seed = NULL,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L) {
  if (!is_bg_board(board)) {
    stop("`board` must inherit from class 'bg_board'.", call. = FALSE)
  }

  selection <- bg_match_engine_selection(selection)
  bg_validate_board(board)
  max_turns <- bg_coerce_integerish(max_turns, "max_turns", 1L)
  if (max_turns < 0L) {
    stop("`max_turns` must be nonnegative.", call. = FALSE)
  }

  seed_args <- bg_normalize_seed_args(seed)
  rollout_args <- if (bg_is_rollout_family_selection(selection)) {
    bg_normalize_rollout_args(
      rollout_budget = rollout_budget,
      rollout_policy = rollout_policy,
      max_rollout_turns = max_rollout_turns
    )
  } else {
    NULL
  }

  if (is.null(roll_sequence)) {
    out <- if (bg_is_rollout_family_selection(selection)) {
      bg_cpp_play_game_random_rollout(
        unclass(board),
        max_turns,
        selection,
        rollout_args$rollout_budget,
        rollout_args$rollout_policy,
        rollout_args$max_rollout_turns,
        seed_args$seed,
        seed_args$use_seed
      )
    } else {
      bg_cpp_play_game_random(unclass(board), max_turns, selection, seed_args$seed, seed_args$use_seed)
    }
  } else {
    rolls <- bg_normalize_roll_sequence(roll_sequence)
    out <- if (bg_is_rollout_family_selection(selection)) {
      bg_cpp_play_game_scripted_rollout(
        unclass(board),
        rolls,
        max_turns,
        selection,
        rollout_args$rollout_budget,
        rollout_args$rollout_policy,
        rollout_args$max_rollout_turns,
        seed_args$seed,
        seed_args$use_seed
      )
    } else {
      bg_cpp_play_game_scripted(
        unclass(board),
        rolls,
        max_turns,
        selection,
        seed_args$seed,
        seed_args$use_seed
      )
    }
  }

  bg_new_game_result(out)
}

#' Play a full game between two specified archetypes
#'
#' Simulates a full game with independently chosen selection rules for player 1
#' and player 2.
#'
#' @param board A `bg_board` object giving the initial position.
#' @param player1 Selection rule for player 1.
#' @param player2 Selection rule for player 2.
#' @param roll_sequence Optional list of rolls or a single `bg_roll` object.
#' @param max_turns Integer-like scalar giving the maximum number of turns to
#'   play.
#' @param seed Optional integer-like scalar controlling reproducible randomness.
#' @param rollout_budget Integer-like scalar giving the fixed total rollout budget
#'   when either player is rollout-based.
#' @param rollout_policy Baseline policy used inside rollout playouts.
#' @param max_rollout_turns Integer-like scalar giving the maximum number of
#'   turns for each rollout playout.
#'
#' @return A `bg_game_result` object.
#' @export
#'
#' @examples
#' points <- integer(24)
#' points[8] <- 1L
#' points[17] <- -1L
#' board <- bg_board(points = points, off = c(14L, 14L), turn = 1L)
#' game <- bg_play_game_matchup(
#'   board = board,
#'   player1 = "thompson_rollout",
#'   player2 = "random",
#'   max_turns = 20L,
#'   rollout_budget = 8L,
#'   seed = 123
#' )
#' game$n_turns
bg_play_game_matchup <- function(
    board = bg_initial_board(),
    player1 = c("first", "random", "aggressive", "defensive", "rollout", "equal_rollout", "greedy_rollout", "ucb_rollout", "ocba_rollout", "thompson_rollout", "ttts_rollout"),
    player2 = c("first", "random", "aggressive", "defensive", "rollout", "equal_rollout", "greedy_rollout", "ucb_rollout", "ocba_rollout", "thompson_rollout", "ttts_rollout"),
    roll_sequence = NULL,
    max_turns = 1000L,
    seed = NULL,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L) {
  if (!is_bg_board(board)) {
    stop("`board` must inherit from class 'bg_board'.", call. = FALSE)
  }

  player1 <- bg_match_matchup_selection(player1)
  player2 <- bg_match_matchup_selection(player2)
  bg_validate_board(board)
  max_turns <- bg_coerce_integerish(max_turns, "max_turns", 1L)
  if (max_turns < 0L) {
    stop("`max_turns` must be nonnegative.", call. = FALSE)
  }

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
      bg_cpp_play_game_matchup_random_rollout(
        unclass(board),
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
      bg_cpp_play_game_matchup_random(
        unclass(board),
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
      bg_cpp_play_game_matchup_scripted_rollout(
        unclass(board),
        rolls,
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
      bg_cpp_play_game_matchup_scripted(
        unclass(board),
        rolls,
        max_turns,
        player1,
        player2,
        seed_args$seed,
        seed_args$use_seed
      )
    }
  }

  bg_new_game_result(out)
}

bg_match_heuristic_selection <- function(selection) {
  match.arg(selection, choices = c("aggressive", "defensive"))
}

#' Score a board with the aggressive heuristic
#'
#' Computes the aggressive archetype score for a board from a specified player's
#' perspective.
#'
#' The aggressive heuristic rewards:
#'
#' - putting opposing checkers on the bar;
#' - keeping or creating contact with opposing checkers;
#' - making blocking points;
#' - preserving some tactical pressure on opposing blots.
#'
#' It applies only a modest penalty to self-exposure, so it can prefer volatile
#' contact-heavy positions when they increase attacking chances.
#'
#' @param board A `bg_board` object.
#' @param player Optional integer-like scalar. If `NULL`, `board$turn` is used.
#'
#' @return A numeric scalar.
#' @export
#'
#' @examples
#' board <- bg_initial_board()
#' bg_score_board_aggressive(board)
bg_score_board_aggressive <- function(board, player = NULL) {
  if (!is_bg_board(board)) {
    stop("`board` must inherit from class 'bg_board'.", call. = FALSE)
  }

  bg_validate_board(board)
  if (is.null(player)) {
    player <- board$turn
  } else {
    player <- bg_coerce_integerish(player, "player", 1L)
  }

  if (!player %in% c(-1L, 1L)) {
    stop("`player` must be either 1L or -1L.", call. = FALSE)
  }

  bg_cpp_heuristic_board_score(unclass(board), player, "aggressive")
}

#' Score a board with the defensive heuristic
#'
#' Computes the defensive archetype score for a board from a specified player's
#' perspective.
#'
#' The defensive heuristic rewards:
#'
#' - minimizing exposed blots;
#' - making and preserving safe points;
#' - reducing direct hit risk;
#' - moving toward safer, lower-contact structures.
#'
#' @param board A `bg_board` object.
#' @param player Optional integer-like scalar. If `NULL`, `board$turn` is used.
#'
#' @return A numeric scalar.
#' @export
#'
#' @examples
#' board <- bg_initial_board()
#' bg_score_board_defensive(board)
bg_score_board_defensive <- function(board, player = NULL) {
  if (!is_bg_board(board)) {
    stop("`board` must inherit from class 'bg_board'.", call. = FALSE)
  }

  bg_validate_board(board)
  if (is.null(player)) {
    player <- board$turn
  } else {
    player <- bg_coerce_integerish(player, "player", 1L)
  }

  if (!player %in% c(-1L, 1L)) {
    stop("`player` must be either 1L or -1L.", call. = FALSE)
  }

  bg_cpp_heuristic_board_score(unclass(board), player, "defensive")
}

bg_choose_heuristic_move <- function(board, legal_moves, selection) {
  if (!is_bg_board(board)) {
    stop("`board` must inherit from class 'bg_board'.", call. = FALSE)
  }

  selection <- bg_match_heuristic_selection(selection)
  bg_validate_board(board)
  moves <- bg_normalize_move_sequence_list(legal_moves)

  bg_new_move_sequence(bg_cpp_heuristic_move_choice(unclass(board), moves, selection))
}

#' Choose a legal move with the aggressive archetype
#'
#' Scores the board resulting from each supplied legal move and returns the
#' highest-scoring move under the aggressive heuristic.
#'
#' @param board A `bg_board` object.
#' @param legal_moves A list of legal `bg_move_sequence` objects or a single
#'   `bg_move_sequence` object.
#'
#' @return A single `bg_move_sequence` object.
#' @export
#'
#' @examples
#' points <- integer(24)
#' points[8] <- 1L
#' points[5] <- -1L
#' board <- bg_board(points = points, off = c(14L, 14L), turn = 1L)
#' moves <- bg_legal_moves(board, bg_roll(3, 2))
#' bg_aggressive_move(board, moves)
bg_aggressive_move <- function(board, legal_moves) {
  bg_choose_heuristic_move(board, legal_moves, selection = "aggressive")
}

#' Choose a legal move with the defensive archetype
#'
#' Scores the board resulting from each supplied legal move and returns the
#' highest-scoring move under the defensive heuristic.
#'
#' @param board A `bg_board` object.
#' @param legal_moves A list of legal `bg_move_sequence` objects or a single
#'   `bg_move_sequence` object.
#'
#' @return A single `bg_move_sequence` object.
#' @export
#'
#' @examples
#' points <- integer(24)
#' points[c(8, 4, 2)] <- 1L
#' board <- bg_board(points = points, off = c(12L, 15L), turn = 1L)
#' moves <- bg_legal_moves(board, bg_roll(3, 2))
#' bg_defensive_move(board, moves)
bg_defensive_move <- function(board, legal_moves) {
  bg_choose_heuristic_move(board, legal_moves, selection = "defensive")
}

#' Play one turn with the aggressive archetype
#'
#' Convenience wrapper around [bg_play_turn()] using `selection = "aggressive"`.
#'
#' @param board A `bg_board` object.
#' @param roll Optional `bg_roll` object or roll-like list. If `NULL`, dice are
#'   rolled in C++.
#' @param seed Optional integer-like scalar. This controls dice rolling when
#'   `roll = NULL`.
#'
#' @return A `bg_turn_result` object.
#' @export
bg_play_turn_aggressive_player <- function(board, roll = NULL, seed = NULL) {
  bg_play_turn(board = board, roll = roll, selection = "aggressive", seed = seed)
}

#' Play one turn with the defensive archetype
#'
#' Convenience wrapper around [bg_play_turn()] using `selection = "defensive"`.
#'
#' @param board A `bg_board` object.
#' @param roll Optional `bg_roll` object or roll-like list. If `NULL`, dice are
#'   rolled in C++.
#' @param seed Optional integer-like scalar. This controls dice rolling when
#'   `roll = NULL`.
#'
#' @return A `bg_turn_result` object.
#' @export
bg_play_turn_defensive_player <- function(board, roll = NULL, seed = NULL) {
  bg_play_turn(board = board, roll = roll, selection = "defensive", seed = seed)
}

#' Simulate a game between two heuristic players
#'
#' Simulates a full game with independently chosen heuristic archetypes for
#' player 1 and player 2.
#'
#' @param board A `bg_board` object giving the initial position.
#' @param player1 Heuristic selection for player 1.
#' @param player2 Heuristic selection for player 2.
#' @param roll_sequence Optional list of rolls or a single `bg_roll` object.
#' @param max_turns Integer-like scalar giving the maximum number of turns to
#'   play.
#' @param seed Optional integer-like scalar. When `roll_sequence` is `NULL`,
#'   this controls reproducible dice rolling.
#'
#' @return A `bg_game_result` object.
#' @export
#'
#' @examples
#' points <- integer(24)
#' points[8] <- 1L
#' points[17] <- -1L
#' board <- bg_board(points = points, off = c(14L, 14L), turn = 1L)
#' game <- bg_play_game_heuristic_players(
#'   board = board,
#'   player1 = "aggressive",
#'   player2 = "defensive",
#'   roll_sequence = list(bg_roll(3, 2), bg_roll(3, 2)),
#'   max_turns = 2L,
#'   seed = 123
#' )
#' game$n_turns
bg_play_game_heuristic_players <- function(
    board = bg_initial_board(),
    player1 = c("aggressive", "defensive"),
    player2 = c("aggressive", "defensive"),
    roll_sequence = NULL,
    max_turns = 1000L,
    seed = NULL) {
  if (!is_bg_board(board)) {
    stop("`board` must inherit from class 'bg_board'.", call. = FALSE)
  }

  player1 <- bg_match_heuristic_selection(player1)
  player2 <- bg_match_heuristic_selection(player2)
  bg_validate_board(board)
  max_turns <- bg_coerce_integerish(max_turns, "max_turns", 1L)
  if (max_turns < 0L) {
    stop("`max_turns` must be nonnegative.", call. = FALSE)
  }

  if (is.null(seed)) {
    seed <- 0L
    use_seed <- FALSE
  } else {
    seed <- bg_coerce_integerish(seed, "seed", 1L)
    if (seed < 0L) {
      stop("`seed` must be nonnegative when supplied.", call. = FALSE)
    }
    use_seed <- TRUE
  }

  if (is.null(roll_sequence)) {
    out <- bg_cpp_play_game_matchup_random(
      unclass(board),
      max_turns,
      player1,
      player2,
      seed,
      use_seed
    )
  } else {
    rolls <- bg_normalize_roll_sequence(roll_sequence)
    out <- bg_cpp_play_game_matchup_scripted(
      unclass(board),
      rolls,
      max_turns,
      player1,
      player2,
      seed,
      use_seed
    )
  }

  bg_new_game_result(out)
}

#' Simulate a game between two aggressive players
#'
#' Convenience wrapper around [bg_play_game()] using `selection = "aggressive"`.
#'
#' @inheritParams bg_play_game
#'
#' @return A `bg_game_result` object.
#' @export
bg_play_game_aggressive_players <- function(
    board = bg_initial_board(),
    roll_sequence = NULL,
    max_turns = 1000L,
    seed = NULL) {
  bg_play_game(
    board = board,
    roll_sequence = roll_sequence,
    max_turns = max_turns,
    selection = "aggressive",
    seed = seed
  )
}

#' Simulate a game between two defensive players
#'
#' Convenience wrapper around [bg_play_game()] using `selection = "defensive"`.
#'
#' @inheritParams bg_play_game
#'
#' @return A `bg_game_result` object.
#' @export
bg_play_game_defensive_players <- function(
    board = bg_initial_board(),
    roll_sequence = NULL,
    max_turns = 1000L,
    seed = NULL) {
  bg_play_game(
    board = board,
    roll_sequence = roll_sequence,
    max_turns = max_turns,
    selection = "defensive",
    seed = seed
  )
}

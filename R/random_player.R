#' Choose a legal move uniformly at random
#'
#' Selects one move sequence uniformly at random from a supplied legal-move set.
#'
#' @param legal_moves A list of `bg_move_sequence` objects, such as the output of
#'   [bg_legal_moves()], or a single `bg_move_sequence` object.
#' @param seed Optional integer-like scalar for reproducible random choice.
#'
#' @return A single `bg_move_sequence` object.
#' @export
#'
#' @examples
#' points <- integer(24)
#' points[8] <- 1L
#' board <- bg_board(points = points, off = c(14L, 15L), turn = 1L)
#' legal_moves <- bg_legal_moves(board, bg_roll(3, 2))
#' bg_random_move(legal_moves, seed = 123)
bg_random_move <- function(legal_moves, seed = NULL) {
  moves <- bg_normalize_move_sequence_list(legal_moves)

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

  bg_new_move_sequence(bg_cpp_random_move_choice(moves, seed, use_seed))
}

#' Play one turn with the RandomPlayer archetype
#'
#' Convenience wrapper around [bg_play_turn()] using `selection = "random"`.
#'
#' @param board A `bg_board` object.
#' @param roll Optional `bg_roll` object or roll-like list. If `NULL`, dice are
#'   rolled in C++.
#' @param seed Optional integer-like scalar for reproducible dice rolling and
#'   random move choice.
#'
#' @return A `bg_turn_result` object.
#' @export
bg_play_turn_random_player <- function(board, roll = NULL, seed = NULL) {
  bg_play_turn(board = board, roll = roll, selection = "random", seed = seed)
}

#' Simulate a game between two random players
#'
#' Convenience wrapper around [bg_play_game()] using `selection = "random"` for
#' both players.
#'
#' @param board A `bg_board` object giving the initial position.
#' @param roll_sequence Optional list of rolls or a single `bg_roll` object.
#' @param max_turns Integer-like scalar giving the maximum number of turns to
#'   play.
#' @param seed Optional integer-like scalar for reproducible dice rolling and
#'   random move choice.
#'
#' @return A `bg_game_result` object.
#' @export
#'
#' @examples
#' points <- integer(24)
#' points[8] <- 1L
#' points[17] <- -1L
#' board <- bg_board(points = points, off = c(14L, 14L), turn = 1L)
#' game <- bg_play_game_random_players(
#'   board,
#'   roll_sequence = list(bg_roll(3, 2), bg_roll(3, 2)),
#'   max_turns = 2L,
#'   seed = 123
#' )
#' game$n_turns
bg_play_game_random_players <- function(
    board = bg_initial_board(),
    roll_sequence = NULL,
    max_turns = 1000L,
    seed = NULL) {
  bg_play_game(
    board = board,
    roll_sequence = roll_sequence,
    max_turns = max_turns,
    selection = "random",
    seed = seed
  )
}

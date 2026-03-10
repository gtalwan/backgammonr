bg_wrap_move_sequence_output <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  bg_new_move_sequence(x)
}

#' Generate all legal full-turn move sequences
#'
#' Enumerates every legal full-turn move sequence for a given board, acting
#' player, and dice roll.
#'
#' The generator currently handles:
#'
#' - normal movement;
#' - blocked points;
#' - hitting blots;
#' - entering from the bar;
#' - the requirement to enter from the bar before any other move;
#' - doubles;
#' - bearing off;
#' - forced-use rules:
#'   - use as many dice as legally possible;
#'   - if only one die can be played from a non-double roll, use the higher die
#'     when it is playable.
#'
#' The return value is a list of `bg_move_sequence` objects. If no legal move is
#' available, an empty list is returned.
#'
#' @param board A `bg_board` object.
#' @param roll A `bg_roll` object or roll-like list.
#' @param player Optional integer-like scalar giving the acting player. If
#'   `NULL`, the value is taken from `board$turn`.
#'
#' @return A list of `bg_move_sequence` objects.
#' @export
#'
#' @examples
#' points <- integer(24)
#' points[8] <- 1L
#' board <- bg_board(points = points, off = c(14, 15), turn = 1L)
#' roll <- bg_roll(3, 2)
#' moves <- bg_legal_moves(board, roll)
#' length(moves)
#' moves[[1]]
bg_legal_moves <- function(board, roll, player = NULL) {
  if (!is_bg_board(board)) {
    stop("`board` must inherit from class 'bg_board'.", call. = FALSE)
  }

  bg_validate_board(board)
  roll <- bg_as_roll(roll)

  if (is.null(player)) {
    player <- board$turn
  } else {
    player <- bg_coerce_integerish(player, "player", 1L)
  }

  if (!player %in% c(-1L, 1L)) {
    stop("`player` must be either 1L or -1L.", call. = FALSE)
  }

  out <- bg_cpp_legal_moves(unclass(board), player, unclass(roll))
  lapply(out, bg_wrap_move_sequence_output)
}

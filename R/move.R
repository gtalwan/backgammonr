bg_new_move_step <- function(x) {
  structure(x, class = "bg_move_step")
}

bg_new_move_sequence <- function(x) {
  x$steps <- lapply(x$steps, bg_new_move_step)
  x$roll <- bg_wrap_roll_output(x$roll)
  structure(x, class = "bg_move_sequence")
}

bg_unclass_move_step <- function(step) {
  if (is_bg_move_step(step)) {
    return(unclass(step))
  }

  if (is.list(step)) {
    return(unclass(bg_as_move_step(step)))
  }

  stop("Each step must be a `bg_move_step` object or a list-like step representation.", call. = FALSE)
}

bg_as_move_step <- function(step) {
  if (is_bg_move_step(step)) {
    return(step)
  }

  if (!is.list(step)) {
    stop("Each step must be a `bg_move_step` object or a list-like step representation.", call. = FALSE)
  }

  required <- c("from", "to", "die")
  missing_fields <- required[!required %in% names(step)]
  if (length(missing_fields) > 0L) {
    stop(
      sprintf(
        "A step-like list is missing required field(s): %s.",
        paste(missing_fields, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  hit <- if (is.null(step$hit)) FALSE else step$hit
  bg_move_step(step$from, step$to, step$die, hit = hit)
}

bg_normalize_move_sequence_list <- function(legal_moves) {
  if (is_bg_move_sequence(legal_moves)) {
    return(list(bg_unclass_move_sequence(legal_moves)))
  }

  if (!is.list(legal_moves)) {
    stop(
      "`legal_moves` must be a list of `bg_move_sequence` objects or a single `bg_move_sequence` object.",
      call. = FALSE
    )
  }

  lapply(legal_moves, bg_unclass_move_sequence)
}

bg_unclass_move_sequence <- function(sequence) {
  if (is_bg_move_sequence(sequence)) {
    return(unclass(sequence))
  }

  if (is.list(sequence)) {
    return(unclass(bg_as_move_sequence(sequence)))
  }

  stop(
    "Each move sequence must be a `bg_move_sequence` object or a list-like move-sequence representation.",
    call. = FALSE
  )
}

bg_as_move_sequence <- function(sequence) {
  if (is_bg_move_sequence(sequence)) {
    return(sequence)
  }

  if (!is.list(sequence)) {
    stop(
      "Each move sequence must be a `bg_move_sequence` object or a list-like move-sequence representation.",
      call. = FALSE
    )
  }

  required <- c("player", "steps")
  missing_fields <- required[!required %in% names(sequence)]
  if (length(missing_fields) > 0L) {
    stop(
      sprintf(
        "A move-sequence-like list is missing required field(s): %s.",
        paste(missing_fields, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  roll <- if (is.null(sequence$roll)) NULL else sequence$roll
  bg_move_sequence(
    player = sequence$player,
    steps = sequence$steps,
    roll = roll
  )
}

#' Test whether an object is an atomic backgammon move step
#'
#' @param x An object.
#'
#' @return `TRUE` if `x` inherits from class `"bg_move_step"`; otherwise
#'   `FALSE`.
#' @export
is_bg_move_step <- function(x) {
  inherits(x, "bg_move_step")
}

#' Test whether an object is a backgammon move sequence
#'
#' @param x An object.
#'
#' @return `TRUE` if `x` inherits from class `"bg_move_sequence"`; otherwise
#'   `FALSE`.
#' @export
is_bg_move_sequence <- function(x) {
  inherits(x, "bg_move_sequence")
}

#' Construct a single move step
#'
#' Creates one atomic checker movement. This is a structural representation only;
#' no legal move generation is performed at this stage.
#'
#' Coordinate conventions are:
#'
#' - `from = 0` means the checker enters from the bar;
#' - `to = 25` means the checker is borne off;
#' - board points use absolute coordinates `1:24`.
#'
#' @param from Integer-like scalar in `0:24`.
#' @param to Integer-like scalar in `1:25`.
#' @param die Integer-like scalar in `1:6`.
#' @param hit Logical scalar indicating whether the move hits an opposing blot.
#'
#' @return An object of class `bg_move_step` with fields `from`, `to`, `die`,
#'   and `hit`.
#' @export
#'
#' @examples
#' step <- bg_move_step(from = 24, to = 21, die = 3)
#' step
#' unclass(step)
bg_move_step <- function(from, to, die, hit = FALSE) {
  from <- bg_coerce_integerish(from, "from", 1L)
  to <- bg_coerce_integerish(to, "to", 1L)
  die <- bg_coerce_integerish(die, "die", 1L)
  bg_assert_scalar_flag(hit, "hit")

  bg_new_move_step(bg_cpp_move_step_create(from, to, die, hit))
}

#' Construct a full-turn move sequence
#'
#' Creates an ordered sequence of atomic move steps for one player's turn. This
#' is a structural representation only; it does not yet assert that the sequence
#' is legal on a particular board.
#'
#' A move sequence must contain at least one step. Use an empty legal-move list,
#' not an empty move sequence, to represent a forced pass.
#'
#' If a `roll` is supplied, the sequence is checked for internal consistency with
#' that roll. In particular, a die value cannot be used more times than it
#' appears in the roll's explicit `expanded` representation, which handles
#' doubles directly.
#'
#' @param player Integer-like scalar indicating the acting player. Must be `1`
#'   or `-1`.
#' @param steps A list of `bg_move_step` objects or step-like lists.
#' @param roll Optional `bg_roll` object or roll-like list.
#'
#' @return An object of class `bg_move_sequence` with fields `player`, `roll`,
#'   `steps`, `dice_used`, and `n_steps`.
#' @export
#'
#' @examples
#' roll <- bg_roll(3, 3)
#' seqn <- bg_move_sequence(
#'   player = 1,
#'   roll = roll,
#'   steps = list(
#'     bg_move_step(24, 21, 3),
#'     bg_move_step(24, 21, 3)
#'   )
#' )
#' seqn
#' unclass(seqn)
bg_move_sequence <- function(player, steps = list(), roll = NULL) {
  player <- bg_coerce_integerish(player, "player", 1L)
  if (!player %in% c(-1L, 1L)) {
    stop("`player` must be either 1L or -1L.", call. = FALSE)
  }

  if (!is.list(steps)) {
    stop("`steps` must be a list of move-step objects or step-like lists.", call. = FALSE)
  }

  steps_in <- lapply(steps, bg_unclass_move_step)

  out <- if (is.null(roll)) {
    bg_cpp_move_sequence_create(player, steps_in)
  } else {
    bg_cpp_move_sequence_create_with_roll(player, steps_in, unclass(bg_as_roll(roll)))
  }

  bg_new_move_sequence(out)
}

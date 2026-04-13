# Backgammon engine-facing R API.
#
# This file keeps the board/roll/move/game/simulation/player helpers together
# so the game engine surface is readable without hopping across many files.

# -----------------------------------------------------------------------------
# Source: board.R
# -----------------------------------------------------------------------------
# Board constructors, validators, and board-level utilities.
bg_known_board_fields <- function() {
  c("points", "bar", "off", "turn")
}

bg_assert_scalar_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be TRUE or FALSE.", name), call. = FALSE)
  }
  invisible(x)
}

bg_coerce_integerish <- function(x, name, expected_length) {
  if (!is.atomic(x)) {
    stop(sprintf("`%s` must be an atomic vector.", name), call. = FALSE)
  }

  if (length(x) != expected_length) {
    stop(
      sprintf("`%s` must have length %d.", name, expected_length),
      call. = FALSE
    )
  }

  if (!is.numeric(x) && !is.integer(x)) {
    stop(sprintf("`%s` must be numeric or integer.", name), call. = FALSE)
  }

  if (anyNA(x)) {
    stop(sprintf("`%s` cannot contain `NA` values.", name), call. = FALSE)
  }

  if (any(!is.finite(x))) {
    stop(sprintf("`%s` must contain only finite values.", name), call. = FALSE)
  }

  if (any(x != trunc(x))) {
    stop(sprintf("`%s` must contain whole numbers only.", name), call. = FALSE)
  }

  as.integer(x)
}

bg_normalize_board_list <- function(x) {
  extras <- names(x)[!names(x) %in% bg_known_board_fields()]
  out <- list(
    points = x$points,
    bar = x$bar,
    off = x$off,
    turn = x$turn
  )

  for (nm in extras) {
    out[[nm]] <- x[[nm]]
  }

  out
}

bg_new_board <- function(x) {
  structure(bg_normalize_board_list(x), class = "bg_board")
}

bg_unclass_board <- function(board) {
  if (is_bg_board(board)) {
    return(unclass(board))
  }

  if (is.list(board)) {
    return(board)
  }

  stop(
    "`board` must be a `bg_board` object or a list-like board representation.",
    call. = FALSE
  )
}

#' Construct a backgammon board
#'
#' Creates a `bg_board` object from its core components.
#'
#' The board uses a signed point representation over absolute coordinates
#' `1:24`, where positive counts belong to player 1 and negative counts belong
#' to player 2. `bar` and `off` store counts for players 1 and 2 in that order.
#'
#' This constructor accepts numeric whole numbers and normalizes them to integer
#' storage before validation. It is the recommended way to create custom board
#' positions for tests and later engine components.
#'
#' @param points Numeric or integer vector of length 24.
#' @param bar Numeric or integer vector of length 2.
#' @param off Numeric or integer vector of length 2.
#' @param turn Integer-like scalar indicating the player to move. Must be `1`
#'   or `-1`.
#' @param validate Logical scalar. If `TRUE`, validate the board before
#'   returning it.
#'
#' @return An object of class `bg_board`.
#' @export
bg_board <- function(points, bar = c(0, 0), off = c(0, 0), turn = 1L, validate = TRUE) {
  bg_assert_scalar_flag(validate, "validate")

  board <- bg_new_board(list(
    points = bg_coerce_integerish(points, "points", 24L),
    bar = bg_coerce_integerish(bar, "bar", 2L),
    off = bg_coerce_integerish(off, "off", 2L),
    turn = bg_coerce_integerish(turn, "turn", 1L)
  ))

  if (isTRUE(validate)) {
    bg_validate_board(board)
  }

  board
}

#' Test whether an object is a backgammon board
#'
#' @param x An object.
#'
#' @return `TRUE` if `x` inherits from class `"bg_board"`; otherwise `FALSE`.
#' @export
is_bg_board <- function(x) {
  inherits(x, "bg_board")
}

#' Create the standard initial backgammon board
#'
#' Constructs the standard opening position using the package's signed-point
#' representation.
#'
#' Positive point counts belong to player 1. Negative point counts belong to
#' player 2. The `points` vector uses absolute board coordinates `1:24`, where
#' `points[1]` is board point 1 and `points[24]` is board point 24.
#'
#' @param turn Integer scalar indicating the player to move. Must be `1L` or
#'   `-1L`.
#'
#' @return A validated object of class `bg_board`.
#' @export
bg_initial_board <- function(turn = 1L) {
  turn <- bg_coerce_integerish(turn, "turn", 1L)

  if (!turn %in% c(-1L, 1L)) {
    stop("`turn` must be either 1L or -1L.", call. = FALSE)
  }

  board <- bg_new_board(bg_cpp_board_initial(turn))
  bg_validate_board(board)
  board
}

#' Clone a backgammon board
#'
#' Creates a normalized copy of a board object through the compiled C++ layer.
#' Known board fields are normalized, and any additional list elements are
#' preserved so the representation can be extended later.
#'
#' @param board A `bg_board` object.
#'
#' @return A new `bg_board` object.
#' @export
bg_clone_board <- function(board) {
  if (!is_bg_board(board)) {
    stop("`board` must inherit from class 'bg_board'.", call. = FALSE)
  }

  bg_new_board(bg_cpp_board_clone(unclass(board)))
}

#' Validate a backgammon board
#'
#' Validates structural consistency for a board representation. Validation is
#' performed in C++ so the same rules can be reused by later engine components.
#'
#' The current checks cover:
#'
#' - presence of `points`, `bar`, `off`, and `turn` fields;
#' - integer storage, expected lengths, and absence of `NA` values;
#' - nonnegative `bar` and `off` counts;
#' - `turn` being either `1L` or `-1L`;
#' - each player having exactly 15 total checkers across board, bar, and borne
#'   off;
#' - rejecting impossible states in which both players have already borne off
#'   all 15 checkers.
#'
#' Extra fields are currently preserved but ignored by the validator.
#'
#' @param board A `bg_board` object or list-like board representation.
#' @param error Logical scalar. If `TRUE`, invalid boards raise an error. If
#'   `FALSE`, a validation report is returned.
#'
#' @return If `error = TRUE`, returns `TRUE` invisibly for a valid board.
#'   Otherwise returns a named list with elements `valid` and `messages`.
#' @export
bg_validate_board <- function(board, error = TRUE) {
  bg_assert_scalar_flag(error, "error")

  if (!is_bg_board(board) && !is.list(board)) {
    message <- "`board` must be a `bg_board` object or a list-like board representation."
    if (isTRUE(error)) {
      stop(message, call. = FALSE)
    }
    return(list(valid = FALSE, messages = message))
  }

  report <- bg_cpp_board_validate(bg_unclass_board(board))

  if (isTRUE(report$valid)) {
    if (isTRUE(error)) {
      return(invisible(TRUE))
    }
    return(report)
  }

  if (isTRUE(error)) {
    stop(paste(report$messages, collapse = "\n"), call. = FALSE)
  }

  report
}

#' Inspect a backgammon board
#'
#' Returns a compact R summary of a board suitable for debugging and tests.
#'
#' @param board A `bg_board` object.
#'
#' @return A named list with elements `turn`, `bar`, `off`, `points`, and
#'   `extra_fields`.
#' @export
bg_inspect_board <- function(board) {
  if (!is_bg_board(board)) {
    stop("`board` must inherit from class 'bg_board'.", call. = FALSE)
  }

  bg_validate_board(board)

  points_df <- data.frame(
    point = seq_len(24L),
    signed_count = board$points,
    owner = ifelse(
      board$points > 0L,
      "player_1",
      ifelse(board$points < 0L, "player_2", "none")
    ),
    n_checkers = abs(board$points),
    stringsAsFactors = FALSE
  )

  list(
    turn = if (board$turn == 1L) "player_1" else "player_2",
    bar = stats::setNames(as.integer(board$bar), c("player_1", "player_2")),
    off = stats::setNames(as.integer(board$off), c("player_1", "player_2")),
    points = points_df,
    extra_fields = names(board)[!names(board) %in% bg_known_board_fields()]
  )
}

bg_point_token <- function(value) {
  if (value > 0L) {
    return(paste0("X", value))
  }
  if (value < 0L) {
    return(paste0("O", abs(value)))
  }
  "."
}

#' Print a human-readable ASCII board diagram
#'
#' Renders a board as two lanes (`24 -> 13` and `12 -> 1`) so it is easier to
#' visually inspect positions during interactive analysis.
#'
#' Tokens:
#' - `Xn`: `n` checkers for player 1 on that point;
#' - `On`: `n` checkers for player 2 on that point;
#' - `.`: empty point.
#'
#' @param board A `bg_board` object.
#' @param show_indices Logical scalar. If `TRUE`, print point indices above each
#'   lane.
#'
#' @return The input `board`, invisibly.
#' @export
#'
#' @examples
#' b <- bg_initial_board()
#' bg_print_board(b)
bg_print_board <- function(board, show_indices = TRUE) {
  if (!is_bg_board(board)) {
    stop("`board` must inherit from class 'bg_board'.", call. = FALSE)
  }
  bg_assert_scalar_flag(show_indices, "show_indices")
  if (exists("format.bg_board", mode = "function")) {
    cat(format.bg_board(board, show_indices = show_indices), sep = "\n")
  } else {
    bg_validate_board(board)
    top_idx <- 24:13
    bottom_idx <- 12:1
    top_tokens <- vapply(top_idx, function(i) bg_point_token(board$points[[i]]), character(1L))
    bottom_tokens <- vapply(bottom_idx, function(i) bg_point_token(board$points[[i]]), character(1L))

    cat("<bg_board_ascii>\n", sep = "")
    cat("turn: ", if (board$turn == 1L) "player_1" else "player_2", "\n", sep = "")
    cat("bar:  p1=", board$bar[[1L]], " p2=", board$bar[[2L]], "\n", sep = "")
    cat("off:  p1=", board$off[[1L]], " p2=", board$off[[2L]], "\n", sep = "")
    cat("\n", sep = "")
    cat("Top (24 -> 13)\n", sep = "")
    if (isTRUE(show_indices)) {
      cat(paste(sprintf("%4d", top_idx), collapse = ""), "\n", sep = "")
    }
    cat(paste(sprintf("%4s", top_tokens), collapse = ""), "\n", sep = "")
    cat("Bottom (12 -> 1)\n", sep = "")
    if (isTRUE(show_indices)) {
      cat(paste(sprintf("%4d", bottom_idx), collapse = ""), "\n", sep = "")
    }
    cat(paste(sprintf("%4s", bottom_tokens), collapse = ""), "\n", sep = "")
  }

  invisible(board)
}

# -----------------------------------------------------------------------------
# Source: dice.R
# -----------------------------------------------------------------------------
# Dice and roll constructors plus reproducible roll helpers.
bg_new_roll <- function(x) {
  structure(x, class = "bg_roll")
}

bg_unclass_roll <- function(roll) {
  if (is_bg_roll(roll)) {
    return(unclass(roll))
  }

  if (is.list(roll)) {
    return(roll)
  }

  stop("`roll` must be a `bg_roll` object or a list-like roll representation.", call. = FALSE)
}

bg_wrap_roll_output <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  bg_new_roll(x)
}

bg_as_roll <- function(roll) {
  if (is_bg_roll(roll)) {
    return(roll)
  }

  if (!is.list(roll)) {
    stop("`roll` must be a `bg_roll` object or a list-like roll representation.", call. = FALSE)
  }

  if (is.null(roll$dice)) {
    stop("A roll-like list must contain a `dice` field.", call. = FALSE)
  }

  dice <- bg_coerce_integerish(roll$dice, "roll$dice", 2L)
  bg_roll(dice[1L], dice[2L])
}

#' Test whether an object is a backgammon roll
#'
#' @param x An object.
#'
#' @return `TRUE` if `x` inherits from class `"bg_roll"`; otherwise `FALSE`.
#' @export
is_bg_roll <- function(x) {
  inherits(x, "bg_roll")
}

#' Construct a backgammon dice roll
#'
#' Creates a normalized `bg_roll` object from two die values.
#'
#' A roll stores both the raw two-die outcome and an explicit `expanded`
#' representation. Non-double rolls expand to two values; doubles expand to four
#' repeated values.
#'
#' @param die1 Integer-like scalar in `1:6`.
#' @param die2 Integer-like scalar in `1:6`.
#'
#' @return An object of class `bg_roll` with fields `dice`, `is_double`, and
#'   `expanded`.
#' @export
#'
#' @examples
#' roll <- bg_roll(3, 3)
#' roll
#' unclass(roll)
bg_roll <- function(die1, die2) {
  die1 <- bg_coerce_integerish(die1, "die1", 1L)
  die2 <- bg_coerce_integerish(die2, "die2", 1L)

  bg_new_roll(bg_cpp_roll_create(die1, die2))
}

#' Roll dice in C++
#'
#' Draws one or more independent dice rolls using the compiled core.
#'
#' If `n = 1`, a single `bg_roll` object is returned. Otherwise a list of
#' `bg_roll` objects is returned in draw order.
#'
#' @param n Integer-like scalar giving the number of rolls to draw.
#' @param seed Optional integer-like scalar. If supplied, the C++ random number
#'   generator is seeded explicitly for reproducible output.
#'
#' @return A `bg_roll` object if `n = 1`, otherwise a list of `bg_roll` objects.
#' @export
#'
#' @examples
#' bg_roll_dice(seed = 123)
#' bg_roll_dice(n = 3, seed = 123)
bg_roll_dice <- function(n = 1L, seed = NULL) {
  n <- bg_coerce_integerish(n, "n", 1L)
  if (n < 1L) {
    stop("`n` must be at least 1.", call. = FALSE)
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

  out <- bg_cpp_roll_dice(n, seed, use_seed)
  out <- lapply(out, bg_new_roll)

  if (n == 1L) {
    return(out[[1L]])
  }

  out
}

# -----------------------------------------------------------------------------
# Source: move.R
# -----------------------------------------------------------------------------
# Move-step and move-sequence constructors plus helpers.
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

# -----------------------------------------------------------------------------
# Source: legal_moves.R
# -----------------------------------------------------------------------------
# Legal-move generation wrappers around the C++ move generator.
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

# -----------------------------------------------------------------------------
# Source: game.R
# -----------------------------------------------------------------------------
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
#' @keywords internal
#' @noRd
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

# -----------------------------------------------------------------------------
# Source: simulation.R
# -----------------------------------------------------------------------------
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
#' @keywords internal
#' @noRd
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

# -----------------------------------------------------------------------------
# Source: random_player.R
# -----------------------------------------------------------------------------
# Random-player wrappers used by game simulation and compatibility APIs.
#' Choose a legal move uniformly at random
#'
#' Selects one move sequence uniformly at random from a supplied legal-move set.
#'
#' @param legal_moves A list of `bg_move_sequence` objects, such as the output of
#'   [bg_legal_moves()], or a single `bg_move_sequence` object.
#' @param seed Optional integer-like scalar for reproducible random choice.
#'
#' @return A single `bg_move_sequence` object.
#' @keywords internal
#' @noRd
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
#' @keywords internal
#' @noRd
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
#' @keywords internal
#' @noRd
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

# -----------------------------------------------------------------------------
# Source: heuristic_player.R
# -----------------------------------------------------------------------------
# Heuristic move-choice utilities and heuristic-player game wrappers.
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
#' @keywords internal
#' @noRd
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
#' @keywords internal
#' @noRd
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
#' @keywords internal
#' @noRd
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
#' @keywords internal
#' @noRd
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
#' @keywords internal
#' @noRd
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
#' @keywords internal
#' @noRd
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
#' @keywords internal
#' @noRd
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
#' @keywords internal
#' @noRd
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
#' @keywords internal
#' @noRd
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

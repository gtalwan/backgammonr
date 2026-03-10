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

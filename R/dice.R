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

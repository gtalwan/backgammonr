# Shared internal helpers used by the focused TS/truth workflows.

bg_derive_seed <- function(seed, ...) {
  if (is.null(seed)) {
    return(NULL)
  }

  seed <- bg_coerce_integerish(seed, "seed", 1L)
  if (seed < 0L) {
    stop("`seed` must be nonnegative when supplied.", call. = FALSE)
  }

  key <- paste(..., collapse = "::")
  if (!nzchar(key)) {
    return(seed)
  }

  ints <- utf8ToInt(key)
  if (length(ints) == 0L) {
    return(seed)
  }

  weights <- seq_along(ints) + 17L
  hashed <- sum(as.numeric(weights) * as.numeric(ints)) %% 2147483647
  as.integer((as.numeric(seed) + hashed) %% 2147483647)
}

bg_step_label <- function(step) {
  from <- if (step$from == 0L) "bar" else as.character(step$from)
  to <- if (step$to == 25L) "off" else as.character(step$to)
  paste0(from, "->", to, if (isTRUE(step$hit)) "*" else "")
}

bg_move_label <- function(move) {
  if (is.null(move)) {
    return("<pass>")
  }

  move <- bg_as_move_sequence(move)
  paste(vapply(move$steps, bg_step_label, character(1L)), collapse = ", ")
}

bg_normalize_study_budgets <- function(budgets, arg_name = "budgets") {
  if (length(budgets) < 1L) {
    stop(sprintf("`%s` must be non-empty.", arg_name), call. = FALSE)
  }

  budgets <- bg_coerce_integerish(budgets, arg_name, length(budgets))
  if (any(budgets < 1L)) {
    stop(sprintf("All values in `%s` must be at least 1.", arg_name), call. = FALSE)
  }

  as.integer(unique(budgets))
}

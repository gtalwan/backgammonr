# Print helpers for legacy benchmark, tradeoff, and variance-control objects.
#' Print a matchup benchmark
#'
#' @param x A `bg_matchup_benchmark` object.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
print.bg_matchup_benchmark <- function(x, ...) {
  if (!is_bg_matchup_benchmark(x)) {
    stop("`x` must inherit from class 'bg_matchup_benchmark'.", call. = FALSE)
  }

  cat("<bg_matchup_benchmark>\n", sep = "")
  cat("player_1: ", x$settings$player1_selection, "\n", sep = "")
  cat("player_2: ", x$settings$player2_selection, "\n", sep = "")
  cat("n_games:  ", x$settings$n_games, "\n", sep = "")
  cat("runtime:  ", format(x$settings$runtime_seconds, digits = 6), " seconds\n", sep = "")

  if (!is.null(x$summary)) {
    print(x$summary, row.names = FALSE)
  }

  invisible(x)
}

#' Print a move-evaluation benchmark
#'
#' @param x A `bg_move_evaluation_benchmark` object.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
print.bg_move_evaluation_benchmark <- function(x, ...) {
  if (!is_bg_move_evaluation_benchmark(x)) {
    stop("`x` must inherit from class 'bg_move_evaluation_benchmark'.", call. = FALSE)
  }

  cat("<bg_move_evaluation_benchmark>\n", sep = "")
  cat("methods: ", paste(x$settings$methods, collapse = ", "), "\n", sep = "")
  cat(
    "reference_method: ",
    if (is.null(x$settings$reference_method)) "<none>" else x$settings$reference_method,
    "\n",
    sep = ""
  )
  cat("rollout_budget: ", x$settings$rollout_budget, "\n", sep = "")

  print(x$summary, row.names = FALSE)
  invisible(x)
}

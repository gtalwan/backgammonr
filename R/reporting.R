# Legacy reporting helpers for benchmark and study objects.
bg_new_analysis_report <- function(x) {
  x$board <- bg_new_board(x$board)
  x$roll <- bg_new_roll(x$roll)
  x$ranking <- as.data.frame(x$ranking, stringsAsFactors = FALSE)
  if (!is.null(x$evaluation) && inherits(x$evaluation, "bg_action_evaluation")) {
    x$evaluation <- bg_new_action_evaluation(unclass(x$evaluation))
  }
  if (!is.null(x$recommendation) && inherits(x$recommendation, "bg_move_recommendation")) {
    x$recommendation <- bg_new_move_recommendation(unclass(x$recommendation))
  }
  structure(x, class = "bg_analysis_report")
}

#' Build a compact analysis report for a board and roll
#'
#' Runs move ranking and recommendation under a selected allocation method and
#' returns a structured report object with ranking, uncertainty, explanation,
#' and optional allocation trace.
#'
#' @param board A `bg_board` object.
#' @param roll A `bg_roll` object.
#' @param method Allocation method.
#' @param total_budget Integer-like rollout budget.
#' @param trace Logical scalar; if `TRUE`, include allocation trace.
#' @param trace_every Trace checkpoint spacing.
#' @inheritParams evaluate_actions_equal
#'
#' @return An object of class `bg_analysis_report`.
bg_analysis_report <- function(
    board,
    roll,
    method = c("thompson", "ocba", "equal", "greedy", "ucb"),
    total_budget = 32L,
    trace = TRUE,
    trace_every = 1L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    unresolved_value = 0.5,
    initial_allocations = 1L,
    ucb_exploration = 1,
    prior_alpha = 1,
    prior_beta = 1,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    seed = NULL) {
  method <- bg_match_allocation_method(method)
  bg_assert_scalar_flag(trace, "trace")

  recommendation <- bg_recommend_move(
    board = board,
    roll = roll,
    method = method,
    total_budget = total_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns,
    unresolved_value = unresolved_value,
    initial_allocations = initial_allocations,
    ucb_exploration = ucb_exploration,
    prior_alpha = prior_alpha,
    prior_beta = prior_beta,
    dice_mode = dice_mode,
    crn = crn,
    trace = trace,
    trace_every = trace_every,
    seed = seed
  )

  evaluation <- recommendation$evaluation
  ranking <- recommendation$ranking

  bg_new_analysis_report(list(
    board = unclass(recommendation$board),
    roll = unclass(recommendation$roll),
    method = recommendation$method,
    settings = recommendation$settings,
    ranking = ranking,
    recommended_index = recommendation$recommended_index,
    recommended_move = recommendation$recommended_move,
    explanation = recommendation$explanation,
    recommendation = recommendation,
    evaluation = evaluation,
    trace = if (!is.null(evaluation)) evaluation$trace else NULL
  ))
}

#' Print an analysis report
#'
#' @param x A `bg_analysis_report` object.
#' @param n Integer-like scalar controlling how many ranked actions to print.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
print.bg_analysis_report <- function(x, n = 10L, ...) {
  cat("<bg_analysis_report>\n", sep = "")
  cat("method:       ", x$method, "\n", sep = "")
  cat("total_budget: ", x$settings$total_budget, "\n", sep = "")
  if (!is.null(x$settings$dice_mode)) {
    cat("dice_mode:    ", x$settings$dice_mode, "\n", sep = "")
  }
  if (!is.null(x$settings$crn)) {
    cat("crn:          ", x$settings$crn, "\n", sep = "")
  }
  cat("recommendation: ", if (is.null(x$recommended_move)) "<pass>" else bg_move_label(x$recommended_move), "\n", sep = "")
  cat("summary: ", x$explanation, "\n", sep = "")
  if (nrow(x$ranking) > 0L) {
    compact <- bg_compact_action_table(x$ranking, n = n)
    print(compact, row.names = FALSE)
    if (nrow(x$ranking) > nrow(compact)) {
      cat("showing_first: ", nrow(compact), " of ", nrow(x$ranking), " candidates\n", sep = "")
    }
  }
  invisible(x)
}

#' Plot an analysis report
#'
#' Draws board, ranking uncertainty, and trace (if present) in a compact
#' diagnostic layout.
#'
#' @param x A `bg_analysis_report` object.
#' @param ... Unused.
#'
#' @return The input report, invisibly.
plot.bg_analysis_report <- function(x, ...) {
  if (!inherits(x, "bg_analysis_report")) {
    stop("`x` must inherit from class 'bg_analysis_report'.", call. = FALSE)
  }

  has_trace <- !is.null(x$trace) && nrow(x$trace) > 0L
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  if (has_trace) {
    graphics::par(mfrow = c(1, 3), mar = c(2.3, 1, 2.5, 1))
  } else {
    graphics::par(mfrow = c(1, 2), mar = c(2.3, 1, 2.5, 1))
  }

  plot.bg_board(x$board, main = "Position")
  bg_plot_move_ranking(x$ranking, main = "Ranked moves")
  if (has_trace) {
    bg_plot_allocation_trace(x$evaluation, metric = "allocation_count")
  }

  invisible(x)
}

bg_new_move_recommendation <- function(x) {
  x$board <- bg_new_board(x$board)
  x$roll <- bg_new_roll(x$roll)
  x$recommended_move <- if (is.null(x$recommended_move)) NULL else bg_new_move_sequence(x$recommended_move)
  x$ranking <- as.data.frame(x$ranking, stringsAsFactors = FALSE)
  if (!is.null(x$evaluation) && inherits(x$evaluation, "bg_action_evaluation")) {
    x$evaluation <- bg_new_action_evaluation(unclass(x$evaluation))
  }
  structure(x, class = "bg_move_recommendation")
}

bg_make_recommendation_summary <- function(ranking, method, total_budget) {
  if (nrow(ranking) == 0L) {
    return("No legal moves are available for this roll, so the player must pass.")
  }

  best <- ranking[ranking$recommended, , drop = FALSE]
  if (nrow(best) != 1L) {
    best <- ranking[1L, , drop = FALSE]
  }

  runner_up <- ranking[ranking$candidate_index != best$candidate_index, , drop = FALSE]
  gap <- if (nrow(runner_up) > 0L) {
    best$estimate[[1L]] - max(runner_up$estimate)
  } else {
    NA_real_
  }

  paste0(
    "Recommended by the ", method, " allocation rule using a total rollout budget of ",
    total_budget, ". The selected move is `", best$move_label[[1L]], "` with estimated win probability ",
    formatC(best$estimate[[1L]], digits = 3, format = "f"),
    " and approximate 95% interval [",
    formatC(best$lower_95[[1L]], digits = 3, format = "f"),
    ", ",
    formatC(best$upper_95[[1L]], digits = 3, format = "f"),
    "].",
    if (!is.na(gap)) {
      paste0(" The estimated margin over the next-best move is ", formatC(gap, digits = 3, format = "f"), ".")
    } else {
      ""
    },
    " The ranking is based on Monte Carlo rollouts: simulated continuations from each candidate move under the baseline policy."
  )
}

bg_format_move_ranking <- function(ranking) {
  ranking <- as.data.frame(ranking, stringsAsFactors = FALSE)
  if (nrow(ranking) == 0L) {
    return(ranking)
  }

  if ("selection_score" %in% names(ranking) && !"method_score" %in% names(ranking)) {
    ranking$method_score <- ranking$selection_score
  }

  preferred <- c(
    "rank",
    "recommended",
    "candidate_index",
    "move_label",
    "n_steps",
    "n_equivalent_sequences",
    "allocation_count",
    "wins",
    "losses",
    "unresolved",
    "empirical_value",
    "estimate",
    "posterior_sd",
    "lower_95",
    "upper_95",
    "prob_best",
    "posterior_expected_regret",
    "method_score",
    "move"
  )

  ranking[, intersect(preferred, names(ranking)), drop = FALSE]
}

#' Construct a backgammon position
#'
#' User-friendly alias for [bg_board()]. It is intended for gamer-facing and
#' research-facing workflows where a board state is supplied directly before
#' generating legal moves or move recommendations.
#'
#' @inheritParams bg_board
#'
#' @return A validated `bg_board` object.
#' @export
bg_position <- function(points, bar = c(0, 0), off = c(0, 0), turn = 1L, validate = TRUE) {
  bg_board(points = points, bar = bar, off = off, turn = turn, validate = validate)
}

#' Rank legal moves for a board and roll
#'
#' Generates legal moves, evaluates them under a fixed rollout budget, and
#' returns a ranked data frame of move recommendations with estimated values and
#' uncertainty summaries.
#'
#' @param board A `bg_board` object.
#' @param roll A `bg_roll` object.
#' @param method Allocation rule used for the evaluation. Supported values are
#'   `"equal"`, `"greedy"`, `"ucb"`, `"thompson"`, `"ttts"`, and `"ocba"`.
#' @param total_budget Integer-like scalar giving the fixed rollout budget.
#' @inheritParams evaluate_actions_equal
#'
#' @return A data frame sorted from best to worst with one row per legal move.
#'   The data frame includes a `move` list-column of `bg_move_sequence` objects.
#' @export
#'
#' @examples
#' points <- integer(24)
#' points[8] <- 1L
#' points[17] <- -1L
#' board <- bg_position(points = points, off = c(14L, 14L), turn = 1L)
#' bg_rank_moves(board, bg_roll(3, 2), method = "thompson", total_budget = 8L, seed = 123)
bg_rank_moves <- function(
    board,
    roll,
    method = c("thompson", "ttts", "ocba", "equal", "greedy", "ucb"),
    total_budget = 32L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    unresolved_value = 0.5,
    initial_allocations = 1L,
    ucb_exploration = 1,
    prior_alpha = 1,
    prior_beta = 1,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    trace = FALSE,
    trace_every = 1L,
    seed = NULL) {
  method <- bg_match_allocation_method(method)

  evaluation <- bg_evaluate_actions_method(
    board = board,
    method = method,
    roll = roll,
    legal_moves = NULL,
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

  bg_format_move_ranking(evaluation$results)
}

#' Recommend a move for a player-facing position query
#'
#' Returns legal moves, a ranked recommendation table, the selected move, and a
#' short explanation of how the recommendation was produced.
#'
#' @param board A `bg_board` object.
#' @param roll A `bg_roll` object.
#' @param method Allocation rule used for move evaluation.
#' @inheritParams bg_rank_moves
#'
#' @return A `bg_move_recommendation` object with elements `recommended_move`,
#'   `ranking`, and `explanation`.
#' @export
#'
#' @examples
#' points <- integer(24)
#' points[8] <- 1L
#' points[17] <- -1L
#' board <- bg_position(points = points, off = c(14L, 14L), turn = 1L)
#' rec <- bg_recommend_move(board, bg_roll(3, 2), method = "thompson", total_budget = 8L, seed = 123)
#' rec$recommended_move
bg_recommend_move <- function(
    board,
    roll,
    method = c("thompson", "ttts", "ocba", "equal", "greedy", "ucb"),
    total_budget = 32L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    unresolved_value = 0.5,
    initial_allocations = 1L,
    ucb_exploration = 1,
    prior_alpha = 1,
    prior_beta = 1,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    trace = FALSE,
    trace_every = 1L,
    seed = NULL) {
  method <- bg_match_allocation_method(method)

  evaluation <- bg_evaluate_actions_method(
    board = board,
    method = method,
    roll = roll,
    legal_moves = NULL,
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

  ranking <- bg_format_move_ranking(evaluation$results)
  explanation <- bg_make_recommendation_summary(
    ranking = ranking,
    method = evaluation$method,
    total_budget = evaluation$settings$total_budget
  )

  if (nrow(ranking) == 0L) {
    return(
      bg_new_move_recommendation(list(
        board = unclass(evaluation$board),
        roll = unclass(bg_as_roll(roll)),
        legal_moves = evaluation$legal_moves,
        ranking = ranking,
        recommended_index = integer(0L),
        recommended_move = NULL,
        explanation = explanation,
        method = evaluation$method,
        settings = evaluation$settings,
        evaluation = evaluation
      ))
    )
  }

  recommended_row <- ranking[ranking$recommended, , drop = FALSE]
  if (nrow(recommended_row) != 1L) {
    stop("Internal error: recommendation table does not contain a unique recommended move.", call. = FALSE)
  }

  bg_new_move_recommendation(list(
    board = unclass(evaluation$board),
    roll = unclass(bg_as_roll(roll)),
    legal_moves = evaluation$legal_moves,
    ranking = ranking,
    recommended_index = evaluation$recommended_index,
    recommended_move = recommended_row$move[[1L]],
    explanation = explanation,
    method = evaluation$method,
    settings = evaluation$settings,
    evaluation = evaluation
  ))
}

#' Explain a move recommendation
#'
#' Produces a short natural-language explanation of a recommendation. This is a
#' gamer-facing convenience wrapper that either accepts a previously created
#' `bg_move_recommendation` object or constructs one from `board` and `roll`.
#'
#' @param x Either a `bg_move_recommendation` object or a `bg_board` object.
#' @param roll Optional `bg_roll` object when `x` is a board.
#' @param method Allocation rule used when `x` is a board.
#' @inheritParams bg_rank_moves
#'
#' @return A character scalar.
#' @export
bg_explain_recommendation <- function(
    x,
    roll = NULL,
    method = c("thompson", "ttts", "ocba", "equal", "greedy", "ucb"),
    total_budget = 32L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    unresolved_value = 0.5,
    initial_allocations = 1L,
    ucb_exploration = 1,
    prior_alpha = 1,
    prior_beta = 1,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    trace = FALSE,
    trace_every = 1L,
    seed = NULL) {
  if (inherits(x, "bg_move_recommendation")) {
    return(x$explanation)
  }

  rec <- bg_recommend_move(
    board = x,
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

  rec$explanation
}

#' Print a move recommendation
#'
#' @param x A `bg_move_recommendation` object.
#' @param n Integer-like scalar controlling how many ranked moves to print.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.bg_move_recommendation <- function(x, n = 10L, ...) {
  if (!inherits(x, "bg_move_recommendation")) {
    stop("`x` must inherit from class 'bg_move_recommendation'.", call. = FALSE)
  }

  cat("<bg_move_recommendation>\n", sep = "")
  cat("method:       ", x$method, "\n", sep = "")
  cat("recommended:  ", if (is.null(x$recommended_move)) "<pass>" else bg_move_label(x$recommended_move), "\n", sep = "")
  if (!is.null(x$settings$total_budget)) {
    cat("total_budget: ", x$settings$total_budget, "\n", sep = "")
  }
  cat("summary:      ", x$explanation, "\n", sep = "")
  if (nrow(x$ranking) > 0L) {
    compact <- bg_compact_action_table(x$ranking, n = n)
    print(compact, row.names = FALSE)
    if (nrow(x$ranking) > nrow(compact)) {
      cat("showing_first: ", nrow(compact), " of ", nrow(x$ranking), " candidates\n", sep = "")
    }
  }
  invisible(x)
}

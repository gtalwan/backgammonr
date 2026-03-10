#' Initialize a board state
#'
#' Alias for [bg_position()] in the research-oriented API.
#'
#' @inheritParams bg_position
#'
#' @return A validated `bg_board` object.
#' @export
initialize_board <- function(...) {
  bg_position(...)
}

#' Validate a board state
#'
#' Alias for [bg_validate_board()] in the research-oriented API.
#'
#' @inheritParams bg_validate_board
#'
#' @return See [bg_validate_board()].
#' @export
validate_board <- function(state, error = TRUE) {
  bg_validate_board(state, error = error)
}

#' Print a board state
#'
#' Alias for [bg_print_board()] in the research-oriented API.
#'
#' @inheritParams bg_print_board
#'
#' @return The input board, invisibly.
#' @export
print_board <- function(board, show_indices = TRUE) {
  bg_print_board(board, show_indices = show_indices)
}

#' Plot a board state
#'
#' Alias for [bg_plot_board()] in the research-oriented API.
#'
#' @inheritParams bg_plot_board
#'
#' @return The input board, invisibly.
#' @export
plot_board <- function(board, ...) {
  bg_plot_board(board, ...)
}

#' Roll dice in the research-oriented API
#'
#' Alias for [bg_roll_dice()].
#'
#' @inheritParams bg_roll_dice
#'
#' @return See [bg_roll_dice()].
#' @export
roll_dice <- function(n = 1L, seed = NULL) {
  bg_roll_dice(n = n, seed = seed)
}

#' Generate legal candidate moves
#'
#' Alias for [bg_legal_moves()] using candidate-action terminology.
#'
#' @inheritParams bg_legal_moves
#'
#' @return A list of legal `bg_move_sequence` objects.
#' @export
generate_legal_moves <- function(state, dice, player = NULL) {
  bg_legal_moves(state, dice, player = player)
}

#' Summarize legal candidate moves in a readable table
#'
#' Converts a legal-move list into an indexed data frame with concise move
#' labels. This is useful in tutorials, debugging sessions, and reproducible
#' analyses where you want to refer to actions by stable candidate index.
#'
#' @param legal_moves A list of `bg_move_sequence` objects, or a single
#'   `bg_move_sequence`.
#' @param max_candidates Optional integer-like scalar. If supplied, only the
#'   first `max_candidates` moves are shown.
#' @param print_table Logical scalar. If `TRUE`, prints a compact summary.
#'
#' @return A data frame with columns `candidate_index`, `move_label`,
#'   `n_steps`, and `dice_used`. The full number of legal candidates is stored
#'   as attribute `n_total_candidates`.
#' @export
#'
#' @examples
#' board <- bg_initial_board()
#' roll <- bg_roll(1L, 6L)
#' legal <- generate_legal_moves(board, roll)
#' summarize_legal_moves(legal, max_candidates = 5L)
summarize_legal_moves <- function(
    legal_moves,
    max_candidates = NULL,
    print_table = TRUE) {
  moves <- lapply(bg_normalize_move_sequence_list(legal_moves), bg_as_move_sequence)
  n_total <- length(moves)

  if (is.null(max_candidates)) {
    n_show <- n_total
  } else {
    max_candidates <- bg_coerce_integerish(max_candidates, "max_candidates", 1L)
    if (max_candidates < 1L) {
      stop("`max_candidates` must be at least 1 when supplied.", call. = FALSE)
    }
    n_show <- min(n_total, max_candidates)
  }

  if (n_show == 0L) {
    out <- data.frame(
      candidate_index = integer(0L),
      move_label = character(0L),
      n_steps = integer(0L),
      dice_used = character(0L),
      stringsAsFactors = FALSE
    )
    attr(out, "n_total_candidates") <- n_total
    if (isTRUE(print_table)) {
      cat("<legal_moves_summary>\n", sep = "")
      cat("n_total_candidates: 0\n", sep = "")
      cat("No legal moves are available.\n", sep = "")
    }
    return(out)
  }

  shown <- moves[seq_len(n_show)]
  out <- data.frame(
    candidate_index = seq_len(n_show),
    move_label = vapply(shown, bg_move_label, character(1L)),
    n_steps = vapply(shown, function(x) x$n_steps, integer(1L)),
    dice_used = vapply(
      shown,
      function(x) {
        if (length(x$dice_used) == 0L) {
          return("")
        }
        paste(x$dice_used, collapse = " ")
      },
      character(1L)
    ),
    stringsAsFactors = FALSE
  )
  attr(out, "n_total_candidates") <- n_total

  if (isTRUE(print_table)) {
    cat("<legal_moves_summary>\n", sep = "")
    cat("n_total_candidates: ", n_total, "\n", sep = "")
    if (n_show < n_total) {
      cat("showing_first:      ", n_show, "\n", sep = "")
    }
    print(out, row.names = FALSE)
  }

  out
}

#' Apply a move to a board state
#'
#' Alias for [bg_apply_move_sequence()] using action terminology.
#'
#' @param state A `bg_board` object.
#' @param move A legal `bg_move_sequence` object.
#'
#' @return A `bg_board` object after applying the move.
#' @export
apply_move <- function(state, move) {
  bg_apply_move_sequence(state, move)
}

bg_match_policy_name <- function(policy, arg_name = "policy") {
  if (!is.character(policy) || length(policy) != 1L || is.na(policy)) {
    stop(sprintf("`%s` must be a non-missing character scalar.", arg_name), call. = FALSE)
  }
  bg_match_engine_selection(policy)
}

#' Simulate a continuation game from a state
#'
#' Research-oriented wrapper around [bg_play_game_matchup()].
#'
#' @param state A `bg_board` object.
#' @param policy_white Selection policy for player 1.
#' @param policy_black Selection policy for player -1.
#' @param max_turns Integer-like scalar maximum game length.
#' @param roll_sequence Optional scripted roll sequence.
#' @param seed Optional integer seed.
#' @param rollout_budget Rollout budget used by rollout-family policies.
#' @param rollout_policy Baseline rollout policy used during rollout playouts.
#' @param max_rollout_turns Integer-like scalar maximum rollout horizon.
#'
#' @return A `bg_game_result` object.
#' @export
simulate_game <- function(
    state = bg_initial_board(),
    policy_white = c(
      "random", "aggressive", "defensive", "rollout", "equal_rollout",
      "greedy_rollout", "ucb_rollout", "ocba_rollout", "thompson_rollout", "ttts_rollout"
    ),
    policy_black = c(
      "random", "aggressive", "defensive", "rollout", "equal_rollout",
      "greedy_rollout", "ucb_rollout", "ocba_rollout", "thompson_rollout", "ttts_rollout"
    ),
    max_turns = 1000L,
    roll_sequence = NULL,
    seed = NULL,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L) {
  policy_white <- bg_match_policy_name(policy_white, "policy_white")
  policy_black <- bg_match_policy_name(policy_black, "policy_black")

  bg_play_game_matchup(
    board = state,
    player1 = policy_white,
    player2 = policy_black,
    roll_sequence = roll_sequence,
    max_turns = max_turns,
    seed = seed,
    rollout_budget = rollout_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns
  )
}

#' Enumerate candidate moves for a state and dice roll
#'
#' Alias for [bg_legal_moves()] with research terminology.
#'
#' @param state A `bg_board` object.
#' @param dice A `bg_roll` object.
#'
#' @return A list of candidate moves (`bg_move_sequence`).
#' @export
enumerate_candidate_moves <- function(state, dice) {
  bg_legal_moves(state, dice)
}

#' Estimate one action value by rollout
#'
#' Estimates `mu(a) = P(win | s, a, rollout policy)` with Bernoulli reward
#' under the current rollout model.
#'
#' @param state A `bg_board` object.
#' @param action A `bg_move_sequence` object.
#' @param n_rollouts Integer-like scalar budget for this action.
#' @param rollout_policy Baseline continuation policy.
#' @param max_rollout_turns Maximum continuation length.
#' @param unresolved_value Value assigned to unresolved playouts.
#' @param prior_alpha Prior Beta alpha for Bernoulli win probability.
#' @param prior_beta Prior Beta beta for Bernoulli win probability.
#' @param seed Optional integer seed.
#'
#' @return A one-row data frame with posterior estimate and uncertainty.
#' @export
estimate_action_value <- function(
    state,
    action,
    n_rollouts = 64L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    unresolved_value = 0.5,
    prior_alpha = 1,
    prior_beta = 1,
    seed = NULL) {
  action <- bg_as_move_sequence(action)
  out <- evaluate_actions_equal(
    board = state,
    legal_moves = list(action),
    total_budget = n_rollouts,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns,
    unresolved_value = unresolved_value,
    prior_alpha = prior_alpha,
    prior_beta = prior_beta,
    seed = seed
  )
  out$results
}

#' Rollout value estimate for one move
#'
#' Alias for [estimate_action_value()].
#'
#' @inheritParams estimate_action_value
#'
#' @return A one-row data frame.
#' @export
rollout_move_value <- function(
    state,
    move,
    n_rollouts = 64L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    unresolved_value = 0.5,
    prior_alpha = 1,
    prior_beta = 1,
    seed = NULL) {
  estimate_action_value(
    state = state,
    action = move,
    n_rollouts = n_rollouts,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns,
    unresolved_value = unresolved_value,
    prior_alpha = prior_alpha,
    prior_beta = prior_beta,
    seed = seed
  )
}

#' Evaluate candidates under equal allocation
#'
#' Alias for [evaluate_actions_equal()] with research-oriented naming.
#'
#' @param state A `bg_board` object.
#' @param dice A `bg_roll` object.
#' @param budget Integer-like total simulation budget.
#' @inheritParams evaluate_actions_equal
#'
#' @return A `bg_action_evaluation` object.
#' @export
evaluate_moves_equal_allocation <- function(state, dice, budget = 32L, ...) {
  evaluate_actions_equal(board = state, roll = dice, total_budget = budget, ...)
}

#' Evaluate candidates under UCB allocation
#'
#' Alias for [evaluate_actions_ucb()] with research-oriented naming.
#'
#' @param state A `bg_board` object.
#' @param dice A `bg_roll` object.
#' @param budget Integer-like total simulation budget.
#' @inheritParams evaluate_actions_ucb
#'
#' @return A `bg_action_evaluation` object.
#' @export
evaluate_moves_ucb <- function(state, dice, budget = 32L, ...) {
  evaluate_actions_ucb(board = state, roll = dice, total_budget = budget, ...)
}

#' Evaluate candidates under Thompson allocation
#'
#' Alias for [evaluate_actions_thompson()] with research-oriented naming.
#'
#' @param state A `bg_board` object.
#' @param dice A `bg_roll` object.
#' @param budget Integer-like total simulation budget.
#' @inheritParams evaluate_actions_thompson
#'
#' @return A `bg_action_evaluation` object.
#' @export
evaluate_moves_thompson <- function(state, dice, budget = 32L, ...) {
  evaluate_actions_thompson(board = state, roll = dice, total_budget = budget, ...)
}

#' Evaluate candidates under top-two Thompson allocation
#'
#' Alias for [evaluate_actions_ttts()] with research-oriented naming.
#'
#' @param state A `bg_board` object.
#' @param dice A `bg_roll` object.
#' @param budget Integer-like total simulation budget.
#' @inheritParams evaluate_actions_ttts
#'
#' @return A `bg_action_evaluation` object.
#' @export
evaluate_moves_ttts <- function(state, dice, budget = 32L, ...) {
  evaluate_actions_ttts(board = state, roll = dice, total_budget = budget, ...)
}

#' Evaluate candidates with successive elimination (MVP heuristic)
#'
#' Current implementation uses an OCBA-backed elimination surrogate under the
#' same fixed-budget interface. A dedicated C++ successive-elimination engine is
#' planned for a future release.
#'
#' @param state A `bg_board` object.
#' @param dice A `bg_roll` object.
#' @param budget Integer-like total simulation budget.
#' @inheritParams evaluate_actions_ocba
#'
#' @return A `bg_action_evaluation` object.
#' @export
evaluate_moves_successive_elimination <- function(state, dice, budget = 32L, ...) {
  evaluate_actions_ocba(board = state, roll = dice, total_budget = budget, ...)
}

#' Identify a reference best move using a large budget
#'
#' @param state A `bg_board` object.
#' @param dice A `bg_roll` object.
#' @param large_budget Integer-like truth budget.
#' @inheritParams approximate_action_truth
#'
#' @return A list with `reference_index`, `reference_move`, and `truth`.
#' @export
identify_reference_best_move <- function(
    state,
    dice,
    large_budget = 512L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    unresolved_value = 0.5,
    prior_alpha = 1,
    prior_beta = 1,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    seed = NULL) {
  truth <- approximate_action_truth(
    board = state,
    roll = dice,
    truth_budget = large_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns,
    unresolved_value = unresolved_value,
    prior_alpha = prior_alpha,
    prior_beta = prior_beta,
    dice_mode = dice_mode,
    crn = crn,
    seed = seed
  )
  list(
    reference_index = truth$recommended_index,
    reference_move = truth$recommended_move,
    truth = truth
  )
}

#' Benchmark evaluators over positions and budgets
#'
#' Wrapper around [benchmark_allocation_methods()].
#'
#' @param test_positions A list of `bg_benchmark_case` objects.
#' @param budgets Integer-like budget vector.
#' @param methods Allocation methods.
#' @inheritParams benchmark_allocation_methods
#'
#' @return A `bg_allocation_benchmark` object.
#' @export
benchmark_evaluators <- function(
    test_positions,
    budgets = c(16L, 32L, 64L),
    methods = c("thompson", "ttts", "equal", "ucb", "ocba"),
    truth_budget = 512L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    unresolved_value = 0.5,
    initial_allocations = 1L,
    ucb_exploration = 1,
    prior_alpha = 1,
    prior_beta = 1,
    dice_modes = c("iid"),
    crn_values = c(FALSE),
    truth_dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    truth_crn = FALSE,
    seed = NULL) {
  benchmark_allocation_methods(
    cases = test_positions,
    methods = methods,
    budgets = budgets,
    truth_budget = truth_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns,
    unresolved_value = unresolved_value,
    initial_allocations = initial_allocations,
    ucb_exploration = ucb_exploration,
    prior_alpha = prior_alpha,
    prior_beta = prior_beta,
    dice_modes = dice_modes,
    crn_values = crn_values,
    truth_dice_mode = truth_dice_mode,
    truth_crn = truth_crn,
    seed = seed
  )
}

#' Summarize benchmark outputs
#'
#' @param x A `bg_allocation_benchmark` object.
#'
#' @return A summary data frame.
#' @export
summarize_benchmark_results <- function(x) {
  if (!inherits(x, "bg_allocation_benchmark")) {
    stop("`x` must inherit from class 'bg_allocation_benchmark'.", call. = FALSE)
  }
  x$summary
}

#' Plot benchmark summary results
#'
#' @param x A `bg_allocation_benchmark` object.
#' @param metric Metric to plot.
#' @param ... Passed to [bg_plot_benchmark_summary()].
#'
#' @return The plotting data frame, invisibly.
#' @export
plot_benchmark_results <- function(
    x,
    metric = c(
      "probability_correct_selection",
      "mean_simple_regret",
      "mean_mse",
      "mean_runtime_seconds"
    ),
    ...) {
  bg_plot_benchmark_summary(x, metric = metric, ...)
}

#' Plot budget vs selection accuracy
#'
#' @param x A `bg_allocation_benchmark` object.
#' @param ... Unused.
#'
#' @return The plotting data frame, invisibly.
#' @export
plot_budget_accuracy_curve <- function(x, ...) {
  bg_plot_benchmark_summary(x, metric = "probability_correct_selection")
}

#' Plot budget vs runtime
#'
#' @param x A `bg_allocation_benchmark` object.
#' @param ... Unused.
#'
#' @return The plotting data frame, invisibly.
#' @export
plot_runtime_curve <- function(x, ...) {
  bg_plot_benchmark_summary(x, metric = "mean_runtime_seconds")
}

#' Explain a position and optional roll
#'
#' @param state A `bg_board` object.
#' @param dice Optional `bg_roll` object.
#'
#' @return A character scalar.
#' @export
explain_position <- function(state, dice = NULL) {
  info <- bg_inspect_board(state)
  if (is.null(dice)) {
    return(
      paste0(
        "Position with turn ", info$turn, ", bar counts p1=", info$bar[[1L]],
        " p2=", info$bar[[2L]], ", off counts p1=", info$off[[1L]],
        " p2=", info$off[[2L]], "."
      )
    )
  }
  moves <- bg_legal_moves(state, dice)
  paste0(
    "Position with turn ", info$turn, " and roll ", paste(bg_as_roll(dice)$dice, collapse = "-"),
    ". Number of legal candidate moves: ", length(moves), "."
  )
}

#' Explain a move-evaluation object
#'
#' @param x A `bg_action_evaluation` or `bg_move_recommendation` object.
#'
#' @return A character scalar.
#' @export
explain_move_evaluation <- function(x) {
  if (inherits(x, "bg_move_recommendation")) {
    return(x$explanation)
  }
  if (!inherits(x, "bg_action_evaluation")) {
    stop("`x` must be a `bg_action_evaluation` or `bg_move_recommendation` object.", call. = FALSE)
  }
  if (nrow(x$results) == 0L) {
    return("No legal moves were available in this position under the supplied roll.")
  }
  best <- x$results[x$recommended_index == x$results$candidate_index, , drop = FALSE]
  paste0(
    "Method ", x$method, " selected candidate ", best$candidate_index[[1L]],
    " (`", best$move_label[[1L]], "`) with posterior mean ",
    formatC(best$estimate[[1L]], digits = 3, format = "f"),
    " after ", best$allocation_count[[1L]], " rollouts."
  )
}

#' Compare posterior summaries across actions
#'
#' @param x A `bg_action_evaluation` or `bg_move_recommendation` object.
#' @param top_n Number of top rows to return.
#' @param diagnostics Logical scalar. If `TRUE`, include Beta-shape and
#'   rollout-outcome count diagnostics in addition to the compact posterior
#'   interpretation columns.
#'
#' @return A data frame of posterior summaries.
#' @export
compare_action_posteriors <- function(x, top_n = 10L, diagnostics = FALSE) {
  top_n <- bg_coerce_integerish(top_n, "top_n", 1L)
  if (top_n < 1L) {
    stop("`top_n` must be at least 1.", call. = FALSE)
  }
  bg_assert_scalar_flag(diagnostics, "diagnostics")
  tab <- if (inherits(x, "bg_move_recommendation")) x$ranking else x$results
  if (is.null(tab)) {
    stop("`x` does not contain action summaries.", call. = FALSE)
  }

  if (isTRUE(diagnostics)) {
    cols <- intersect(
      c(
        "rank",
        "candidate_index",
        "move_label",
        "allocation_count",
        "wins",
        "losses",
        "unresolved",
        "alpha",
        "beta",
        "estimate",
        "posterior_sd",
        "lower_95",
        "upper_95",
        "prob_best",
        "posterior_expected_regret"
      ),
      names(tab)
    )
    out <- tab[, cols, drop = FALSE]
    return(utils::head(out, top_n))
  }

  compact <- bg_compact_action_table(tab, n = top_n)
  if ("action_id" %in% names(compact)) {
    names(compact)[names(compact) == "action_id"] <- "candidate_index"
  }
  if ("action" %in% names(compact)) {
    names(compact)[names(compact) == "action"] <- "move_label"
  }
  if ("alloc_n" %in% names(compact)) {
    names(compact)[names(compact) == "alloc_n"] <- "allocation_count"
  }
  compact
}

#' Extract allocation trace history
#'
#' @param x A `bg_action_evaluation`, `bg_move_recommendation`, or
#'   `bg_analysis_report` object.
#'
#' @return A trace data frame, or `NULL` if unavailable.
#' @export
trace_allocation_history <- function(x) {
  if (inherits(x, "bg_analysis_report")) {
    return(x$trace)
  }
  if (inherits(x, "bg_move_recommendation")) {
    return(if (!is.null(x$evaluation)) x$evaluation$trace else NULL)
  }
  if (inherits(x, "bg_action_evaluation")) {
    return(x$trace)
  }
  stop("`x` must be a `bg_action_evaluation`, `bg_move_recommendation`, or `bg_analysis_report` object.", call. = FALSE)
}

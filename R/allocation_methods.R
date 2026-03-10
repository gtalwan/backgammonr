bg_match_allocation_method <- function(method) {
  match.arg(
    method,
    choices = c(
      "equal",
      "greedy",
      "ucb",
      "ocba",
      "thompson",
      "ttts",
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

bg_canonicalize_allocation_method <- function(method) {
  method <- bg_match_allocation_method(method)

  if (method %in% c("equal", "rollout", "equal_rollout")) {
    return("equal")
  }

  if (method %in% c("greedy", "greedy_rollout")) {
    return("greedy")
  }

  if (method %in% c("ucb", "ucb_rollout")) {
    return("ucb")
  }

  if (method %in% c("ocba", "ocba_rollout")) {
    return("ocba")
  }

  if (method %in% c("ttts", "ttts_rollout")) {
    return("ttts")
  }

  "thompson"
}

bg_match_rollout_selection <- function(selection) {
  match.arg(
    selection,
    choices = c(
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

  weights <- seq_along(ints) + 17
  hashed <- sum(as.numeric(weights) * as.numeric(ints)) %% 2147483647
  as.integer((as.numeric(seed) + hashed) %% 2147483647)
}

bg_normalize_allocation_args <- function(
    total_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    unresolved_value = 0.5,
    initial_allocations = 1L,
    ucb_exploration = 1,
    prior_alpha = 1,
    prior_beta = 1,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    fast_diagnostics = FALSE) {
  total_budget <- bg_coerce_integerish(total_budget, "total_budget", 1L)
  if (total_budget < 1L) {
    stop("`total_budget` must be at least 1.", call. = FALSE)
  }

  rollout_policy <- bg_match_rollout_policy(rollout_policy)

  max_rollout_turns <- bg_coerce_integerish(max_rollout_turns, "max_rollout_turns", 1L)
  if (max_rollout_turns < 0L) {
    stop("`max_rollout_turns` must be nonnegative.", call. = FALSE)
  }

  initial_allocations <- bg_coerce_integerish(initial_allocations, "initial_allocations", 1L)
  if (initial_allocations < 0L) {
    stop("`initial_allocations` must be nonnegative.", call. = FALSE)
  }

  if (!is.numeric(unresolved_value) || length(unresolved_value) != 1L || is.na(unresolved_value)) {
    stop("`unresolved_value` must be a numeric scalar.", call. = FALSE)
  }
  if (unresolved_value < 0 || unresolved_value > 1) {
    stop("`unresolved_value` must lie between 0 and 1.", call. = FALSE)
  }

  if (!is.numeric(ucb_exploration) || length(ucb_exploration) != 1L || is.na(ucb_exploration)) {
    stop("`ucb_exploration` must be a numeric scalar.", call. = FALSE)
  }
  if (ucb_exploration < 0) {
    stop("`ucb_exploration` must be nonnegative.", call. = FALSE)
  }

  if (!is.numeric(prior_alpha) || length(prior_alpha) != 1L || is.na(prior_alpha)) {
    stop("`prior_alpha` must be a numeric scalar.", call. = FALSE)
  }
  if (!is.numeric(prior_beta) || length(prior_beta) != 1L || is.na(prior_beta)) {
    stop("`prior_beta` must be a numeric scalar.", call. = FALSE)
  }
  if (prior_alpha <= 0 || prior_beta <= 0) {
    stop("`prior_alpha` and `prior_beta` must be strictly positive.", call. = FALSE)
  }

  dice_mode <- match.arg(dice_mode)
  bg_assert_scalar_flag(crn, "crn")
  bg_assert_scalar_flag(fast_diagnostics, "fast_diagnostics")

  list(
    total_budget = total_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns,
    unresolved_value = as.numeric(unresolved_value),
    initial_allocations = initial_allocations,
    ucb_exploration = as.numeric(ucb_exploration),
    prior_alpha = as.numeric(prior_alpha),
    prior_beta = as.numeric(prior_beta),
    dice_mode = dice_mode,
    crn = isTRUE(crn),
    fast_diagnostics = isTRUE(fast_diagnostics)
  )
}

bg_roll_equals <- function(x, y) {
  if (is.null(x) || is.null(y)) {
    return(FALSE)
  }

  identical(bg_as_roll(x)$dice, bg_as_roll(y)$dice)
}

bg_extract_shared_roll <- function(legal_moves) {
  if (length(legal_moves) == 0L) {
    return(NULL)
  }

  rolls <- lapply(legal_moves, function(x) bg_as_move_sequence(x)$roll)
  if (any(vapply(rolls, is.null, logical(1L)))) {
    return(NULL)
  }

  ref <- rolls[[1L]]
  if (all(vapply(rolls, bg_roll_equals, logical(1L), y = ref))) {
    return(ref)
  }

  NULL
}

bg_prepare_action_inputs <- function(board, roll = NULL, legal_moves = NULL) {
  if (!is_bg_board(board)) {
    stop("`board` must inherit from class 'bg_board'.", call. = FALSE)
  }

  bg_validate_board(board)

  if (is.null(legal_moves)) {
    if (is.null(roll)) {
      stop("Supply either `roll` or `legal_moves`.", call. = FALSE)
    }

    roll <- bg_as_roll(roll)
    legal_moves <- bg_legal_moves(board, roll)
  } else {
    legal_moves <- bg_normalize_move_sequence_list(legal_moves)
    if (any(vapply(legal_moves, function(x) is.null(bg_as_move_sequence(x)$roll), logical(1L)))) {
      stop(
        "Each move in `legal_moves` must include a `roll` field. Use `bg_legal_moves()` to generate legal move sets.",
        call. = FALSE
      )
    }

    if (is.null(roll)) {
      roll <- bg_extract_shared_roll(legal_moves)
    } else {
      roll <- bg_as_roll(roll)
    }
  }

  list(
    board = board,
    roll = roll,
    legal_moves = lapply(legal_moves, bg_unclass_move_sequence)
  )
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

bg_new_action_evaluation <- function(x) {
  x$board <- bg_new_board(x$board)
  x$roll <- bg_wrap_roll_output(x$roll)
  x$legal_moves <- lapply(x$legal_moves, bg_new_move_sequence)
  x$recommended_move <- if (is.null(x$recommended_move)) NULL else bg_new_move_sequence(x$recommended_move)
  x$results <- as.data.frame(x$results, stringsAsFactors = FALSE)
  x$trace <- if (is.null(x$trace)) NULL else as.data.frame(x$trace, stringsAsFactors = FALSE)
  x$runtime_seconds <- if (is.null(x$runtime_seconds)) NA_real_ else as.numeric(x$runtime_seconds[[1L]])
  structure(x, class = "bg_action_evaluation")
}

bg_action_results_with_labels <- function(results, legal_moves) {
  results <- as.data.frame(results, stringsAsFactors = FALSE)
  if (nrow(results) == 0L) {
    results$move_label <- character(0L)
    results$n_steps <- integer(0L)
    return(results)
  }

  if (!"candidate_index" %in% names(results)) {
    stop("Internal error: expected a `candidate_index` column in evaluation results.", call. = FALSE)
  }

  candidate_index <- as.integer(results$candidate_index)
  if (anyNA(candidate_index) || any(candidate_index < 1L) || any(candidate_index > length(legal_moves))) {
    stop("Internal error: `candidate_index` contains out-of-range move indices.", call. = FALSE)
  }

  move_objects <- lapply(candidate_index, function(i) bg_new_move_sequence(legal_moves[[i]]))
  results$move <- I(move_objects)
  results$move_label <- vapply(move_objects, bg_move_label, character(1L))
  results$n_steps <- vapply(move_objects, function(x) x$n_steps, integer(1L))

  results
}

bg_sort_action_results <- function(results) {
  if (nrow(results) == 0L) {
    return(results)
  }

  ord <- order(-results$estimate, -results$empirical_value, results$candidate_index)
  out <- results[ord, , drop = FALSE]
  out$rank <- seq_len(nrow(out))
  rownames(out) <- NULL
  out
}

bg_trace_checkpoints <- function(total_budget, trace_every) {
  trace_every <- bg_coerce_integerish(trace_every, "trace_every", 1L)
  if (trace_every < 1L) {
    stop("`trace_every` must be at least 1.", call. = FALSE)
  }

  checkpoints <- seq.int(from = trace_every, to = total_budget, by = trace_every)
  if (length(checkpoints) == 0L || checkpoints[[length(checkpoints)]] != total_budget) {
    checkpoints <- c(checkpoints, total_budget)
  }
  checkpoints
}

bg_as_named_numeric <- function(values, keys) {
  stats::setNames(as.numeric(values), as.character(keys))
}

bg_build_allocation_trace <- function(
    board,
    legal_moves,
    method,
    alloc_args,
    seed_args,
    trace_every) {
  # Re-run the same evaluation at growing checkpoints to reconstruct a compact
  # allocation trajectory without storing per-rollout state in C++.
  checkpoints <- bg_trace_checkpoints(alloc_args$total_budget, trace_every)
  trace_rows <- vector("list", length(checkpoints))
  previous_counts <- NULL
  previous_checkpoint <- NA_integer_

  for (i in seq_along(checkpoints)) {
    checkpoint <- checkpoints[[i]]
    raw <- bg_cpp_allocation_evaluate(
      board,
      legal_moves,
      method,
      checkpoint,
      alloc_args$rollout_policy,
      alloc_args$max_rollout_turns,
      alloc_args$unresolved_value,
      alloc_args$initial_allocations,
      alloc_args$ucb_exploration,
      alloc_args$prior_alpha,
      alloc_args$prior_beta,
      alloc_args$dice_mode,
      alloc_args$crn,
      alloc_args$fast_diagnostics,
      seed_args$seed,
      seed_args$use_seed
    )

    tab <- as.data.frame(raw$results, stringsAsFactors = FALSE)
    if (nrow(tab) == 0L) {
      trace_rows[[i]] <- tab
      next
    }
    tab <- tab[order(tab$candidate_index), , drop = FALSE]
    leader_index <- tab$candidate_index[[which.max(tab$estimate)]]
    tab$checkpoint <- checkpoint
    tab$leader_index <- leader_index
    tab$selected_candidate <- NA_integer_

    current_counts <- bg_as_named_numeric(tab$allocation_count, tab$candidate_index)
    if (!is.null(previous_counts) && identical(checkpoint, previous_checkpoint + 1L)) {
      # When checkpoints are one-step apart, infer the selected candidate from
      # the allocation-count increment pattern.
      keys <- union(names(previous_counts), names(current_counts))
      prev <- previous_counts[keys]
      curr <- current_counts[keys]
      prev[is.na(prev)] <- 0
      curr[is.na(curr)] <- 0
      delta <- curr - prev

      if (sum(delta) == 1 && sum(delta == 1) == 1 && all(delta >= 0)) {
        selected <- as.integer(names(delta)[delta == 1][1])
        tab$selected_candidate <- selected
      }
    }

    previous_counts <- current_counts
    previous_checkpoint <- checkpoint
    trace_rows[[i]] <- tab
  }

  out <- do.call(rbind, trace_rows)
  if (!is.null(out) && nrow(out) > 0L) {
    rownames(out) <- NULL
  }
  out
}

bg_evaluate_actions_method <- function(
    board,
    method,
    roll = NULL,
    legal_moves = NULL,
    total_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    unresolved_value = 0.5,
    initial_allocations = 1L,
    ucb_exploration = 1,
    prior_alpha = 1,
    prior_beta = 1,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    fast_diagnostics = FALSE,
    trace = FALSE,
    trace_every = 1L,
    seed = NULL) {
  method <- bg_canonicalize_allocation_method(method)
  prepared <- bg_prepare_action_inputs(board = board, roll = roll, legal_moves = legal_moves)
  alloc_args <- bg_normalize_allocation_args(
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
    fast_diagnostics = fast_diagnostics
  )
  bg_assert_scalar_flag(trace, "trace")
  seed_args <- bg_normalize_seed_args(seed)
  start_time <- as.numeric(proc.time()[3L])

  if (length(prepared$legal_moves) == 0L) {
    empty <- data.frame(
      candidate_index = integer(0L),
      allocation_count = integer(0L),
      wins = integer(0L),
      losses = integer(0L),
      unresolved = integer(0L),
      empirical_value = numeric(0L),
      alpha = numeric(0L),
      beta = numeric(0L),
      n_equivalent_sequences = integer(0L),
      estimate = numeric(0L),
      posterior_sd = numeric(0L),
      lower_95 = numeric(0L),
      upper_95 = numeric(0L),
      prob_best = numeric(0L),
      posterior_expected_regret = numeric(0L),
      selection_score = numeric(0L),
      stringsAsFactors = FALSE
    )

    return(
      bg_new_action_evaluation(list(
        board = unclass(prepared$board),
        roll = if (is.null(prepared$roll)) NULL else unclass(prepared$roll),
        legal_moves = prepared$legal_moves,
        results = bg_action_results_with_labels(empty, prepared$legal_moves),
        recommended_index = integer(0L),
        recommended_move = NULL,
        trace = data.frame(),
        method = method,
        runtime_seconds = as.numeric(proc.time()[3L] - start_time),
        settings = c(
          alloc_args,
          list(
            seed = if (is.null(seed)) NULL else seed,
            runtime_seconds = as.numeric(proc.time()[3L] - start_time)
          )
        )
      ))
    )
  }

  raw <- if (isTRUE(trace)) {
    bg_cpp_allocation_evaluate_trace(
      unclass(prepared$board),
      prepared$legal_moves,
      method,
      alloc_args$total_budget,
      alloc_args$rollout_policy,
      alloc_args$max_rollout_turns,
      alloc_args$unresolved_value,
      alloc_args$initial_allocations,
      alloc_args$ucb_exploration,
      alloc_args$prior_alpha,
      alloc_args$prior_beta,
      alloc_args$dice_mode,
      alloc_args$crn,
      alloc_args$fast_diagnostics,
      trace_every,
      seed_args$seed,
      seed_args$use_seed
    )
  } else {
    bg_cpp_allocation_evaluate(
      unclass(prepared$board),
      prepared$legal_moves,
      method,
      alloc_args$total_budget,
      alloc_args$rollout_policy,
      alloc_args$max_rollout_turns,
      alloc_args$unresolved_value,
      alloc_args$initial_allocations,
      alloc_args$ucb_exploration,
      alloc_args$prior_alpha,
      alloc_args$prior_beta,
      alloc_args$dice_mode,
      alloc_args$crn,
      alloc_args$fast_diagnostics,
      seed_args$seed,
      seed_args$use_seed
    )
  }

  results <- bg_action_results_with_labels(raw$results, prepared$legal_moves)
  results <- bg_sort_action_results(results)
  recommended_index <- as.integer(raw$recommended_index[[1L]])
  recommended_position <- match(recommended_index, results$candidate_index)
  results$recommended <- results$candidate_index == recommended_index
  trace_df <- if (isTRUE(trace) && !is.null(raw$trace)) {
    as.data.frame(raw$trace, stringsAsFactors = FALSE)
  } else if (isTRUE(trace)) {
    # Fallback path for compatibility if a future engine omits trace payload.
    bg_build_allocation_trace(
      board = unclass(prepared$board),
      legal_moves = prepared$legal_moves,
      method = method,
      alloc_args = alloc_args,
      seed_args = seed_args,
      trace_every = trace_every
    )
  } else {
    NULL
  }
  runtime_seconds <- as.numeric(proc.time()[3L] - start_time)

  bg_new_action_evaluation(list(
    board = unclass(prepared$board),
    roll = if (is.null(prepared$roll)) NULL else unclass(prepared$roll),
    legal_moves = prepared$legal_moves,
    results = results,
    recommended_index = recommended_index,
    recommended_move = prepared$legal_moves[[recommended_index]],
    recommended_rank = if (is.na(recommended_position)) NA_integer_ else recommended_position,
    trace = trace_df,
    method = as.character(raw$method[[1L]]),
    runtime_seconds = runtime_seconds,
    settings = c(
      alloc_args,
      list(
        seed = if (is.null(seed)) NULL else seed,
        runtime_seconds = runtime_seconds
      )
    )
  ))
}

#' Evaluate legal moves with equal rollout allocation
#'
#' In package terms, a *rollout* is a simulation that starts from one candidate
#' move, plays the game forward under a baseline policy, and records the final
#' outcome. In statistical language, each rollout is a Monte Carlo path used to
#' estimate a move-specific expected outcome, here a win probability with
#' unresolved games mapped to `unresolved_value`.
#'
#' `evaluate_actions_equal()` spends a fixed total simulation budget across the
#' legal moves as evenly as possible. It is the main equal-allocation baseline
#' for comparing adaptive allocation rules such as UCB and Thompson sampling.
#'
#' Statistical question addressed:
#'
#' - With a fixed finite budget, what can we learn about the ranking of legal
#'   actions if we do not adapt allocation across actions?
#' - How much uncertainty remains in each action estimate after equal spending?
#'
#' Interpretation guide:
#'
#' - `estimate`: posterior mean action value under the rollout model.
#' - `posterior_sd` and `lower_95`/`upper_95`: uncertainty in that estimate.
#' - `allocation_count`: simulation effort spent on each action.
#' - `prob_best` and `posterior_expected_regret`: optional decision diagnostics
#'   (set `fast_diagnostics = FALSE` to compute them).
#' - `recommended`: action selected by the method at budget end.
#'
#' Limitations:
#'
#' - Reported values are model-relative (rollout policy, horizon, unresolved
#'   handling, and dice settings).
#' - Finite-budget uncertainty can remain large on hard positions.
#' - Equal allocation is intentionally non-adaptive and may be sample-inefficient
#'   when only a few actions are plausible best moves.
#'
#' @param board A `bg_board` object.
#' @param roll Optional `bg_roll` object. Supply `roll` when `legal_moves` is
#'   omitted.
#' @param legal_moves Optional list of legal `bg_move_sequence` objects. When
#'   omitted, legal moves are generated from `board` and `roll`.
#' @param total_budget Integer-like scalar giving the total number of rollouts
#'   to allocate across all legal moves.
#' @param rollout_policy Baseline policy used inside rollout playouts.
#' @param max_rollout_turns Integer-like scalar giving the maximum number of
#'   turns per rollout playout.
#' @param unresolved_value Numeric scalar in `[0, 1]` used when a rollout hits
#'   the turn limit without finishing. A value of `0.5` treats unresolved
#'   playouts as half a win.
#' @param initial_allocations Integer-like scalar giving the number of initial
#'   round-robin allocations used by adaptive methods. It is ignored by the
#'   equal-allocation baseline but retained for interface consistency.
#' @param ucb_exploration Exploration constant for UCB. Ignored here.
#' @param prior_alpha Positive prior Beta shape parameter. Used to smooth the
#'   posterior mean reported in the output.
#' @param prior_beta Positive prior Beta shape parameter. Used to smooth the
#'   posterior mean reported in the output.
#' @param dice_mode Dice-randomization mode for rollout starts. `"iid"` uses
#'   standard independent rollouts. `"stratified_first_roll"` and
#'   `"stratified_first_two_rolls"` balance early dice outcomes.
#' @param crn Logical scalar; if `TRUE`, uses common random numbers across
#'   candidate moves by matching rollout random streams at each sample index.
#' @param trace Logical scalar; if `TRUE`, returns checkpoint-level allocation
#'   traces in the `trace` component.
#' @param trace_every Integer-like scalar giving the checkpoint interval for
#'   `trace`.
#' @param seed Optional integer-like scalar for reproducible rollout sampling.
#'
#' @return A `bg_action_evaluation` object with elements:
#'   \describe{
#'     \item{`results`}{data frame of candidate-level estimates, uncertainty
#'     summaries, and move labels;}
#'     \item{`recommended_index`}{1-based index of the recommended legal move;}
#'     \item{`recommended_move`}{the recommended `bg_move_sequence` object;}
#'     \item{`runtime_seconds`}{elapsed runtime for the evaluation call;}
#'     \item{`trace`}{optional checkpoint-level allocation history when
#'     `trace = TRUE`;}
#'     \item{`settings`}{evaluation settings used to construct the output.}
#'   }
#' @export
#'
#' @examples
#' points <- integer(24)
#' points[8] <- 1L
#' points[17] <- -1L
#' board <- bg_position(points = points, off = c(14L, 14L), turn = 1L)
#' evaluate_actions_equal(board, roll = bg_roll(3, 2), total_budget = 8L, seed = 123)
evaluate_actions_equal <- function(
    board,
    roll = NULL,
    legal_moves = NULL,
    total_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    unresolved_value = 0.5,
    initial_allocations = 1L,
    ucb_exploration = 1,
    prior_alpha = 1,
    prior_beta = 1,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    fast_diagnostics = FALSE,
    trace = FALSE,
    trace_every = 1L,
    seed = NULL) {
  bg_evaluate_actions_method(
    board = board,
    method = "equal",
    roll = roll,
    legal_moves = legal_moves,
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
    fast_diagnostics = fast_diagnostics,
    trace = trace,
    trace_every = trace_every,
    seed = seed
  )
}

#' Evaluate legal moves with greedy rollout allocation
#'
#' Uses a fixed total rollout budget but always allocates the next simulation to
#' the move with the currently highest posterior mean. This method is fast and
#' can work well on easy positions, but it may lock in too early on noisy
#' initial estimates.
#'
#' @inheritParams evaluate_actions_equal
#'
#' @return A `bg_action_evaluation` object.
#' @export
evaluate_actions_greedy <- function(
    board,
    roll = NULL,
    legal_moves = NULL,
    total_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    unresolved_value = 0.5,
    initial_allocations = 1L,
    ucb_exploration = 1,
    prior_alpha = 1,
    prior_beta = 1,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    fast_diagnostics = FALSE,
    trace = FALSE,
    trace_every = 1L,
    seed = NULL) {
  bg_evaluate_actions_method(
    board = board,
    method = "greedy",
    roll = roll,
    legal_moves = legal_moves,
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
    fast_diagnostics = fast_diagnostics,
    trace = trace,
    trace_every = trace_every,
    seed = seed
  )
}

#' Evaluate legal moves with UCB rollout allocation
#'
#' Uses an upper-confidence-bound allocation rule to balance exploration and
#' exploitation under a fixed rollout budget.
#'
#' @inheritParams evaluate_actions_equal
#'
#' @return A `bg_action_evaluation` object.
#' @export
evaluate_actions_ucb <- function(
    board,
    roll = NULL,
    legal_moves = NULL,
    total_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    unresolved_value = 0.5,
    initial_allocations = 1L,
    ucb_exploration = 1,
    prior_alpha = 1,
    prior_beta = 1,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    fast_diagnostics = FALSE,
    trace = FALSE,
    trace_every = 1L,
    seed = NULL) {
  bg_evaluate_actions_method(
    board = board,
    method = "ucb",
    roll = roll,
    legal_moves = legal_moves,
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
    fast_diagnostics = fast_diagnostics,
    trace = trace,
    trace_every = trace_every,
    seed = seed
  )
}

#' Evaluate legal moves with OCBA rollout allocation
#'
#' Uses an OCBA-inspired adaptive allocation rule to focus simulations on moves
#' with high uncertainty and small estimated gaps from the current best move.
#'
#' @inheritParams evaluate_actions_equal
#'
#' @return A `bg_action_evaluation` object.
#' @export
evaluate_actions_ocba <- function(
    board,
    roll = NULL,
    legal_moves = NULL,
    total_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    unresolved_value = 0.5,
    initial_allocations = 1L,
    ucb_exploration = 1,
    prior_alpha = 1,
    prior_beta = 1,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    fast_diagnostics = FALSE,
    trace = FALSE,
    trace_every = 1L,
    seed = NULL) {
  bg_evaluate_actions_method(
    board = board,
    method = "ocba",
    roll = roll,
    legal_moves = legal_moves,
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
    fast_diagnostics = fast_diagnostics,
    trace = trace,
    trace_every = trace_every,
    seed = seed
  )
}

#' Evaluate legal moves with Thompson-sampling rollout allocation
#'
#' Uses Thompson sampling to adaptively allocate a fixed total rollout budget
#' across candidate moves. Each candidate move carries a Beta posterior over its
#' win probability, and the next rollout is assigned to the move with the
#' largest sampled posterior draw.
#'
#' Why this function exists:
#'
#' - It is the package's central finite-budget allocation rule for studying
#'   best-action identification under simulation noise.
#' - It provides both action-level posterior summaries and allocation behavior,
#'   so users can inspect *how* budget was spent, not only which move won.
#'
#' Statistical question addressed:
#'
#' - Does adaptive posterior sampling recover the high-budget reference-best
#'   action with less budget than non-adaptive baselines?
#' - Does Thompson focus rollouts on plausible high-value actions while keeping
#'   uncertainty-aware exploration?
#'
#' How to evaluate the returned output:
#'
#' - Check `results$allocation_count` to see whether budget concentrated on
#'   plausible best actions.
#' - Use `results$prob_best` and `results$posterior_expected_regret` to quantify
#'   recommendation confidence and downside risk.
#' - Compare finite-budget output to [approximate_action_reference()] or
#'   [compare_thompson_to_reference()] for proxy PCS, simple regret, and MSE.
#' - Enable `trace = TRUE` for checkpoint-level allocation dynamics.
#'
#' Limitations:
#'
#' - Thompson can be unstable at very small budgets.
#' - Early noisy wins can cause temporary overcommitment.
#' - Posterior quantities are conditional on rollout-model assumptions.
#'
#' @inheritParams evaluate_actions_equal
#'
#' @return A `bg_action_evaluation` object.
#' @export
evaluate_actions_thompson <- function(
    board,
    roll = NULL,
    legal_moves = NULL,
    total_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    unresolved_value = 0.5,
    initial_allocations = 1L,
    ucb_exploration = 1,
    prior_alpha = 1,
    prior_beta = 1,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    fast_diagnostics = FALSE,
    trace = FALSE,
    trace_every = 1L,
    seed = NULL) {
  bg_evaluate_actions_method(
    board = board,
    method = "thompson",
    roll = roll,
    legal_moves = legal_moves,
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
    fast_diagnostics = fast_diagnostics,
    trace = trace,
    trace_every = trace_every,
    seed = seed
  )
}

#' Evaluate legal moves with Top-Two Thompson Sampling allocation
#'
#' Uses a Top-Two Thompson Sampling (TTTS) budget-allocation rule for finite
#' rollout budgets. At each allocation step:
#'
#' 1. sample posterior action values and identify the current sampled best action
#'    `I`;
#' 2. with probability `ttts_beta`, allocate to `I`;
#' 3. otherwise allocate to a distinct sampled challenger action.
#'
#' This variant is tailored to best-action identification under fixed budgets.
#'
#' In practice, TTTS often allocates more rollouts to near-tied top actions than
#' standard Thompson sampling. This can improve separation of hard top-two
#' cases, but gains are state-dependent and not guaranteed.
#'
#' @inheritParams evaluate_actions_equal
#' @param ttts_beta Numeric scalar in `(0, 1]` controlling how often TTTS
#'   allocates to the currently sampled best action. Values near `0.5` are a
#'   common default in best-arm identification studies.
#'
#' @return A `bg_action_evaluation` object.
#' @export
evaluate_actions_ttts <- function(
    board,
    roll = NULL,
    legal_moves = NULL,
    total_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    unresolved_value = 0.5,
    initial_allocations = 1L,
    ttts_beta = 0.5,
    prior_alpha = 1,
    prior_beta = 1,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    fast_diagnostics = FALSE,
    trace = FALSE,
    trace_every = 1L,
    seed = NULL) {
  if (!is.numeric(ttts_beta) || length(ttts_beta) != 1L || is.na(ttts_beta)) {
    stop("`ttts_beta` must be a numeric scalar.", call. = FALSE)
  }
  if (ttts_beta <= 0 || ttts_beta > 1) {
    stop("`ttts_beta` must lie in (0, 1].", call. = FALSE)
  }

  bg_evaluate_actions_method(
    board = board,
    method = "ttts",
    roll = roll,
    legal_moves = legal_moves,
    total_budget = total_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns,
    unresolved_value = unresolved_value,
    initial_allocations = initial_allocations,
    ucb_exploration = as.numeric(ttts_beta),
    prior_alpha = prior_alpha,
    prior_beta = prior_beta,
    dice_mode = dice_mode,
    crn = crn,
    fast_diagnostics = fast_diagnostics,
    trace = trace,
    trace_every = trace_every,
    seed = seed
  )
}

#' Build a high-budget reference estimate for a move-evaluation problem
#'
#' This helper computes an equal-allocation evaluation with a large rollout
#' budget and uses it as a high-budget reference estimate (proxy truth) for
#' benchmarking simple regret, best-move identification, and estimation error.
#'
#' This function supports the package's Thompson-evaluation workflow: finite
#' runs are interpreted relative to this high-budget proxy, not as absolute
#' truth.
#'
#' @inheritParams evaluate_actions_equal
#' @param truth_budget Integer-like scalar giving the total budget used for the
#'   high-budget reference calculation.
#'
#' @return A `bg_action_evaluation` object tagged with classes
#'   `"bg_action_reference"` and `"bg_action_truth"`.
#' @export
approximate_action_truth <- function(
    board,
    roll = NULL,
    legal_moves = NULL,
    truth_budget = 512L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    unresolved_value = 0.5,
    prior_alpha = 1,
    prior_beta = 1,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    seed = NULL) {
  out <- evaluate_actions_equal(
    board = board,
    roll = roll,
    legal_moves = legal_moves,
    total_budget = truth_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns,
    unresolved_value = unresolved_value,
    initial_allocations = 1L,
    ucb_exploration = 1,
    prior_alpha = prior_alpha,
    prior_beta = prior_beta,
    dice_mode = dice_mode,
    crn = crn,
    seed = seed
  )
  out$truth_budget <- as.integer(truth_budget)
  class(out) <- c("bg_action_reference", "bg_action_truth", class(out))
  out
}

#' Build a high-budget action-value reference estimate
#'
#' Alias for [approximate_action_truth()] using explicit reference-estimate
#' terminology.
#'
#' @inheritParams approximate_action_truth
#'
#' @return A `bg_action_evaluation` object tagged as reference output.
#' @export
approximate_action_reference <- function(...) {
  approximate_action_truth(...)
}

bg_extract_truth_values <- function(truth) {
  if (inherits(truth, "bg_action_evaluation")) {
    tab <- truth$results[order(truth$results$candidate_index), , drop = FALSE]
    return(tab$estimate)
  }

  if (is.numeric(truth)) {
    return(as.numeric(truth))
  }

  stop("`truth` must be numeric or a `bg_action_evaluation` object.", call. = FALSE)
}

#' Compute simple regret
#'
#' Computes simple regret under a best-move identification setup. When the best
#' available move has value `v*` and the selected move has value `v`, the simple
#' regret is `v* - v`.
#'
#' @param selected_value Numeric vector of selected-action values.
#' @param best_value Numeric vector of best-action values.
#'
#' @return Numeric vector of regrets.
#' @export
compute_regret <- function(selected_value, best_value) {
  if (!is.numeric(selected_value) || !is.numeric(best_value)) {
    stop("`selected_value` and `best_value` must be numeric.", call. = FALSE)
  }

  best_value - selected_value
}

#' Compute best-action accuracy
#'
#' Computes the proportion of selections that match the best action under an
#' approximate truth benchmark.
#'
#' @param chosen_index Integer-like vector of selected-action indices.
#' @param truth_best_index Integer-like vector of best-action indices under the
#'   benchmark truth.
#'
#' @return Numeric scalar in `[0, 1]`.
#' @export
compute_best_action_accuracy <- function(chosen_index, truth_best_index) {
  chosen_index <- as.integer(chosen_index)
  truth_best_index <- as.integer(truth_best_index)

  if (length(chosen_index) != length(truth_best_index)) {
    stop("`chosen_index` and `truth_best_index` must have the same length.", call. = FALSE)
  }

  mean(chosen_index == truth_best_index, na.rm = TRUE)
}

#' Compute mean squared error
#'
#' Computes the mean squared error between an estimated move-value vector and a
#' truth vector.
#'
#' @param estimate Numeric vector of estimates.
#' @param truth Numeric vector of benchmark truth values.
#'
#' @return Numeric scalar.
#' @export
compute_mse <- function(estimate, truth) {
  if (!is.numeric(estimate) || !is.numeric(truth)) {
    stop("`estimate` and `truth` must be numeric.", call. = FALSE)
  }

  if (length(estimate) != length(truth)) {
    stop("`estimate` and `truth` must have the same length.", call. = FALSE)
  }

  mean((estimate - truth)^2)
}

#' Compute probability of correct selection
#'
#' Alias of [compute_best_action_accuracy()] for best-action identification
#' experiments.
#'
#' @inheritParams compute_best_action_accuracy
#'
#' @return Numeric scalar in `[0, 1]`.
#' @export
compute_probability_of_correct_selection <- function(chosen_index, truth_best_index) {
  compute_best_action_accuracy(chosen_index, truth_best_index)
}

#' Compute simple regret
#'
#' Alias of [compute_regret()] used in benchmark summaries.
#'
#' @inheritParams compute_regret
#'
#' @return Numeric vector of regrets.
#' @export
compute_simple_regret <- function(selected_value, best_value) {
  compute_regret(selected_value, best_value)
}

#' Compute value-estimation mean squared error
#'
#' Alias of [compute_mse()] for action-value estimation experiments.
#'
#' @inheritParams compute_mse
#'
#' @return Numeric scalar.
#' @export
compute_value_mse <- function(estimate, truth) {
  compute_mse(estimate, truth)
}

#' Stratify positions by difficulty
#'
#' Assigns a difficulty label using the gap between the best and second-best
#' approximate truth values. Smaller gaps indicate harder best-move
#' identification problems.
#'
#' @param x Numeric vector of gaps, or a data frame with a `difficulty_gap`
#'   column.
#' @param breaks Numeric vector of cut points.
#' @param labels Character vector of labels. By default, smaller gaps are
#'   labelled `"hard"` and larger gaps are labelled `"easy"`.
#'
#' @return An ordered factor if `x` is numeric. If `x` is a data frame, returns
#'   the data frame with an added `difficulty_label` column.
#' @export
stratify_positions_by_difficulty <- function(
    x,
    breaks = c(-Inf, 0.02, 0.05, Inf),
    labels = c("hard", "moderate", "easy")) {
  if (is.data.frame(x)) {
    if (!"difficulty_gap" %in% names(x)) {
      stop("When `x` is a data frame it must contain a `difficulty_gap` column.", call. = FALSE)
    }
    x$difficulty_label <- stratify_positions_by_difficulty(x$difficulty_gap, breaks = breaks, labels = labels)
    return(x)
  }

  if (!is.numeric(x)) {
    stop("`x` must be numeric or a data frame with `difficulty_gap`.", call. = FALSE)
  }

  cut(x, breaks = breaks, labels = labels, right = TRUE, ordered_result = TRUE)
}

bg_summarize_benchmark_results <- function(results) {
  first_mode_label <- function(x) {
    x <- as.character(x)
    x <- x[!is.na(x) & nzchar(x)]
    if (length(x) == 0L) {
      return(NA_character_)
    }
    counts <- sort(table(x), decreasing = TRUE)
    as.character(names(counts)[[1L]])
  }

  unique_or_varies <- function(x) {
    x <- as.character(x)
    x <- unique(x[!is.na(x) & nzchar(x)])
    if (length(x) == 0L) {
      return(NA_character_)
    }
    if (length(x) == 1L) {
      return(x[[1L]])
    }
    "<varies>"
  }

  # Summaries are produced per method/budget/variance-control configuration.
  grouping_cols <- c("method", "total_budget", "dice_mode", "crn")
  grouping_cols <- grouping_cols[grouping_cols %in% names(results)]

  if (length(grouping_cols) == 0L) {
    stop("Internal error: benchmark results must contain at least a `method` column.", call. = FALSE)
  }

  keys <- unique(results[, grouping_cols, drop = FALSE])
  out <- vector("list", nrow(keys))

  for (i in seq_len(nrow(keys))) {
    key <- keys[i, , drop = FALSE]
    idx <- rep(TRUE, nrow(results))
    for (nm in grouping_cols) {
      idx <- idx & (results[[nm]] == key[[nm]])
    }
    rows <- results[idx, , drop = FALSE]
    reference_best_label_col <- if ("reference_best_move_label" %in% names(rows)) {
      "reference_best_move_label"
    } else {
      "truth_best_move_label"
    }

    # Correct-selection rate is PCS when aggregated over many cases.
    correct_selection_rate <- if (all(is.na(rows$correct_selection))) {
      NA_real_
    } else {
      mean(rows$correct_selection, na.rm = TRUE)
    }

    out[[i]] <- cbind(
      key,
      data.frame(
        total_cases = nrow(rows),
        multi_move_cases = sum(rows$n_legal_moves > 1L, na.rm = TRUE),
        correct_selection_count = sum(rows$correct_selection %in% TRUE, na.rm = TRUE),
        correct_selection_rate = correct_selection_rate,
        reference_best_move_label = unique_or_varies(rows[[reference_best_label_col]]),
        most_selected_move_label = first_mode_label(rows$chosen_move_label),
        mean_n_legal_moves = mean(rows$n_legal_moves),
        probability_correct_selection = correct_selection_rate,
        mean_simple_regret = if (all(is.na(rows$simple_regret))) NA_real_ else mean(rows$simple_regret, na.rm = TRUE),
        mean_mse = if (all(is.na(rows$mse))) NA_real_ else mean(rows$mse, na.rm = TRUE),
        mean_runtime_seconds = if (all(is.na(rows$runtime_seconds))) NA_real_ else mean(rows$runtime_seconds, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    )
  }

  summary_df <- do.call(rbind, out)
  # Backward-compatible aliases kept for existing user code/tests.
  summary_df$n_cases <- summary_df$total_cases
  summary_df$decision_cases <- summary_df$multi_move_cases
  rownames(summary_df) <- NULL
  summary_df
}

bg_new_allocation_benchmark <- function(x) {
  x$results <- as.data.frame(x$results, stringsAsFactors = FALSE)
  x$summary <- as.data.frame(x$summary, stringsAsFactors = FALSE)
  x$truth <- as.data.frame(x$truth, stringsAsFactors = FALSE)
  rownames(x$results) <- NULL
  rownames(x$summary) <- NULL
  rownames(x$truth) <- NULL
  structure(x, class = "bg_allocation_benchmark")
}

#' Benchmark rollout-allocation methods on benchmark cases
#'
#' Compares fixed-budget allocation rules on a collection of local backgammon
#' decision problems. The function first computes a large-budget approximate
#' truth for each case, then evaluates selected allocation methods under smaller
#' budgets and reports selection accuracy, simple regret, MSE, and runtime.
#'
#' The design can be crossed over:
#'
#' - allocation method;
#' - rollout budget;
#' - dice variance-reduction mode;
#' - common-random-number (CRN) setting.
#'
#' @param cases A list of `bg_benchmark_case` objects, each containing a
#'   `board`, `roll`, and optional `case_id`.
#' @param methods Character vector of allocation methods. Supported values
#'   include `"equal"`, `"greedy"`, `"ucb"`, `"thompson"`, and `"ocba"`.
#' @param total_budget Integer-like scalar used when `budgets` is `NULL`.
#' @param truth_budget Integer-like scalar giving the large rollout budget used
#'   to approximate truth for each case.
#' @param budgets Optional integer-like vector of budgets for crossed
#'   experiments. If `NULL`, uses `total_budget`.
#' @param dice_modes Character vector of dice modes to compare.
#' @param crn_values Logical vector indicating whether to run with `crn = FALSE`
#'   and/or `crn = TRUE`.
#' @param truth_dice_mode Dice mode used for truth construction.
#' @param truth_crn Logical scalar indicating whether truth construction uses
#'   common random numbers.
#' @inheritParams evaluate_actions_equal
#'
#' @return A `bg_allocation_benchmark` object with rectangular `results`,
#'   grouped `summary`, case-level `truth`, and `settings`.
#' @export
#'
#' @examples
#' points <- integer(24)
#' points[8] <- 1L
#' points[17] <- -1L
#' case <- bg_benchmark_case(
#'   board = bg_position(points = points, off = c(14L, 14L), turn = 1L),
#'   roll = bg_roll(3, 2),
#'   case_id = "demo"
#' )
#' benchmark_allocation_methods(
#'   cases = list(case),
#'   methods = c("thompson", "ttts", "equal", "ocba"),
#'   budgets = c(8L, 16L),
#'   truth_budget = 64L,
#'   seed = 123
#' )
benchmark_allocation_methods <- function(
    cases,
    methods = c("thompson", "ttts", "equal", "ucb", "ocba", "greedy"),
    total_budget = 32L,
    truth_budget = 256L,
    budgets = NULL,
    dice_modes = c("iid"),
    crn_values = c(FALSE),
    truth_dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    truth_crn = FALSE,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    unresolved_value = 0.5,
    initial_allocations = 1L,
    ucb_exploration = 1,
    prior_alpha = 1,
    prior_beta = 1,
    seed = NULL) {
  if (!is.list(cases) || length(cases) < 1L) {
    stop("`cases` must be a non-empty list of `bg_benchmark_case` objects.", call. = FALSE)
  }

  methods <- unique(vapply(methods, bg_match_allocation_method, character(1L), USE.NAMES = FALSE))
  methods <- vapply(methods, bg_canonicalize_allocation_method, character(1L), USE.NAMES = FALSE)

  total_budget <- bg_coerce_integerish(total_budget, "total_budget", 1L)
  if (is.null(budgets)) {
    budgets <- total_budget
  } else {
    budgets <- bg_coerce_integerish(budgets, "budgets", length(budgets))
    if (any(budgets < 1L)) {
      stop("All elements of `budgets` must be at least 1.", call. = FALSE)
    }
  }
  budgets <- as.integer(unique(budgets))

  truth_budget <- bg_coerce_integerish(truth_budget, "truth_budget", 1L)
  if (truth_budget < max(budgets)) {
    stop("`truth_budget` should be at least as large as `max(budgets)`.", call. = FALSE)
  }

  valid_dice_modes <- c("iid", "stratified_first_roll", "stratified_first_two_rolls")
  if (!is.character(dice_modes) || length(dice_modes) < 1L || anyNA(dice_modes)) {
    stop("`dice_modes` must be a non-empty character vector.", call. = FALSE)
  }
  if (!all(dice_modes %in% valid_dice_modes)) {
    stop(
      sprintf(
        "`dice_modes` values must be one of: %s.",
        paste(valid_dice_modes, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  dice_modes <- unique(dice_modes)

  if (!is.logical(crn_values) || length(crn_values) < 1L || anyNA(crn_values)) {
    stop("`crn_values` must be a non-empty logical vector.", call. = FALSE)
  }
  crn_values <- unique(as.logical(crn_values))

  truth_dice_mode <- match.arg(truth_dice_mode)
  bg_assert_scalar_flag(truth_crn, "truth_crn")

  alloc_args <- bg_normalize_allocation_args(
    total_budget = max(budgets),
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns,
    unresolved_value = unresolved_value,
    initial_allocations = initial_allocations,
    ucb_exploration = ucb_exploration,
    prior_alpha = prior_alpha,
    prior_beta = prior_beta
  )

  rows <- list()
  truth_rows <- list()
  row_id <- 1L
  truth_id <- 1L

  for (i in seq_along(cases)) {
    case <- cases[[i]]
    if (!is_bg_benchmark_case(case)) {
      stop("Each element of `cases` must inherit from class 'bg_benchmark_case'.", call. = FALSE)
    }

    case_id <- case$case_id
    board <- case$board
    roll <- case$roll
    legal_moves <- bg_legal_moves(board, roll)
    n_legal_moves <- length(legal_moves)

    if (n_legal_moves == 0L) {
      truth_rows[[truth_id]] <- data.frame(
        case_id = case_id,
        n_legal_moves = 0L,
        reference_best_index = NA_integer_,
        reference_best_move_label = NA_character_,
        reference_best_value = NA_real_,
        reference_second_best_value = NA_real_,
        truth_best_index = NA_integer_,
        truth_best_move_label = NA_character_,
        truth_best_value = NA_real_,
        truth_second_best_value = NA_real_,
        difficulty_gap = NA_real_,
        difficulty_label = factor(NA, levels = c("hard", "moderate", "easy"), ordered = TRUE),
        stringsAsFactors = FALSE
      )
      truth_id <- truth_id + 1L

      for (budget in budgets) {
        for (dice_mode in dice_modes) {
          for (crn in crn_values) {
            for (method in methods) {
              rows[[row_id]] <- data.frame(
                case_id = case_id,
                method = method,
                total_budget = budget,
                dice_mode = dice_mode,
                crn = crn,
              n_legal_moves = 0L,
              chosen_index = NA_integer_,
              chosen_move_label = NA_character_,
              reference_best_index = NA_integer_,
              reference_best_move_label = NA_character_,
              truth_best_index = NA_integer_,
              truth_best_move_label = NA_character_,
              correct_selection = NA,
              selected_reference_value = NA_real_,
              best_reference_value = NA_real_,
              selected_truth_value = NA_real_,
              best_truth_value = NA_real_,
                simple_regret = NA_real_,
                mse = NA_real_,
                runtime_seconds = 0,
                difficulty_gap = NA_real_,
                difficulty_label = factor(NA, levels = c("hard", "moderate", "easy"), ordered = TRUE),
                stringsAsFactors = FALSE
              )
              row_id <- row_id + 1L
            }
          }
        }
      }
      next
    }

    truth_eval <- approximate_action_truth(
      board = board,
      roll = roll,
      legal_moves = legal_moves,
      truth_budget = truth_budget,
      rollout_policy = alloc_args$rollout_policy,
      max_rollout_turns = alloc_args$max_rollout_turns,
      unresolved_value = alloc_args$unresolved_value,
      prior_alpha = alloc_args$prior_alpha,
      prior_beta = alloc_args$prior_beta,
      dice_mode = truth_dice_mode,
      crn = truth_crn,
      seed = bg_derive_seed(seed, case_id, "truth")
    )

    truth_tab <- truth_eval$results[order(truth_eval$results$candidate_index), , drop = FALSE]
    truth_values <- truth_tab$estimate
    truth_lookup <- stats::setNames(truth_values, truth_tab$candidate_index)
    truth_label_lookup <- stats::setNames(as.character(truth_tab$move_label), truth_tab$candidate_index)
    best_index <- truth_tab$candidate_index[[which.max(truth_values)]]
    best_move_label <- truth_label_lookup[[as.character(best_index)]]
    sorted_truth <- sort(truth_values, decreasing = TRUE)
    second_best <- if (length(sorted_truth) > 1L) sorted_truth[[2L]] else sorted_truth[[1L]]
    difficulty_gap <- sorted_truth[[1L]] - second_best
    difficulty_label <- stratify_positions_by_difficulty(difficulty_gap)

    truth_rows[[truth_id]] <- data.frame(
      case_id = case_id,
      n_legal_moves = n_legal_moves,
      reference_best_index = best_index,
      reference_best_move_label = best_move_label,
      reference_best_value = sorted_truth[[1L]],
      reference_second_best_value = second_best,
      truth_best_index = best_index,
      truth_best_move_label = best_move_label,
      truth_best_value = sorted_truth[[1L]],
      truth_second_best_value = second_best,
      difficulty_gap = difficulty_gap,
      difficulty_label = difficulty_label,
      stringsAsFactors = FALSE
    )
    truth_id <- truth_id + 1L

    # Fully crossed benchmark grid: method x budget x dice_mode x CRN.
    for (budget in budgets) {
      for (dice_mode in dice_modes) {
        for (crn in crn_values) {
          for (method in methods) {
            start <- as.numeric(proc.time()[3L])
            eval <- bg_evaluate_actions_method(
              board = board,
              method = method,
              roll = roll,
              legal_moves = legal_moves,
              total_budget = budget,
              rollout_policy = alloc_args$rollout_policy,
              max_rollout_turns = alloc_args$max_rollout_turns,
              unresolved_value = alloc_args$unresolved_value,
              initial_allocations = alloc_args$initial_allocations,
              ucb_exploration = alloc_args$ucb_exploration,
              prior_alpha = alloc_args$prior_alpha,
              prior_beta = alloc_args$prior_beta,
              dice_mode = dice_mode,
              crn = crn,
              seed = bg_derive_seed(seed, case_id, method, budget, dice_mode, crn)
            )
            runtime_seconds <- as.numeric(proc.time()[3L] - start)

            eval_tab <- eval$results[order(eval$results$candidate_index), , drop = FALSE]
            selected_index <- eval$recommended_index
            selected_move_label <- eval_tab$move_label[[match(selected_index, eval_tab$candidate_index)]]
            selected_truth_value <- truth_lookup[[as.character(selected_index)]]
            if (is.null(selected_truth_value)) {
              stop("Internal error: selected move index not found in truth table.", call. = FALSE)
            }
            best_truth_value <- max(truth_values)
            simple_regret <- compute_regret(selected_truth_value, best_truth_value)
            mse <- compute_mse(eval_tab$estimate, truth_values)

            rows[[row_id]] <- data.frame(
              case_id = case_id,
              method = method,
              total_budget = budget,
              dice_mode = dice_mode,
              crn = crn,
              n_legal_moves = n_legal_moves,
              chosen_index = selected_index,
              chosen_move_label = selected_move_label,
              reference_best_index = best_index,
              reference_best_move_label = best_move_label,
              truth_best_index = best_index,
              truth_best_move_label = best_move_label,
              correct_selection = selected_index == best_index,
              selected_reference_value = selected_truth_value,
              best_reference_value = best_truth_value,
              selected_truth_value = selected_truth_value,
              best_truth_value = best_truth_value,
              simple_regret = simple_regret,
              mse = mse,
              runtime_seconds = runtime_seconds,
              difficulty_gap = difficulty_gap,
              difficulty_label = difficulty_label,
              stringsAsFactors = FALSE
            )
            row_id <- row_id + 1L
          }
        }
      }
    }
  }

  results <- if (length(rows) == 0L) {
    data.frame(
      case_id = character(0L),
      method = character(0L),
      total_budget = integer(0L),
      dice_mode = character(0L),
      crn = logical(0L),
      n_legal_moves = integer(0L),
      chosen_index = integer(0L),
      chosen_move_label = character(0L),
      reference_best_index = integer(0L),
      reference_best_move_label = character(0L),
      truth_best_index = integer(0L),
      truth_best_move_label = character(0L),
      correct_selection = logical(0L),
      selected_reference_value = numeric(0L),
      best_reference_value = numeric(0L),
      selected_truth_value = numeric(0L),
      best_truth_value = numeric(0L),
      simple_regret = numeric(0L),
      mse = numeric(0L),
      runtime_seconds = numeric(0L),
      difficulty_gap = numeric(0L),
      difficulty_label = character(0L),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, rows)
  }

  truth <- if (length(truth_rows) == 0L) {
    data.frame(
      case_id = character(0L),
      n_legal_moves = integer(0L),
      reference_best_index = integer(0L),
      reference_best_move_label = character(0L),
      reference_best_value = numeric(0L),
      reference_second_best_value = numeric(0L),
      truth_best_index = integer(0L),
      truth_best_move_label = character(0L),
      truth_best_value = numeric(0L),
      truth_second_best_value = numeric(0L),
      difficulty_gap = numeric(0L),
      difficulty_label = character(0L),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, truth_rows)
  }

  summary <- if (nrow(results) == 0L) {
    data.frame(
      method = character(0L),
      total_budget = integer(0L),
      dice_mode = character(0L),
      crn = logical(0L),
      total_cases = integer(0L),
      multi_move_cases = integer(0L),
      correct_selection_count = integer(0L),
      correct_selection_rate = numeric(0L),
      reference_best_move_label = character(0L),
      most_selected_move_label = character(0L),
      n_cases = integer(0L),
      decision_cases = integer(0L),
      mean_n_legal_moves = numeric(0L),
      probability_correct_selection = numeric(0L),
      mean_simple_regret = numeric(0L),
      mean_mse = numeric(0L),
      mean_runtime_seconds = numeric(0L),
      stringsAsFactors = FALSE
    )
  } else {
    bg_summarize_benchmark_results(results)
  }

  bg_new_allocation_benchmark(list(
    results = results,
    summary = summary,
    truth = truth,
    settings = c(
      alloc_args,
      list(
        methods = methods,
        budgets = budgets,
        dice_modes = dice_modes,
        crn_values = crn_values,
        truth_budget = truth_budget,
        truth_dice_mode = truth_dice_mode,
        truth_crn = truth_crn,
        seed = if (is.null(seed)) NULL else seed
      )
    )
  ))
}

#' Summarize an action-evaluation object
#'
#' Returns a one-row decision summary focused on the recommended action and key
#' diagnostics used in finite-budget best-action identification.
#'
#' @param object A `bg_action_evaluation` object.
#' @param ... Unused.
#'
#' @return A one-row data frame.
#' @export
summary.bg_action_evaluation <- function(object, ...) {
  if (!inherits(object, "bg_action_evaluation")) {
    stop("`object` must inherit from class 'bg_action_evaluation'.", call. = FALSE)
  }

  if (nrow(object$results) == 0L) {
    return(
      data.frame(
        method = object$method,
        total_budget = object$settings$total_budget,
        n_candidates = 0L,
        recommended_index = NA_integer_,
        recommended_move_label = NA_character_,
        recommended_estimate = NA_real_,
        recommended_prob_best = NA_real_,
        recommended_expected_regret = NA_real_,
        recommended_allocation_count = NA_integer_,
        runtime_seconds = object$runtime_seconds,
        stringsAsFactors = FALSE
      )
    )
  }

  rec <- object$results[object$results$candidate_index == object$recommended_index, , drop = FALSE]
  if (nrow(rec) != 1L) {
    rec <- object$results[1L, , drop = FALSE]
  }

  data.frame(
    method = object$method,
    total_budget = object$settings$total_budget,
    n_candidates = nrow(object$results),
    recommended_index = object$recommended_index,
    recommended_move_label = rec$move_label[[1L]],
    recommended_estimate = rec$estimate[[1L]],
    recommended_prob_best = if ("prob_best" %in% names(rec)) rec$prob_best[[1L]] else NA_real_,
    recommended_expected_regret = if ("posterior_expected_regret" %in% names(rec)) {
      rec$posterior_expected_regret[[1L]]
    } else {
      NA_real_
    },
    recommended_allocation_count = if ("allocation_count" %in% names(rec)) rec$allocation_count[[1L]] else NA_integer_,
    runtime_seconds = object$runtime_seconds,
    stringsAsFactors = FALSE
  )
}

#' Print an action-evaluation object
#'
#' Prints a compact action table that emphasizes recommendation, allocation,
#' estimate, uncertainty, probability-best, and regret diagnostics.
#'
#' @param x A `bg_action_evaluation` object.
#' @param n Integer-like scalar controlling how many action rows to print.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.bg_action_evaluation <- function(x, n = 10L, ...) {
  if (!inherits(x, "bg_action_evaluation")) {
    stop("`x` must inherit from class 'bg_action_evaluation'.", call. = FALSE)
  }

  cat("<bg_action_evaluation>\n", sep = "")
  cat("method:       ", x$method, "\n", sep = "")
  cat("candidates:   ", nrow(x$results), "\n", sep = "")
  cat("total_budget: ", x$settings$total_budget, "\n", sep = "")
  if (nrow(x$results) > 0L) {
    rec <- x$results[x$results$candidate_index == x$recommended_index, , drop = FALSE]
    if (nrow(rec) == 1L) {
      cat("recommended:  ", rec$move_label[[1L]], "\n", sep = "")
    }
  }
  if (!is.null(x$runtime_seconds) && !is.na(x$runtime_seconds)) {
    cat("runtime:      ", format(x$runtime_seconds, digits = 6), " seconds\n", sep = "")
  }

  if (nrow(x$results) > 0L) {
    compact <- bg_compact_action_table(x$results, n = n)
    print(compact, row.names = FALSE)
    if (nrow(x$results) > nrow(compact)) {
      cat("showing_first: ", nrow(compact), " of ", nrow(x$results), " candidates\n", sep = "")
    }
    if ("prob_best" %in% names(x$results) && all(is.na(x$results$prob_best))) {
      cat("note: `prob_best` and regret diagnostics are unavailable (set `fast_diagnostics = FALSE`).\n", sep = "")
    }
  } else {
    cat("No legal moves were available.\n", sep = "")
  }

  invisible(x)
}

#' Print an allocation benchmark object
#'
#' @param x A `bg_allocation_benchmark` object.
#' @param n Integer-like scalar controlling how many summary rows to print.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.bg_allocation_benchmark <- function(x, n = 20L, ...) {
  if (!inherits(x, "bg_allocation_benchmark")) {
    stop("`x` must inherit from class 'bg_allocation_benchmark'.", call. = FALSE)
  }

  cat("<bg_allocation_benchmark>\n", sep = "")
  cat("methods:      ", paste(x$settings$methods, collapse = ", "), "\n", sep = "")
  if (!is.null(x$settings$budgets)) {
    cat("budgets:      ", paste(x$settings$budgets, collapse = ", "), "\n", sep = "")
  } else {
    cat("total_budget: ", x$settings$total_budget, "\n", sep = "")
  }
  if (!is.null(x$settings$dice_modes)) {
    cat("dice_modes:   ", paste(x$settings$dice_modes, collapse = ", "), "\n", sep = "")
  }
  if (!is.null(x$settings$crn_values)) {
    cat("crn_values:   ", paste(as.character(x$settings$crn_values), collapse = ", "), "\n", sep = "")
  }
  cat("truth_budget: ", x$settings$truth_budget, "\n", sep = "")
  compact <- bg_compact_benchmark_summary(x$summary, n = n)
  print(compact, row.names = FALSE)
  if (nrow(x$summary) > nrow(compact)) {
    cat("showing_first: ", nrow(compact), " of ", nrow(x$summary), " summary rows\n", sep = "")
  }
  invisible(x)
}

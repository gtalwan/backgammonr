# Legacy compatibility layer.
#
# These helpers are retained for backward compatibility and regression tests,
# but they are intentionally quarantined away from the main research-facing
# package narrative.

# -----------------------------------------------------------------------------
# Source: bg_legacy_allocation.R
# -----------------------------------------------------------------------------
# Legacy scalar-engine allocation wrappers and benchmark-facing method adapters.
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

bg_as_named_character <- function(values, keys) {
  stats::setNames(as.character(values), as.character(keys))
}

bg_results_by_candidate <- function(x) {
  results <- if (inherits(x, "bg_action_evaluation")) x$results else x
  results <- as.data.frame(results, stringsAsFactors = FALSE)
  if (nrow(results) == 0L || !"candidate_index" %in% names(results)) {
    return(results)
  }

  results[order(results$candidate_index), , drop = FALSE]
}

bg_recommended_row <- function(x) {
  results <- bg_results_by_candidate(x)
  if (nrow(results) == 0L) {
    return(results)
  }

  if ("recommended" %in% names(results)) {
    hit <- results[results$recommended, , drop = FALSE]
    if (nrow(hit) > 0L) {
      return(hit[1L, , drop = FALSE])
    }
  }

  if (inherits(x, "bg_action_evaluation") &&
      !is.null(x$recommended_index) &&
      length(x$recommended_index) > 0L) {
    hit <- results[results$candidate_index == x$recommended_index[[1L]], , drop = FALSE]
    if (nrow(hit) > 0L) {
      return(hit[1L, , drop = FALSE])
    }
  }

  results[1L, , drop = FALSE]
}

bg_action_runtime_seconds <- function(evaluation) {
  if (is.null(evaluation$runtime_seconds) || length(evaluation$runtime_seconds) == 0L) {
    return(NA_real_)
  }

  as.numeric(evaluation$runtime_seconds[[1L]])
}

bg_candidate_label_lookup <- function(results) {
  if (nrow(results) == 0L) {
    return(stats::setNames(character(0L), character(0L)))
  }

  bg_as_named_character(results$move_label, results$candidate_index)
}

bg_top_two_values <- function(values) {
  values <- sort(as.numeric(values), decreasing = TRUE)
  if (length(values) == 0L) {
    return(c(best = NA_real_, second_best = NA_real_))
  }
  if (length(values) == 1L) {
    return(c(best = values[[1L]], second_best = values[[1L]]))
  }

  c(best = values[[1L]], second_best = values[[2L]])
}

bg_top_two_gap <- function(values) {
  top_two <- bg_top_two_values(values)
  if (anyNA(top_two)) {
    return(NA_real_)
  }

  unname(top_two[["best"]] - top_two[["second_best"]])
}

bg_reference_snapshot <- function(reference) {
  tab <- bg_results_by_candidate(reference)
  estimate_values <- if (nrow(tab) == 0L || !"estimate" %in% names(tab)) numeric(0L) else tab$estimate
  top_two <- bg_top_two_values(estimate_values)
  difficulty_gap <- if (anyNA(top_two)) NA_real_ else unname(top_two[["best"]] - top_two[["second_best"]])

  list(
    table = tab,
    values = estimate_values,
    value_lookup = bg_as_named_numeric(estimate_values, tab$candidate_index),
    label_lookup = bg_candidate_label_lookup(tab),
    best_value = unname(top_two[["best"]]),
    second_best_value = unname(top_two[["second_best"]]),
    difficulty_gap = difficulty_gap,
    difficulty_label = stratify_positions_by_difficulty(difficulty_gap)
  )
}

bg_action_reference_metrics <- function(evaluation, reference_snapshot, reference_best_index) {
  eval_table <- bg_results_by_candidate(evaluation)
  if (nrow(eval_table) == 0L) {
    return(list(
      evaluation_table = eval_table,
      chosen_index = NA_integer_,
      chosen_move_label = NA_character_,
      chosen_estimate = NA_real_,
      chosen_allocation_count = NA_integer_,
      chosen_prob_best = NA_real_,
      chosen_expected_regret = NA_real_,
      chosen_reference_value = NA_real_,
      correct_selection = NA,
      simple_regret = NA_real_,
      mse = NA_real_,
      n_legal_moves = 0L
    ))
  }

  if (!identical(eval_table$candidate_index, reference_snapshot$table$candidate_index)) {
    stop("Internal error: candidate sets differ between evaluation and reference runs.", call. = FALSE)
  }

  chosen_row <- bg_recommended_row(evaluation)
  chosen_index <- evaluation$recommended_index
  chosen_reference_value <- reference_snapshot$value_lookup[[as.character(chosen_index)]]
  if (is.null(chosen_reference_value)) {
    stop("Internal error: selected candidate was not found in reference lookup.", call. = FALSE)
  }

  list(
    evaluation_table = eval_table,
    chosen_index = chosen_index,
    chosen_move_label = chosen_row$move_label[[1L]],
    chosen_estimate = chosen_row$estimate[[1L]],
    chosen_allocation_count = if ("allocation_count" %in% names(chosen_row)) {
      chosen_row$allocation_count[[1L]]
    } else {
      NA_integer_
    },
    chosen_prob_best = if ("prob_best" %in% names(chosen_row)) chosen_row$prob_best[[1L]] else NA_real_,
    chosen_expected_regret = if ("posterior_expected_regret" %in% names(chosen_row)) {
      chosen_row$posterior_expected_regret[[1L]]
    } else {
      NA_real_
    },
    chosen_reference_value = chosen_reference_value,
    correct_selection = chosen_index == reference_best_index,
    simple_regret = compute_simple_regret(chosen_reference_value, reference_snapshot$best_value),
    mse = compute_mse(eval_table$estimate, reference_snapshot$values),
    n_legal_moves = nrow(eval_table)
  )
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

approximate_action_reference <- function(...) {
  approximate_action_truth(...)
}

bg_extract_truth_values <- function(truth) {
  if (inherits(truth, "bg_action_evaluation")) {
    tab <- bg_results_by_candidate(truth)
    return(tab$estimate)
  }

  if (is.numeric(truth)) {
    return(as.numeric(truth))
  }

  stop("`truth` must be numeric or a `bg_action_evaluation` object.", call. = FALSE)
}

compute_regret <- function(selected_value, best_value) {
  if (!is.numeric(selected_value) || !is.numeric(best_value)) {
    stop("`selected_value` and `best_value` must be numeric.", call. = FALSE)
  }

  best_value - selected_value
}

compute_best_action_accuracy <- function(chosen_index, truth_best_index) {
  chosen_index <- as.integer(chosen_index)
  truth_best_index <- as.integer(truth_best_index)

  if (length(chosen_index) != length(truth_best_index)) {
    stop("`chosen_index` and `truth_best_index` must have the same length.", call. = FALSE)
  }

  mean(chosen_index == truth_best_index, na.rm = TRUE)
}

compute_mse <- function(estimate, truth) {
  if (!is.numeric(estimate) || !is.numeric(truth)) {
    stop("`estimate` and `truth` must be numeric.", call. = FALSE)
  }

  if (length(estimate) != length(truth)) {
    stop("`estimate` and `truth` must have the same length.", call. = FALSE)
  }

  mean((estimate - truth)^2)
}

compute_probability_of_correct_selection <- function(chosen_index, truth_best_index) {
  compute_best_action_accuracy(chosen_index, truth_best_index)
}

compute_simple_regret <- function(selected_value, best_value) {
  compute_regret(selected_value, best_value)
}

compute_value_mse <- function(estimate, truth) {
  compute_mse(estimate, truth)
}

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

    truth_info <- bg_reference_snapshot(truth_eval)
    best_index <- truth_info$table$candidate_index[[which.max(truth_info$values)]]
    best_move_label <- truth_info$label_lookup[[as.character(best_index)]]
    difficulty_gap <- truth_info$difficulty_gap
    difficulty_label <- truth_info$difficulty_label

    truth_rows[[truth_id]] <- data.frame(
      case_id = case_id,
      n_legal_moves = n_legal_moves,
      reference_best_index = best_index,
      reference_best_move_label = best_move_label,
      reference_best_value = truth_info$best_value,
      reference_second_best_value = truth_info$second_best_value,
      truth_best_index = best_index,
      truth_best_move_label = best_move_label,
      truth_best_value = truth_info$best_value,
      truth_second_best_value = truth_info$second_best_value,
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
            runtime_seconds <- bg_action_runtime_seconds(eval)
            metrics <- bg_action_reference_metrics(
              evaluation = eval,
              reference_snapshot = truth_info,
              reference_best_index = best_index
            )

            rows[[row_id]] <- data.frame(
              case_id = case_id,
              method = method,
              total_budget = budget,
              dice_mode = dice_mode,
              crn = crn,
              n_legal_moves = metrics$n_legal_moves,
              chosen_index = metrics$chosen_index,
              chosen_move_label = metrics$chosen_move_label,
              reference_best_index = best_index,
              reference_best_move_label = best_move_label,
              truth_best_index = best_index,
              truth_best_move_label = best_move_label,
              correct_selection = metrics$correct_selection,
              selected_reference_value = metrics$chosen_reference_value,
              best_reference_value = truth_info$best_value,
              selected_truth_value = metrics$chosen_reference_value,
              best_truth_value = truth_info$best_value,
              simple_regret = metrics$simple_regret,
              mse = metrics$mse,
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

#' @export
#' @noRd
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

#' @export
#' @noRd
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

#' @export
#' @noRd
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

# -----------------------------------------------------------------------------
# Source: bg_legacy_benchmarking.R
# -----------------------------------------------------------------------------
# Legacy benchmarking workflows built around the scalar rollout engine.
bg_normalize_benchmark_methods <- function(methods, arg_name = "methods") {
  if (!is.character(methods) || length(methods) < 1L || anyNA(methods)) {
    stop(sprintf("`%s` must be a non-missing character vector.", arg_name), call. = FALSE)
  }

  methods <- vapply(methods, bg_match_simulation_archetype, character(1L), USE.NAMES = FALSE)

  if (anyDuplicated(methods)) {
    stop(sprintf("`%s` must not contain duplicates.", arg_name), call. = FALSE)
  }

  methods
}

bg_new_matchup_benchmark <- function(x) {
  x$initial_board <- bg_new_board(x$initial_board)
  x$games <- as.data.frame(x$games, stringsAsFactors = FALSE)
  x$summary <- as.data.frame(x$summary, stringsAsFactors = FALSE)
  x$settings <- list(
    player1_selection = as.character(x$settings$player1_selection[[1L]]),
    player2_selection = as.character(x$settings$player2_selection[[1L]]),
    n_games = as.integer(x$settings$n_games[[1L]]),
    max_turns = as.integer(x$settings$max_turns[[1L]]),
    used_scripted_rolls = isTRUE(x$settings$used_scripted_rolls[[1L]]),
    rollout_budget = as.integer(x$settings$rollout_budget[[1L]]),
    rollout_policy = as.character(x$settings$rollout_policy[[1L]]),
    max_rollout_turns = as.integer(x$settings$max_rollout_turns[[1L]]),
    runtime_seconds = as.numeric(x$settings$runtime_seconds[[1L]])
  )
  structure(x, class = "bg_matchup_benchmark")
}

is_bg_matchup_benchmark <- function(x) {
  inherits(x, "bg_matchup_benchmark")
}

benchmark_matchup <- function(
    player1 = c("random", "aggressive", "defensive", "rollout", "equal_rollout", "greedy_rollout", "ucb_rollout", "ocba_rollout", "thompson_rollout", "ttts_rollout"),
    player2 = c("random", "aggressive", "defensive", "rollout", "equal_rollout", "greedy_rollout", "ucb_rollout", "ocba_rollout", "thompson_rollout", "ttts_rollout"),
    n_games = 100L,
    board = bg_initial_board(),
    max_turns = 1000L,
    roll_sequence = NULL,
    seed = NULL,
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

  seed_args <- bg_normalize_seed_args(seed)
  uses_rollout <- bg_is_rollout_family_selection(player1) || bg_is_rollout_family_selection(player2)
  rollout_args <- if (uses_rollout) {
    bg_normalize_rollout_args(
      rollout_budget = rollout_budget,
      rollout_policy = rollout_policy,
      max_rollout_turns = max_rollout_turns
    )
  } else {
    list(rollout_budget = 16L, rollout_policy = "random", max_rollout_turns = 1000L)
  }

  out <- if (is.null(roll_sequence)) {
    bg_cpp_benchmark_matchup_random(
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
    rolls <- bg_normalize_roll_sequence(roll_sequence)
    bg_cpp_benchmark_matchup_scripted(
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
  }

  bg_new_matchup_benchmark(out)
}

#' @export
#' @noRd
summary.bg_matchup_benchmark <- function(object, ...) {
  object$summary
}

bg_new_benchmark_case <- function(x) {
  x$board <- bg_new_board(x$board)
  x$roll <- bg_new_roll(x$roll)
  structure(x, class = "bg_benchmark_case")
}

bg_as_benchmark_case <- function(x, index = NULL) {
  if (is_bg_benchmark_case(x)) {
    return(x)
  }

  if (!is.list(x)) {
    stop("Each benchmark case must be a `bg_benchmark_case` object or a list-like case.", call. = FALSE)
  }

  if (is.null(x$board) || is.null(x$roll)) {
    stop("Each benchmark case must contain `board` and `roll` fields.", call. = FALSE)
  }

  case_id <- x$case_id
  if (is.null(case_id) || (is.character(case_id) && length(case_id) == 1L && !is.na(case_id) && !nzchar(case_id))) {
    case_id <- if (is.null(index)) "case_1" else paste0("case_", index)
  }

  if (!is.character(case_id) || length(case_id) != 1L || is.na(case_id) || !nzchar(case_id)) {
    stop("`case_id` must be a non-missing character scalar when supplied.", call. = FALSE)
  }

  bg_new_benchmark_case(list(
    case_id = case_id,
    board = unclass(bg_as_benchmark_board(x$board)),
    roll = unclass(bg_as_roll(x$roll))
  ))
}

bg_as_benchmark_board <- function(board) {
  if (!is_bg_board(board)) {
    stop("Each benchmark case `board` must be a `bg_board` object.", call. = FALSE)
  }
  bg_validate_board(board)
  board
}

bg_unclass_benchmark_case <- function(x) {
  case <- bg_as_benchmark_case(x)
  list(
    case_id = case$case_id,
    board = unclass(case$board),
    roll = unclass(case$roll)
  )
}

bg_normalize_benchmark_cases <- function(cases) {
  if (is_bg_benchmark_case(cases)) {
    return(list(bg_unclass_benchmark_case(bg_as_benchmark_case(unclass(cases), index = 1L))))
  }

  if (is.list(cases) && all(c("board", "roll") %in% names(cases))) {
    return(list(bg_unclass_benchmark_case(bg_as_benchmark_case(cases, index = 1L))))
  }

  if (!is.list(cases) || length(cases) < 1L) {
    stop("`cases` must be a non-empty list of benchmark cases.", call. = FALSE)
  }

  Map(function(case, idx) bg_unclass_benchmark_case(bg_as_benchmark_case(case, index = idx)), cases, seq_along(cases))
}

is_bg_benchmark_case <- function(x) {
  inherits(x, "bg_benchmark_case")
}

bg_benchmark_case <- function(board, roll, case_id = NULL) {
  board <- bg_as_benchmark_board(board)
  roll <- bg_as_roll(roll)

  if (is.null(case_id)) {
    case_id <- ""
  }

  if (!is.character(case_id) || length(case_id) != 1L || is.na(case_id)) {
    stop("`case_id` must be a non-missing character scalar when supplied.", call. = FALSE)
  }

  bg_new_benchmark_case(list(
    case_id = case_id,
    board = unclass(board),
    roll = unclass(roll)
  ))
}

bg_new_move_evaluation_benchmark <- function(x) {
  x$results <- as.data.frame(x$results, stringsAsFactors = FALSE)
  x$summary <- as.data.frame(x$summary, stringsAsFactors = FALSE)
  reference_method <- as.character(x$settings$reference_method[[1L]])
  if (is.na(reference_method) || identical(reference_method, "")) {
    reference_method <- NULL
  }

  x$settings <- list(
    methods = as.character(x$settings$methods),
    reference_method = reference_method,
    rollout_budget = as.integer(x$settings$rollout_budget[[1L]]),
    rollout_policy = as.character(x$settings$rollout_policy[[1L]]),
    max_rollout_turns = as.integer(x$settings$max_rollout_turns[[1L]]),
    reference_rollout_budget = as.integer(x$settings$reference_rollout_budget[[1L]]),
    reference_rollout_policy = as.character(x$settings$reference_rollout_policy[[1L]]),
    reference_max_rollout_turns = as.integer(x$settings$reference_max_rollout_turns[[1L]])
  )
  structure(x, class = "bg_move_evaluation_benchmark")
}

is_bg_move_evaluation_benchmark <- function(x) {
  inherits(x, "bg_move_evaluation_benchmark")
}

benchmark_move_evaluators <- function(
    cases,
    methods = c("random", "aggressive", "defensive", "rollout", "equal_rollout", "greedy_rollout", "ucb_rollout", "ocba_rollout", "thompson_rollout", "ttts_rollout"),
    reference_method = NULL,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    reference_rollout_budget = rollout_budget,
    reference_rollout_policy = rollout_policy,
    reference_max_rollout_turns = max_rollout_turns,
    seed = NULL) {
  cases <- bg_normalize_benchmark_cases(cases)
  methods <- bg_normalize_benchmark_methods(methods, arg_name = "methods")

  if (is.null(reference_method)) {
    reference_method_string <- ""
  } else {
    if (!is.character(reference_method) || length(reference_method) != 1L || is.na(reference_method)) {
      stop("`reference_method` must be a single non-missing archetype name when supplied.", call. = FALSE)
    }
    reference_method_string <- bg_normalize_benchmark_methods(reference_method, arg_name = "reference_method")
    reference_method_string <- reference_method_string[[1L]]
  }

  methods_use_rollout <- any(vapply(methods, bg_is_rollout_family_selection, logical(1L)))
  reference_uses_rollout <- !identical(reference_method_string, "") && bg_is_rollout_family_selection(reference_method_string)

  rollout_args <- if (methods_use_rollout) {
    bg_normalize_rollout_args(
      rollout_budget = rollout_budget,
      rollout_policy = rollout_policy,
      max_rollout_turns = max_rollout_turns
    )
  } else {
    list(rollout_budget = 16L, rollout_policy = "random", max_rollout_turns = 1000L)
  }
  reference_rollout_args <- if (reference_uses_rollout) {
    bg_normalize_rollout_args(
      rollout_budget = reference_rollout_budget,
      rollout_policy = reference_rollout_policy,
      max_rollout_turns = reference_max_rollout_turns
    )
  } else {
    rollout_args
  }
  seed_args <- bg_normalize_seed_args(seed)

  out <- bg_cpp_benchmark_move_evaluators(
    cases,
    methods,
    reference_method_string,
    rollout_args$rollout_budget,
    rollout_args$rollout_policy,
    rollout_args$max_rollout_turns,
    reference_rollout_args$rollout_budget,
    reference_rollout_args$rollout_policy,
    reference_rollout_args$max_rollout_turns,
    seed_args$seed,
    seed_args$use_seed
  )

  bg_new_move_evaluation_benchmark(out)
}

#' @export
#' @noRd
summary.bg_move_evaluation_benchmark <- function(object, ...) {
  object$summary
}

# -----------------------------------------------------------------------------
# Source: bg_legacy_compat.R
# -----------------------------------------------------------------------------
# Legacy compatibility wrappers kept out of the main package narrative.

initialize_board <- function(...) {
  .Deprecated("bg_position", package = "backgammonr")
  bg_position(...)
}

validate_board <- function(state, error = TRUE) {
  .Deprecated("bg_validate_board", package = "backgammonr")
  bg_validate_board(state, error = error)
}

print_board <- function(board, show_indices = TRUE) {
  .Deprecated("bg_print_board", package = "backgammonr")
  bg_print_board(board, show_indices = show_indices)
}

plot_board <- function(board, ...) {
  .Deprecated("bg_plot_board", package = "backgammonr")
  bg_plot_board(board, ...)
}

roll_dice <- function(n = 1L, seed = NULL) {
  .Deprecated("bg_roll_dice", package = "backgammonr")
  bg_roll_dice(n = n, seed = seed)
}

generate_legal_moves <- function(state, dice, player = NULL) {
  .Deprecated("bg_legal_moves", package = "backgammonr")
  bg_legal_moves(state, dice, player = player)
}

apply_move <- function(state, move) {
  .Deprecated("bg_apply_move_sequence", package = "backgammonr")
  bg_apply_move_sequence(state, move)
}

enumerate_candidate_moves <- function(state, dice) {
  .Deprecated("bg_legal_moves", package = "backgammonr")
  bg_legal_moves(state, dice)
}

# -----------------------------------------------------------------------------
# Source: bg_legacy_ocba_rollout_player.R
# -----------------------------------------------------------------------------
# Legacy OCBA rollout wrappers retained for compatibility with older APIs.
bg_ocba_rollout_evaluate_moves <- function(
    board,
    legal_moves,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    fast_diagnostics = FALSE,
    seed = NULL) {
  evaluate_actions_ocba(
    board = board,
    legal_moves = legal_moves,
    total_budget = rollout_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns,
    fast_diagnostics = fast_diagnostics,
    seed = seed
  )$results
}

bg_ocba_rollout_move <- function(
    board,
    legal_moves,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    fast_diagnostics = FALSE,
    seed = NULL) {
  evaluate_actions_ocba(
    board = board,
    legal_moves = legal_moves,
    total_budget = rollout_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns,
    fast_diagnostics = fast_diagnostics,
    seed = seed
  )$recommended_move
}

bg_play_turn_ocba_rollout_player <- function(
    board,
    roll = NULL,
    seed = NULL,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L) {
  bg_play_turn(
    board = board,
    roll = roll,
    selection = "ocba_rollout",
    seed = seed,
    rollout_budget = rollout_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns
  )
}

bg_play_game_ocba_rollout_players <- function(
    board = bg_initial_board(),
    roll_sequence = NULL,
    max_turns = 1000L,
    seed = NULL,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L) {
  bg_play_game(
    board = board,
    roll_sequence = roll_sequence,
    max_turns = max_turns,
    selection = "ocba_rollout",
    seed = seed,
    rollout_budget = rollout_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns
  )
}

# -----------------------------------------------------------------------------
# Source: bg_legacy_output_tables.R
# -----------------------------------------------------------------------------
# Legacy output-table formatters and report-facing table builders.
bg_has_non_missing <- function(x) {
  # Convenience predicate for dropping entirely empty summary columns.
  any(!is.na(x))
}

bg_truncate_rows <- function(df, n, arg_name = "n") {
  # Optionally keep only the leading rows of a pre-sorted summary table.
  if (is.null(n)) {
    return(df)
  }

  n <- bg_coerce_integerish(n, arg_name, 1L)
  if (n < 1L) {
    stop(sprintf("`%s` must be at least 1.", arg_name), call. = FALSE)
  }

  utils::head(df, n)
}

bg_first_present_column <- function(df, candidates) {
  # Pick the first available column from a preferred-name list.
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0L) {
    return(NULL)
  }
  hit[[1L]]
}

bg_compact_action_table <- function(results, n = NULL, include_interval = TRUE) {
  # Reformat action-level truth/posterior tables into a compact report-ready
  # layout with stable column names.
  results <- as.data.frame(results, stringsAsFactors = FALSE)
  if (nrow(results) == 0L) {
    return(results)
  }

  if ("rank" %in% names(results)) {
    results <- results[order(results$rank), , drop = FALSE]
  } else if (all(c("estimate", "candidate_index") %in% names(results))) {
    results <- results[order(-results$estimate, results$candidate_index), , drop = FALSE]
  }

  results <- bg_truncate_rows(results, n, "n")

  estimate_col <- bg_first_present_column(results, c("estimate", "reference_mean"))
  uncertainty_col <- bg_first_present_column(results, c("posterior_sd", "reference_se"))
  prob_best_col <- bg_first_present_column(results, c("prob_best", "model_relative_prob_best"))
  regret_col <- bg_first_present_column(results, c("posterior_expected_regret", "model_relative_expected_regret"))
  lower_col <- bg_first_present_column(results, c("lower_95", "reference_mc_lower_95"))
  upper_col <- bg_first_present_column(results, c("upper_95", "reference_mc_upper_95"))

  out <- data.frame(
    rank = if ("rank" %in% names(results)) results$rank else seq_len(nrow(results)),
    action_id = if ("candidate_index" %in% names(results)) results$candidate_index else seq_len(nrow(results)),
    action = if ("move_label" %in% names(results)) results$move_label else NA_character_,
    recommended = if ("recommended" %in% names(results)) {
      as.logical(results$recommended)
    } else {
      rep(FALSE, nrow(results))
    },
    alloc_n = if ("allocation_count" %in% names(results)) results$allocation_count else NA_integer_,
    estimate = if (!is.null(estimate_col)) results[[estimate_col]] else NA_real_,
    uncertainty_sd = if (!is.null(uncertainty_col)) results[[uncertainty_col]] else NA_real_,
    prob_best = if (!is.null(prob_best_col)) results[[prob_best_col]] else NA_real_,
    exp_regret = if (!is.null(regret_col)) {
      results[[regret_col]]
    } else {
      NA_real_
    },
    stringsAsFactors = FALSE
  )

  if (isTRUE(include_interval) &&
      !is.null(lower_col) &&
      !is.null(upper_col) &&
      bg_has_non_missing(results[[lower_col]]) &&
      bg_has_non_missing(results[[upper_col]])) {
    out$ci95_low <- results[[lower_col]]
    out$ci95_high <- results[[upper_col]]
  }

  if ("proxy_reference_rank" %in% names(results)) {
    out$reference_rank <- results$proxy_reference_rank
  }
  if ("simple_regret" %in% names(results)) {
    out$simple_regret <- results$simple_regret
  }
  if ("unresolved_fraction" %in% names(results)) {
    out$unresolved_frac <- results$unresolved_fraction
  } else if (all(c("unresolved", "allocation_count") %in% names(results))) {
    out$unresolved_frac <- ifelse(
      results$allocation_count > 0L,
      results$unresolved / results$allocation_count,
      NA_real_
    )
  }

  drop_cols <- character(0L)
  if (!bg_has_non_missing(out$prob_best)) {
    drop_cols <- c(drop_cols, "prob_best")
  }
  if (!bg_has_non_missing(out$exp_regret)) {
    drop_cols <- c(drop_cols, "exp_regret")
  }
  if (!bg_has_non_missing(out$uncertainty_sd)) {
    drop_cols <- c(drop_cols, "uncertainty_sd")
  }
  if ("reference_rank" %in% names(out) && !bg_has_non_missing(out$reference_rank)) {
    drop_cols <- c(drop_cols, "reference_rank")
  }
  if ("simple_regret" %in% names(out) && !bg_has_non_missing(out$simple_regret)) {
    drop_cols <- c(drop_cols, "simple_regret")
  }
  if ("unresolved_frac" %in% names(out) && !bg_has_non_missing(out$unresolved_frac)) {
    drop_cols <- c(drop_cols, "unresolved_frac")
  }
  if ("ci95_low" %in% names(out) && !bg_has_non_missing(out$ci95_low)) {
    drop_cols <- c(drop_cols, "ci95_low", "ci95_high")
  }

  out <- out[, setdiff(names(out), unique(drop_cols)), drop = FALSE]
  out
}

bg_compact_benchmark_summary <- function(summary_df, n = NULL) {
  # Compact one benchmark-study summary to the key cross-method columns.
  summary_df <- as.data.frame(summary_df, stringsAsFactors = FALSE)
  if (nrow(summary_df) == 0L) {
    return(summary_df)
  }

  keep <- intersect(
    c(
      "method",
      "total_budget",
      "probability_correct_selection",
      "mean_simple_regret",
      "mean_mse",
      "mean_runtime_seconds",
      "dice_mode",
      "crn",
      "total_cases",
      "reference_best_move_label",
      "most_selected_move_label"
    ),
    names(summary_df)
  )
  out <- summary_df[, keep, drop = FALSE]
  rename_map <- c(
    probability_correct_selection = "proxy_pcs",
    mean_simple_regret = "simple_regret",
    mean_mse = "mse",
    mean_runtime_seconds = "runtime_seconds",
    reference_best_move_label = "reference_best_action",
    most_selected_move_label = "most_selected_action"
  )
  for (old_nm in names(rename_map)) {
    if (old_nm %in% names(out)) {
      names(out)[names(out) == old_nm] <- rename_map[[old_nm]]
    }
  }
  out <- out[, intersect(
    c(
      "method",
      "total_budget",
      "proxy_pcs",
      "simple_regret",
      "mse",
      "runtime_seconds",
      "dice_mode",
      "crn",
      "total_cases",
      "reference_best_action",
      "most_selected_action"
    ),
    names(out)
  ), drop = FALSE]
  out <- out[order(out$total_budget, out$method), , drop = FALSE]
  bg_truncate_rows(out, n, "n")
}

bg_compact_budget_tradeoff_table <- function(results, n = NULL) {
  # Present one recommendation-vs-budget table with consistent naming across
  # methods and studies.
  results <- as.data.frame(results, stringsAsFactors = FALSE)
  if (nrow(results) == 0L) {
    return(results)
  }

  keep <- intersect(
    c(
      "method",
      "total_budget",
      "chosen_move_label",
      "correct_selection",
      "simple_regret",
      "mse",
      "runtime_seconds",
      "chosen_estimate",
      "chosen_truth_value",
      "truth_best_value"
    ),
    names(results)
  )
  out <- results[, keep, drop = FALSE]
  rename_map <- c(
    chosen_move_label = "recommended_action",
    correct_selection = "proxy_pcs",
    chosen_truth_value = "reference_value_recommended",
    truth_best_value = "reference_best_value"
  )
  for (old_nm in names(rename_map)) {
    if (old_nm %in% names(out)) {
      names(out)[names(out) == old_nm] <- rename_map[[old_nm]]
    }
  }
  out <- out[, intersect(
    c(
      "method",
      "total_budget",
      "recommended_action",
      "proxy_pcs",
      "simple_regret",
      "mse",
      "runtime_seconds",
      "chosen_estimate",
      "reference_value_recommended",
      "reference_best_value"
    ),
    names(out)
  ), drop = FALSE]
  out <- out[order(out$total_budget), , drop = FALSE]
  bg_truncate_rows(out, n, "n")
}

bg_compact_variance_table <- function(results, n = NULL) {
  results <- as.data.frame(results, stringsAsFactors = FALSE)
  if (nrow(results) == 0L) {
    return(results)
  }

  keep <- intersect(
    c(
      "method",
      "total_budget",
      "dice_mode",
      "crn",
      "chosen_move_label",
      "correct_selection",
      "simple_regret",
      "mse",
      "runtime_seconds"
    ),
    names(results)
  )
  out <- results[, keep, drop = FALSE]
  rename_map <- c(
    chosen_move_label = "recommended_action",
    correct_selection = "proxy_pcs"
  )
  for (old_nm in names(rename_map)) {
    if (old_nm %in% names(out)) {
      names(out)[names(out) == old_nm] <- rename_map[[old_nm]]
    }
  }
  out <- out[, intersect(
    c(
      "method",
      "total_budget",
      "dice_mode",
      "crn",
      "recommended_action",
      "proxy_pcs",
      "simple_regret",
      "mse",
      "runtime_seconds"
    ),
    names(out)
  ), drop = FALSE]
  out <- out[order(out$dice_mode, out$crn), , drop = FALSE]
  bg_truncate_rows(out, n, "n")
}

bg_compact_method_comparison_table <- function(summary_df, n = NULL) {
  summary_df <- as.data.frame(summary_df, stringsAsFactors = FALSE)
  if (nrow(summary_df) == 0L) {
    return(summary_df)
  }

  keep <- intersect(
    c(
      "method",
      "recommended_move_label",
      "recommended_allocation_count",
      "recommended_estimate",
      "recommended_posterior_sd",
      "recommended_prob_best",
      "recommended_expected_regret",
      "estimate_gap_top2",
      "runtime_seconds"
    ),
    names(summary_df)
  )
  out <- summary_df[, keep, drop = FALSE]
  rename_map <- c(
    recommended_move_label = "recommended_action",
    recommended_allocation_count = "alloc_n",
    recommended_estimate = "estimate",
    recommended_posterior_sd = "uncertainty_sd",
    recommended_prob_best = "prob_best",
    recommended_expected_regret = "exp_regret"
  )
  for (old_nm in names(rename_map)) {
    if (old_nm %in% names(out)) {
      names(out)[names(out) == old_nm] <- rename_map[[old_nm]]
    }
  }
  out <- out[, intersect(
    c(
      "method",
      "recommended_action",
      "alloc_n",
      "estimate",
      "uncertainty_sd",
      "prob_best",
      "exp_regret",
      "estimate_gap_top2",
      "runtime_seconds"
    ),
    names(out)
  ), drop = FALSE]

  order_key <- rep.int(1L, nrow(out))
  if ("method" %in% names(out)) {
    order_key <- ifelse(out$method == "thompson", 0L, 1L)
  }
  estimate_col <- if ("estimate" %in% names(out)) out$estimate else rep(NA_real_, nrow(out))
  out <- out[order(order_key, -estimate_col, out$method), , drop = FALSE]
  bg_truncate_rows(out, n, "n")
}

bg_compact_truth_table <- function(x, n = 8L) {
  n <- bg_coerce_integerish(n, "n", 1L)

  if (inherits(x, "bg_truth_battery")) {
    df <- x$summary
    keep <- intersect(
      c(
        "problem_id",
        "opening_roll",
        "n_moves",
        "best_move_label",
        "top_two_gap_estimate",
        "n_near_optimal",
        "mc_not_separated_from_best_set_size",
        "mean_reference_se",
        "difficulty_label"
      ),
      names(df)
    )
    out <- df[, keep, drop = FALSE]
    if ("best_move_label" %in% names(out)) {
      names(out)[names(out) == "best_move_label"] <- "truth_best_move"
    }
    out <- out[order(out$top_two_gap_estimate), , drop = FALSE]
    return(bg_round_display_table(bg_truncate_rows(out, n, "n")))
  }

  diag <- bg_truth_diagnostics(x, top_n = n)
  if (is.list(diag) && "move_table" %in% names(diag)) {
    return(bg_round_display_table(diag$move_table))
  }

  bg_round_display_table(bg_truncate_rows(as.data.frame(diag, stringsAsFactors = FALSE), n, "n"))
}

bg_compact_reference_aware_table <- function(
    x,
    truth = NULL,
    checkpoints = NULL,
    top_k = 3L,
    epsilon = 0.01,
    gap_tol = 0.01,
    n = NULL) {
  out <- bg_eval_reference_aware(
    x = x,
    truth = truth,
    checkpoints = checkpoints,
    top_k = top_k,
    epsilon = epsilon,
    gap_tol = gap_tol
  )

  keep <- intersect(
    c(
      "problem_id",
      "allocation_policy",
      "checkpoint",
      "seed",
      "recommended_move_label",
      "truth_best_move_label",
      "top1_match",
      "simple_regret",
      "spearman",
      "top_k_overlap",
      "pairwise_ordering_accuracy",
      "share_top_k_truth",
      "share_mc_screened_suboptimal",
      "allocation_entropy",
      "near_tie",
      "chosen_mc_not_separated_from_best"
    ),
    names(out)
  )
  out <- out[, keep, drop = FALSE]
  bg_round_display_table(bg_truncate_rows(out, n, "n"))
}

bg_compact_state_battery_table <- function(x, n = NULL) {
  if (!inherits(x, "bg_state_battery")) {
    stop("`x` must inherit from class 'bg_state_battery'.", call. = FALSE)
  }

  out <- x$state_table[, intersect(
    c(
      "problem_id",
      "sample_seed",
      "game_index",
      "turn_index",
      "state_class",
      "n_legal_moves",
      "top_two_gap_estimate",
      "n_near_optimal",
      "difficulty_score"
    ),
    names(x$state_table)
  ), drop = FALSE]
  out <- out[order(out$state_class, out$top_two_gap_estimate), , drop = FALSE]
  bg_round_display_table(bg_truncate_rows(out, n, "n"))
}

# -----------------------------------------------------------------------------
# Source: bg_legacy_print_benchmark.R
# -----------------------------------------------------------------------------
# Print helpers for legacy benchmark, tradeoff, and variance-control objects.
#' @export
#' @noRd
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

#' @export
#' @noRd
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

# -----------------------------------------------------------------------------
# Source: bg_legacy_recommendation.R
# -----------------------------------------------------------------------------
# Legacy recommendation helpers built on rollout evaluators and benchmark objects.
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

bg_position <- function(points, bar = c(0, 0), off = c(0, 0), turn = 1L, validate = TRUE) {
  bg_board(points = points, bar = bar, off = off, turn = turn, validate = validate)
}

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

#' @export
#' @noRd
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

# -----------------------------------------------------------------------------
# Source: bg_legacy_reporting.R
# -----------------------------------------------------------------------------
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

#' @export
#' @noRd
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

#' @noRd
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

# -----------------------------------------------------------------------------
# Source: bg_legacy_rollout_player.R
# -----------------------------------------------------------------------------
# Rollout-based move-selection wrappers and convenience helpers.
bg_match_rollout_policy <- function(policy) {
  match.arg(policy, choices = c("random", "aggressive", "defensive"))
}

bg_normalize_rollout_args <- function(
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    unresolved_value = 0.5,
    initial_allocations = 1L,
    ucb_exploration = 1,
    prior_alpha = 1,
    prior_beta = 1) {
  alloc <- bg_normalize_allocation_args(
    total_budget = rollout_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns,
    unresolved_value = unresolved_value,
    initial_allocations = initial_allocations,
    ucb_exploration = ucb_exploration,
    prior_alpha = prior_alpha,
    prior_beta = prior_beta
  )

  c(
    list(
      rollout_budget = alloc$total_budget,
      rollout_policy = alloc$rollout_policy,
      max_rollout_turns = alloc$max_rollout_turns
    ),
    alloc[setdiff(names(alloc), c("total_budget", "rollout_policy", "max_rollout_turns"))]
  )
}

bg_rollout_evaluate_moves <- function(
    board,
    legal_moves,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    fast_diagnostics = FALSE,
    seed = NULL) {
  evaluate_actions_equal(
    board = board,
    legal_moves = legal_moves,
    total_budget = rollout_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns,
    fast_diagnostics = fast_diagnostics,
    seed = seed
  )$results
}

bg_rollout_move <- function(
    board,
    legal_moves,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    fast_diagnostics = FALSE,
    seed = NULL) {
  evaluate_actions_equal(
    board = board,
    legal_moves = legal_moves,
    total_budget = rollout_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns,
    fast_diagnostics = fast_diagnostics,
    seed = seed
  )$recommended_move
}

bg_play_turn_rollout_player <- function(
    board,
    roll = NULL,
    seed = NULL,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L) {
  bg_play_turn(
    board = board,
    roll = roll,
    selection = "rollout",
    seed = seed,
    rollout_budget = rollout_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns
  )
}

bg_play_game_rollout_players <- function(
    board = bg_initial_board(),
    roll_sequence = NULL,
    max_turns = 1000L,
    seed = NULL,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L) {
  bg_play_game(
    board = board,
    roll_sequence = roll_sequence,
    max_turns = max_turns,
    selection = "rollout",
    seed = seed,
    rollout_budget = rollout_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns
  )
}

# -----------------------------------------------------------------------------
# Source: bg_legacy_studies.R
# -----------------------------------------------------------------------------
# Internal opening, game-trace, and structure study workflows.

bg_opening_truth_lookup <- function(truth) {
  if (!inherits(truth, "bg_truth_battery")) {
    stop("`truth` must inherit from class 'bg_truth_battery'.", call. = FALSE)
  }

  lookup <- truth$summary
  keep <- intersect(
    c(
      "problem_id",
      "opening_roll",
      "die1",
      "die2",
      "is_double",
      "roll_group",
      "n_moves",
      "best_move_label",
      "top_two_gap_estimate",
      "top_two_gap_mc_lower_95",
      "mc_gap_excludes_zero",
      "difficulty_label",
      "n_near_optimal",
      "mc_not_separated_from_best_set_size",
      "mean_reference_se"
    ),
    names(lookup)
  )
  lookup <- lookup[, keep, drop = FALSE]
  if ("best_move_label" %in% names(lookup)) {
    names(lookup)[names(lookup) == "best_move_label"] <- "truth_best_move_label"
  }
  lookup[order(lookup$die1, lookup$die2), , drop = FALSE]
}

bg_opening_attach_lookup <- function(df, lookup) {
  if (nrow(df) == 0L) {
    return(df)
  }
  idx <- match(df$problem_id, lookup$problem_id)
  cbind(df, lookup[idx, setdiff(names(lookup), "problem_id"), drop = FALSE], stringsAsFactors = FALSE)
}

bg_opening_metric_leaderboard <- function(metric_panel) {
  aggregate(
    metric_panel[, c(
      "top1_match",
      "simple_regret",
      "spearman",
      "top_k_overlap",
      "share_top_k_truth",
      "share_mc_screened_suboptimal",
      "allocation_entropy"
    )],
    by = list(
      opening_roll = metric_panel$opening_roll,
      roll_group = metric_panel$roll_group,
      method = metric_panel$allocation_policy,
      checkpoint = metric_panel$checkpoint
    ),
    FUN = mean,
    na.rm = TRUE
  )
}

bg_opening_budget_summary <- function(metric_panel) {
  aggregate(
    metric_panel[, c(
      "top1_match",
      "simple_regret",
      "spearman",
      "top_k_overlap",
      "share_top_k_truth",
      "share_mc_screened_suboptimal",
      "allocation_entropy"
    )],
    by = list(
      roll_group = metric_panel$roll_group,
      method = metric_panel$allocation_policy,
      checkpoint = metric_panel$checkpoint
    ),
    FUN = mean,
    na.rm = TRUE
  )
}

bg_progress_step <- function(pb, value) {
  if (!is.null(pb)) {
    utils::setTxtProgressBar(pb, value)
  }
}

bg_opening_study <- function(
    budgets = c(32L, 64L, 128L, 256L),
    seeds = 1:20,
    methods = c("thompson", "top_two_thompson", "ucb", "equal"),
    simulation_policy = c("random", "heuristic", "aggressive", "defensive", "ts_local"),
    include_doubles = TRUE,
    truth = NULL,
    truth_path = NULL,
    reference_budget = max(4096L, 8L * max(budgets)),
    workers_truth = bg_default_workers_truth(),
    n_cores = 1L,
    parallel = FALSE,
    truth_block_size = 128L,
    cache = TRUE,
    cache_dir = NULL,
    save_path = NULL,
    overwrite = FALSE,
    top_k = 3L,
    verbose = interactive()) {
  cached <- bg_maybe_load_saved_study(save_path, overwrite = overwrite)
  if (!is.null(cached)) {
    return(cached)
  }

  budgets <- sort(unique(bg_normalize_study_budgets(budgets)))
  seeds <- sort(unique(bg_coerce_integerish(seeds, "seeds", length(seeds))))
  methods <- unique(vapply(methods, bg_match_allocation_policy_public, character(1L), USE.NAMES = FALSE))
  n_cores <- bg_coerce_integerish(n_cores, "n_cores", 1L)
  bg_assert_scalar_flag(parallel, "parallel")
  bg_assert_scalar_flag(verbose, "verbose")
  bg_assert_scalar_flag(cache, "cache")
  bg_assert_scalar_flag(overwrite, "overwrite")
  bg_assert_scalar_flag(include_doubles, "include_doubles")
  top_k <- bg_coerce_integerish(top_k, "top_k", 1L)

  if (is.null(truth)) {
    if (!is.null(truth_path) && file.exists(truth_path) && !isTRUE(overwrite)) {
      truth <- bg_truth_load(truth_path)
    } else {
      truth <- bg_truth_opening(
        budget = reference_budget,
        simulation_policy = simulation_policy,
        include_doubles = include_doubles,
        n_cores = workers_truth,
        parallel = workers_truth > 1L,
        truth_block_size = truth_block_size,
        cache = cache,
        cache_dir = cache_dir,
        save_path = truth_path,
        overwrite = overwrite,
        seed = bg_derive_seed(min(seeds), "opening-truth", reference_budget, include_doubles),
        verbose = verbose
      )
    }
  }
  if (!inherits(truth, "bg_truth_battery")) {
    stop("`truth` must inherit from class 'bg_truth_battery'.", call. = FALSE)
  }

  lookup <- bg_opening_truth_lookup(truth)
  problems <- lapply(truth$truths, `[[`, "problem")
  references <- lapply(truth$truths, `[[`, "reference")

  comparison <- bg_compare_methods(
    problems = problems,
    methods = methods,
    budgets = budgets,
    seeds = seeds,
    proxy_references = references,
    n_cores = n_cores,
    parallel = parallel,
    progress = verbose
  )

  metric_panel <- bg_opening_attach_lookup(
    bg_eval_reference_aware(
      x = comparison,
      truth = truth,
      top_k = top_k
    ),
    lookup = lookup
  )

  roll_summary <- bg_opening_attach_lookup(comparison$results, lookup = lookup)
  difficulty_table <- lookup[order(lookup$top_two_gap_estimate), , drop = FALSE]
  rownames(difficulty_table) <- NULL
  seed_stability <- bg_opening_attach_lookup(
    bg_eval_seed_stability(comparison, truth = truth, metric = "simple_regret"),
    lookup = lookup
  )

  leaderboard <- bg_opening_metric_leaderboard(metric_panel)
  budget_summary <- bg_opening_budget_summary(metric_panel)

  out <- structure(
    list(
      truth = truth,
      problems = problems,
      references = references,
      comparison = comparison,
      roll_summary = roll_summary,
      metric_panel = metric_panel,
      seed_stability = seed_stability,
      difficulty_table = difficulty_table,
      leaderboard = leaderboard,
      budget_summary = budget_summary,
      settings = list(
        budgets = budgets,
        seeds = seeds,
        methods = methods,
        simulation_policy = bg_match_simulation_policy_public(simulation_policy),
        include_doubles = include_doubles,
        reference_budget = reference_budget,
        truth_path = truth_path,
        top_k = top_k,
        n_cores = n_cores,
        parallel = isTRUE(parallel)
      )
    ),
    class = "bg_opening_study"
  )

  if (!is.null(save_path)) {
    bg_study_save(out, save_path, overwrite = overwrite)
  }

  out
}

bg_game_trace_problems_from_game <- function(game, simulation_policy) {
  turns <- game$turns
  problems <- vector("list", length(turns))
  for (i in seq_along(turns)) {
    problems[[i]] <- bg_problem(
      state = turns[[i]]$board_before,
      roll = turns[[i]]$roll,
      simulation_policy = simulation_policy,
      problem_id = paste0("turn_", i)
    )
  }
  problems
}

bg_game_trace <- function(
    game = NULL,
    problems = NULL,
    board = bg_initial_board(),
    sample_selection = c("random", "aggressive", "defensive"),
    simulation_policy = c("random", "heuristic", "aggressive", "defensive", "ts_local"),
    local_budget = 128L,
    seeds = 1:5,
    n_reference_nodes = 3L,
    reference_budget = 2048L,
    max_turns = 40L,
    seed = NULL) {
  if (is.null(game) && is.null(problems)) {
    sample_selection <- match.arg(sample_selection)
    game <- bg_play_game(
      board = board,
      max_turns = max_turns,
      selection = sample_selection,
      seed = seed
    )
  }

  if (!is.null(game) && !inherits(game, "bg_game_result")) {
    stop("`game` must inherit from class 'bg_game_result' when supplied.", call. = FALSE)
  }

  if (!is.null(game)) {
    problems <- bg_game_trace_problems_from_game(game, simulation_policy = simulation_policy)
  }

  if (!is.list(problems) || length(problems) < 1L || !all(vapply(problems, inherits, logical(1L), what = "bg_problem"))) {
    stop("`problems` must be a non-empty list of `bg_problem` objects.", call. = FALSE)
  }

  local_budget <- bg_coerce_integerish(local_budget, "local_budget", 1L)
  seeds <- sort(unique(bg_coerce_integerish(seeds, "seeds", length(seeds))))
  n_reference_nodes <- bg_coerce_integerish(n_reference_nodes, "n_reference_nodes", 1L)

  action_counts <- vapply(problems, function(problem) nrow(problem$candidate_table), integer(1L))
  node_priority <- order(action_counts, decreasing = TRUE)
  reference_candidates <- node_priority[action_counts[node_priority] > 0L]
  reference_nodes <- head(reference_candidates, min(n_reference_nodes, length(reference_candidates)))
  references <- vector("list", length(problems))

  rows <- list()
  runs <- list()
  row_id <- 1L

  for (node_index in seq_along(problems)) {
    problem <- problems[[node_index]]
    if (node_index %in% reference_nodes) {
      references[[node_index]] <- bg_reference(
        problem = problem,
        budget = reference_budget,
        seed = bg_derive_seed(seed, "game-trace-reference", node_index)
      )
    }

    for (run_seed in seeds) {
      run <- bg_ts_decide(
        problem = problem,
        budget = local_budget,
        proxy_reference = references[[node_index]],
        seed = run_seed
      )
      runs[[paste0(problem$problem_id, "::", run_seed)]] <- run
      row <- run$checkpoint_table[run$checkpoint_table$checkpoint == local_budget, , drop = FALSE]
      row$node_index <- node_index
      row$problem_id <- problem$problem_id
      row$seed <- run_seed
      row$phase <- if (node_index <= 4L) {
        "opening"
      } else if (node_index <= 20L) {
        "midgame"
      } else {
        "endgame"
      }
      rows[[row_id]] <- row
      row_id <- row_id + 1L
    }
  }

  node_table <- do.call(rbind, rows)
  rownames(node_table) <- NULL
  node_summary <- aggregate(
    node_table[, c("recommended_prob_best", "simple_regret", "allocation_entropy", "runtime_seconds")],
    by = list(phase = node_table$phase),
    FUN = mean,
    na.rm = TRUE
  )

  structure(
    list(
      game = game,
      problems = problems,
      references = references,
      node_table = node_table,
      summary = node_summary,
      runs = runs,
      settings = list(
        local_budget = local_budget,
        seeds = seeds,
        n_reference_nodes = n_reference_nodes,
        reference_budget = reference_budget
      )
    ),
    class = "bg_game_trace"
  )
}

bg_structure_study <- function(
    problems,
    budget = 128L,
    seeds = 1:10,
    baseline = c("equal", "top_two_thompson", "ucb", "ocba", "greedy"),
    reference_budget = 2048L,
    train_fraction = 0.7,
    seed = NULL) {
  if (!is.list(problems) || length(problems) < 4L || !all(vapply(problems, inherits, logical(1L), what = "bg_problem"))) {
    stop("`problems` must be a list of at least four `bg_problem` objects.", call. = FALSE)
  }
  baseline <- match.arg(baseline)
  budget <- bg_coerce_integerish(budget, "budget", 1L)
  seeds <- sort(unique(bg_coerce_integerish(seeds, "seeds", length(seeds))))
  if (!is.numeric(train_fraction) || length(train_fraction) != 1L || is.na(train_fraction) || train_fraction <= 0 || train_fraction >= 1) {
    stop("`train_fraction` must be a numeric scalar in (0, 1).", call. = FALSE)
  }

  comparison <- bg_compare_methods(
    problems = problems,
    methods = c("thompson", baseline),
    budgets = budget,
    seeds = seeds,
    reference_budget = reference_budget
  )

  feature_rows <- do.call(
    rbind,
    lapply(
      problems,
      function(problem) {
        board_features <- bg_board_features(problem)$board_features
        data.frame(problem_id = problem$problem_id, board_features, stringsAsFactors = FALSE)
      }
    )
  )

  results <- comparison$results
  results <- results[results$checkpoint == budget, , drop = FALSE]
  by_problem_method <- aggregate(
    results[, c("simple_regret", "recommended_prob_best", "allocation_entropy")],
    by = list(problem_id = results$problem_id, method = results$allocation_policy),
    FUN = mean,
    na.rm = TRUE
  )

  th <- by_problem_method[by_problem_method$method == "thompson", , drop = FALSE]
  bl <- by_problem_method[by_problem_method$method == baseline, , drop = FALSE]
  merged <- merge(th, bl, by = "problem_id", suffixes = c("_thompson", "_baseline"))
  merged$ts_regret_gain <- merged$simple_regret_baseline - merged$simple_regret_thompson
  merged$ts_prob_best_gain <- merged$recommended_prob_best_thompson - merged$recommended_prob_best_baseline
  merged <- merge(merged, feature_rows, by = "problem_id", all.x = TRUE)

  split_seed <- if (is.null(seed)) 1L else bg_coerce_integerish(seed, "seed", 1L)
  problem_ids <- merged$problem_id
  train_ids <- bg_ts_with_seed(
    split_seed,
    sample(problem_ids, size = max(1L, floor(length(problem_ids) * train_fraction)))
  )
  merged$split <- ifelse(merged$problem_id %in% train_ids, "train", "test")

  feature_names <- setdiff(
    names(merged),
    c(
      "problem_id",
      "method_thompson",
      "method_baseline",
      "split"
    )
  )
  feature_names <- feature_names[grepl("^own_|^opponent_", feature_names)]
  formula_terms <- paste(feature_names, collapse = " + ")
  fit_formula <- stats::as.formula(paste("ts_regret_gain ~", formula_terms))
  fit <- stats::lm(fit_formula, data = merged[merged$split == "train", , drop = FALSE])
  merged$predicted_ts_regret_gain <- stats::predict(fit, newdata = merged)

  structure(
    list(
      comparison = comparison,
      feature_table = merged,
      model = fit,
      settings = list(
        budget = budget,
        seeds = seeds,
        baseline = baseline,
        reference_budget = reference_budget,
        train_fraction = train_fraction
      ),
      warnings = "This is an experimental feature-based structure study, not a fully pooled structured Thompson posterior."
    ),
    class = "bg_structure_study"
  )
}

# -----------------------------------------------------------------------------
# Source: bg_legacy_thompson_rollout_player.R
# -----------------------------------------------------------------------------
# Legacy Thompson-rollout wrappers retained for compatibility.
bg_thompson_rollout_evaluate_moves <- function(
    board,
    legal_moves,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    fast_diagnostics = FALSE,
    seed = NULL) {
  evaluate_actions_thompson(
    board = board,
    legal_moves = legal_moves,
    total_budget = rollout_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns,
    fast_diagnostics = fast_diagnostics,
    seed = seed
  )$results
}

bg_thompson_rollout_move <- function(
    board,
    legal_moves,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    fast_diagnostics = FALSE,
    seed = NULL) {
  evaluate_actions_thompson(
    board = board,
    legal_moves = legal_moves,
    total_budget = rollout_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns,
    fast_diagnostics = fast_diagnostics,
    seed = seed
  )$recommended_move
}

bg_play_turn_thompson_rollout_player <- function(
    board,
    roll = NULL,
    seed = NULL,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L) {
  bg_play_turn(
    board = board,
    roll = roll,
    selection = "thompson_rollout",
    seed = seed,
    rollout_budget = rollout_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns
  )
}

bg_play_game_thompson_rollout_players <- function(
    board = bg_initial_board(),
    roll_sequence = NULL,
    max_turns = 1000L,
    seed = NULL,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L) {
  bg_play_game(
    board = board,
    roll_sequence = roll_sequence,
    max_turns = max_turns,
    selection = "thompson_rollout",
    seed = seed,
    rollout_budget = rollout_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns
  )
}

# -----------------------------------------------------------------------------
# Source: bg_legacy_visualization.R
# -----------------------------------------------------------------------------
# Legacy visualization helpers retained for compatibility.
bg_match_board_perspective <- function(perspective) {
  match.arg(perspective, choices = c("player_1", "player_2"))
}

bg_point_token_ascii <- function(value) {
  if (value > 0L) {
    return(paste0("X", value))
  }
  if (value < 0L) {
    return(paste0("O", abs(value)))
  }
  "."
}

bg_ascii_cell <- function(token, highlight = FALSE, width = 6L) {
  token <- as.character(token)
  if (isTRUE(highlight)) {
    token <- paste0("[", token, "]")
  }
  sprintf(paste0("%", width, "s"), token)
}

bg_board_rows_for_perspective <- function(perspective) {
  perspective <- bg_match_board_perspective(perspective)
  if (perspective == "player_1") {
    return(list(top = 24:13, bottom = 12:1))
  }
  list(top = 1:12, bottom = 13:24)
}

bg_move_highlight_points <- function(move) {
  move <- bg_as_move_sequence(move)
  pts <- integer(0L)
  for (step in move$steps) {
    if (!is.null(step$from) && step$from >= 1L && step$from <= 24L) {
      pts <- c(pts, as.integer(step$from))
    }
    if (!is.null(step$to) && step$to >= 1L && step$to <= 24L) {
      pts <- c(pts, as.integer(step$to))
    }
  }
  unique(pts)
}

bg_plot_highlight_points <- function(highlight_points = NULL, highlight_move = NULL) {
  points <- integer(0L)
  if (!is.null(highlight_points)) {
    points <- c(points, as.integer(highlight_points))
  }
  if (!is.null(highlight_move)) {
    points <- c(points, bg_move_highlight_points(highlight_move))
  }
  points <- unique(points)
  points[!is.na(points) & points >= 1L & points <= 24L]
}

#' @export
format.bg_board <- function(
    x,
    ...,
    show_indices = TRUE,
    perspective = c("player_1", "player_2"),
    highlight_points = NULL,
    highlight_move = NULL) {
  if (!is_bg_board(x)) {
    stop("`x` must inherit from class 'bg_board'.", call. = FALSE)
  }
  bg_assert_scalar_flag(show_indices, "show_indices")
  bg_validate_board(x)
  perspective <- bg_match_board_perspective(perspective)

  rows <- bg_board_rows_for_perspective(perspective)
  highlighted <- bg_plot_highlight_points(highlight_points, highlight_move)

  top_tokens <- vapply(rows$top, function(i) bg_point_token_ascii(x$points[[i]]), character(1L))
  bottom_tokens <- vapply(rows$bottom, function(i) bg_point_token_ascii(x$points[[i]]), character(1L))

  top_cells <- mapply(
    function(tok, idx) bg_ascii_cell(tok, idx %in% highlighted),
    top_tokens,
    rows$top,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
  bottom_cells <- mapply(
    function(tok, idx) bg_ascii_cell(tok, idx %in% highlighted),
    bottom_tokens,
    rows$bottom,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

  top_label <- paste0("Top (", rows$top[[1L]], " -> ", rows$top[[length(rows$top)]], ")")
  bottom_label <- paste0("Bottom (", rows$bottom[[1L]], " -> ", rows$bottom[[length(rows$bottom)]], ")")

  lines <- c(
    "<bg_board_ascii>",
    paste0("turn: ", if (x$turn == 1L) "player_1" else "player_2"),
    paste0("perspective: ", perspective),
    paste0("bar:  p1=", x$bar[[1L]], " p2=", x$bar[[2L]]),
    paste0("off:  p1=", x$off[[1L]], " p2=", x$off[[2L]]),
    ""
  )

  lines <- c(lines, top_label)
  if (isTRUE(show_indices)) {
    lines <- c(lines, paste(vapply(rows$top, function(idx) sprintf("%6d", idx), character(1L)), collapse = ""))
  }
  lines <- c(lines, paste(top_cells, collapse = ""))
  lines <- c(lines, bottom_label)
  if (isTRUE(show_indices)) {
    lines <- c(lines, paste(vapply(rows$bottom, function(idx) sprintf("%6d", idx), character(1L)), collapse = ""))
  }
  lines <- c(lines, paste(bottom_cells, collapse = ""))

  paste(lines, collapse = "\n")
}

bg_board_plot_positions <- function(perspective = c("player_1", "player_2")) {
  perspective <- bg_match_board_perspective(perspective)
  rows <- bg_board_rows_for_perspective(perspective)
  slot_x <- c(seq(0.7, 5.7, by = 1), seq(7.3, 12.3, by = 1))

  out <- data.frame(
    point = integer(24L),
    lane = character(24L),
    x = numeric(24L),
    stringsAsFactors = FALSE
  )

  out$point <- c(rows$top, rows$bottom)
  out$lane <- c(rep("top", length(rows$top)), rep("bottom", length(rows$bottom)))
  out$x <- rep(slot_x, 2L)
  out
}

bg_draw_board_base <- function(main = NULL) {
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 13.6), ylim = c(0, 2), xaxs = "i", yaxs = "i")
  graphics::rect(0, 0, 13.6, 2, col = "#d6b58a", border = "#5b3a1a", lwd = 2)
  graphics::rect(6.1, 0, 6.9, 2, col = "#8f6b3f", border = "#5b3a1a", lwd = 2)

  slot_x <- c(seq(0.7, 5.7, by = 1), seq(7.3, 12.3, by = 1))
  for (i in seq_along(slot_x)) {
    x <- slot_x[[i]]
    alt_col <- if (i %% 2L == 0L) "#f0dfc2" else "#a66b3f"
    graphics::polygon(c(x - 0.45, x + 0.45, x), c(2, 2, 1.1), col = alt_col, border = "#5b3a1a")
    graphics::polygon(c(x - 0.45, x + 0.45, x), c(0, 0, 0.9), col = alt_col, border = "#5b3a1a")
  }

  if (!is.null(main) && nzchar(main)) {
    graphics::title(main = main, line = 0.5)
  }
}

bg_draw_checkers <- function(board, positions, highlight_points = integer(0L)) {
  p1_col <- "#1f4e79"
  p2_col <- "#bf3d1f"
  border_col <- "#222222"
  max_visible <- 6L

  for (i in seq_len(nrow(positions))) {
    point <- positions$point[[i]]
    value <- board$points[[point]]
    if (value == 0L) {
      next
    }

    n <- abs(value)
    lane <- positions$lane[[i]]
    x <- positions$x[[i]]
    col <- if (value > 0L) p1_col else p2_col
    visible <- min(n, max_visible)

    for (k in seq_len(visible)) {
      y <- if (lane == "top") 1.9 - (k - 1L) * 0.13 else 0.1 + (k - 1L) * 0.13
      lwd <- if (point %in% highlight_points) 2.5 else 1
      graphics::symbols(
        x = x,
        y = y,
        circles = 0.11,
        inches = FALSE,
        add = TRUE,
        fg = border_col,
        bg = col,
        lwd = lwd
      )
    }

    if (n > max_visible) {
      y_txt <- if (lane == "top") 1.05 else 0.95
      graphics::text(x, y_txt, labels = as.character(n), cex = 0.7, col = "#111111")
    }
  }
}

bg_draw_board_labels <- function(board, positions, perspective) {
  top <- positions[positions$lane == "top", , drop = FALSE]
  bottom <- positions[positions$lane == "bottom", , drop = FALSE]

  graphics::text(top$x, rep(2.03, nrow(top)), labels = top$point, cex = 0.65, xpd = NA)
  graphics::text(bottom$x, rep(-0.03, nrow(bottom)), labels = bottom$point, cex = 0.65, xpd = NA)
  graphics::text(6.5, 1.95, labels = paste0("bar p1=", board$bar[[1L]]), cex = 0.7)
  graphics::text(6.5, 0.05, labels = paste0("bar p2=", board$bar[[2L]]), cex = 0.7)
  graphics::text(13.15, 1.95, labels = paste0("off p1=", board$off[[1L]]), cex = 0.7, xpd = NA, adj = c(0, 0.5))
  graphics::text(13.15, 0.05, labels = paste0("off p2=", board$off[[2L]]), cex = 0.7, xpd = NA, adj = c(0, 0.5))
  graphics::text(0.1, 1.0, labels = paste0("turn: ", if (board$turn == 1L) "player_1" else "player_2"), adj = c(0, 0.5), cex = 0.72)
  graphics::text(13.5, 1.0, labels = perspective, adj = c(1, 0.5), cex = 0.7)
}

#' @export
plot.bg_board <- function(
    x,
    y = NULL,
    perspective = c("player_1", "player_2"),
    highlight_points = NULL,
    highlight_move = NULL,
    main = NULL,
    add = FALSE,
    ...) {
  if (!is_bg_board(x)) {
    stop("`x` must inherit from class 'bg_board'.", call. = FALSE)
  }
  perspective <- bg_match_board_perspective(perspective)
  bg_validate_board(x)

  positions <- bg_board_plot_positions(perspective)
  highlighted <- bg_plot_highlight_points(highlight_points, highlight_move)

  if (!isTRUE(add)) {
    bg_draw_board_base(main = main)
  }
  bg_draw_checkers(x, positions, highlight_points = highlighted)
  bg_draw_board_labels(x, positions, perspective = perspective)

  invisible(x)
}

bg_plot_board <- function(board, ...) {
  plot.bg_board(board, ...)
}

bg_compare_boards <- function(
    before,
    after,
    perspective = c("player_1", "player_2"),
    main_before = "Before",
    main_after = "After",
    ...) {
  perspective <- bg_match_board_perspective(perspective)
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  graphics::par(mfrow = c(1, 2), mar = c(2.5, 1, 3, 1))
  plot.bg_board(before, perspective = perspective, main = main_before, ...)
  plot.bg_board(after, perspective = perspective, main = main_after, ...)

  invisible(list(before = before, after = after))
}

bg_plot_move <- function(board, move, perspective = c("player_1", "player_2"), ...) {
  move <- bg_as_move_sequence(move)
  after <- bg_apply_move_sequence(board, move)
  highlighted <- bg_move_highlight_points(move)
  bg_compare_boards(
    before = board,
    after = after,
    perspective = perspective,
    main_before = paste0("Before: ", bg_move_label(move)),
    main_after = "After move",
    highlight_points = highlighted,
    ...
  )
  invisible(list(before = board, after = after, move = move))
}

bg_shorten_label <- function(x, max_chars = 28L) {
  x <- as.character(x)
  ifelse(nchar(x) > max_chars, paste0(substr(x, 1L, max_chars - 3L), "..."), x)
}

bg_plot_legal_moves <- function(
    board,
    roll,
    top_n = 6L,
    method = NULL,
    total_budget = NULL,
    perspective = c("player_1", "player_2"),
    ...) {
  top_n <- bg_coerce_integerish(top_n, "top_n", 1L)
  if (top_n < 1L) {
    stop("`top_n` must be at least 1.", call. = FALSE)
  }
  perspective <- bg_match_board_perspective(perspective)
  legal_moves <- bg_legal_moves(board, roll)

  if (length(legal_moves) == 0L) {
    plot.bg_board(board, perspective = perspective, main = "No legal moves")
    return(invisible(list(board = board, legal_moves = legal_moves, ranking = data.frame())))
  }

  ranking <- if (is.null(method)) {
    data.frame(
      candidate_index = seq_along(legal_moves),
      move_label = vapply(legal_moves, bg_move_label, character(1L)),
      move = I(legal_moves),
      rank = seq_along(legal_moves),
      stringsAsFactors = FALSE
    )
  } else {
    if (is.null(total_budget)) {
      total_budget <- 32L
    }
    bg_rank_moves(
      board = board,
      roll = roll,
      method = method,
      total_budget = total_budget,
      ...
    )
  }

  n_show <- min(as.integer(top_n), nrow(ranking))
  if (n_show < 1L) {
    plot.bg_board(board, perspective = perspective, main = "No candidate rows")
    return(invisible(list(board = board, legal_moves = legal_moves, ranking = ranking)))
  }

  n_panels <- n_show + 1L
  ncol <- min(3L, n_panels)
  nrow <- ceiling(n_panels / ncol)

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mfrow = c(nrow, ncol), mar = c(2.3, 1, 2.7, 1))

  plot.bg_board(board, perspective = perspective, main = "Current board")
  for (i in seq_len(n_show)) {
    move <- ranking$move[[i]]
    after <- bg_apply_move_sequence(board, move)
    score <- if ("estimate" %in% names(ranking)) paste0(" p=", formatC(ranking$estimate[[i]], digits = 3, format = "f")) else ""
    title <- paste0("#", ranking$rank[[i]], " ", bg_shorten_label(ranking$move_label[[i]]), score)
    plot.bg_board(after, perspective = perspective, highlight_move = move, main = title)
  }

  invisible(list(board = board, legal_moves = legal_moves, ranking = ranking, shown = ranking[seq_len(n_show), , drop = FALSE]))
}

bg_extract_ranking_table <- function(x) {
  if (inherits(x, "bg_move_recommendation")) {
    return(x$ranking)
  }
  if (inherits(x, "bg_action_evaluation")) {
    return(x$results)
  }
  if (is.data.frame(x)) {
    return(x)
  }
  stop("`x` must be a `bg_action_evaluation`, `bg_move_recommendation`, or data frame.", call. = FALSE)
}

bg_plot_move_ranking <- function(x, main = "Move ranking with uncertainty", ...) {
  tab <- bg_extract_ranking_table(x)
  if (nrow(tab) == 0L) {
    graphics::plot.new()
    graphics::title("No legal moves")
    return(invisible(tab))
  }

  if (!"rank" %in% names(tab)) {
    tab <- tab[order(-tab$estimate, tab$candidate_index), , drop = FALSE]
    tab$rank <- seq_len(nrow(tab))
  } else {
    tab <- tab[order(tab$rank), , drop = FALSE]
  }

  if (!"move_label" %in% names(tab)) {
    tab$move_label <- if ("candidate_index" %in% names(tab)) {
      paste0("candidate_", tab$candidate_index)
    } else {
      paste0("move_", seq_len(nrow(tab)))
    }
  }

  labels <- paste0("#", tab$rank, ": ", bg_shorten_label(tab$move_label))
  means <- tab$estimate
  lower <- if ("lower_95" %in% names(tab)) tab$lower_95 else means
  upper <- if ("upper_95" %in% names(tab)) tab$upper_95 else means
  recommended <- if ("recommended" %in% names(tab)) as.logical(tab$recommended) else rep(FALSE, nrow(tab))
  cols <- ifelse(recommended, "#2e7d32", "#4a6fa5")

  bar_x <- graphics::barplot(
    height = means,
    col = cols,
    border = "#1a1a1a",
    ylim = c(0, max(1, upper, na.rm = TRUE)),
    names.arg = rep("", nrow(tab)),
    ylab = "Estimated win probability",
    main = main
  )

  graphics::arrows(bar_x, lower, bar_x, upper, angle = 90, code = 3, length = 0.04, lwd = 1.2)
  graphics::axis(1, at = bar_x, labels = labels, las = 2, cex.axis = 0.72)

  invisible(tab)
}

bg_plot_allocation_trace <- function(
    x,
    top_n = 4L,
    metric = c("allocation_count", "estimate", "selection_score"),
    ...) {
  if (!inherits(x, "bg_action_evaluation")) {
    stop("`x` must inherit from class 'bg_action_evaluation'.", call. = FALSE)
  }
  metric <- match.arg(metric)
  top_n <- bg_coerce_integerish(top_n, "top_n", 1L)
  if (top_n < 1L) {
    stop("`top_n` must be at least 1.", call. = FALSE)
  }
  if (is.null(x$trace) || nrow(x$trace) == 0L) {
    stop("`x` does not contain a non-empty trace. Re-run evaluation with `trace = TRUE`.", call. = FALSE)
  }

  tr <- x$trace
  final_checkpoint <- max(tr$checkpoint)
  final <- tr[tr$checkpoint == final_checkpoint, , drop = FALSE]
  final <- final[order(-final$estimate), , drop = FALSE]
  keep <- head(final$candidate_index, as.integer(top_n))
  plot_df <- tr[tr$candidate_index %in% keep, , drop = FALSE]
  plot_df <- plot_df[order(plot_df$candidate_index, plot_df$checkpoint), , drop = FALSE]

  y <- plot_df[[metric]]
  if (!is.numeric(y)) {
    stop(sprintf("Trace metric `%s` is not numeric.", metric), call. = FALSE)
  }

  xlim <- range(plot_df$checkpoint, na.rm = TRUE)
  ylim <- range(y, na.rm = TRUE)
  if (diff(ylim) == 0) {
    ylim <- ylim + c(-0.01, 0.01)
  }

  graphics::plot(
    NA,
    xlim = xlim,
    ylim = ylim,
    xlab = "Checkpoint",
    ylab = metric,
    main = paste("Allocation trace:", x$method)
  )

  ids <- sort(unique(plot_df$candidate_index))
  cols <- grDevices::hcl.colors(length(ids), "Dark 3")
  for (i in seq_along(ids)) {
    rows <- plot_df[plot_df$candidate_index == ids[[i]], , drop = FALSE]
    graphics::lines(rows$checkpoint, rows[[metric]], type = "l", lwd = 2, col = cols[[i]])
  }
  graphics::legend(
    "topleft",
    legend = paste0("candidate ", ids),
    col = cols,
    lwd = 2,
    cex = 0.8,
    bty = "n"
  )

  invisible(plot_df)
}

bg_plot_budget_stability <- function(
    board,
    roll,
    budgets = c(8L, 16L, 32L, 64L),
    methods = c("thompson", "ttts", "equal", "ucb", "ocba"),
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    seed = NULL,
    ...) {
  budgets <- bg_coerce_integerish(budgets, "budgets", length(budgets))
  if (any(budgets < 1L)) {
    stop("All elements of `budgets` must be at least 1.", call. = FALSE)
  }
  budgets <- sort(unique(as.integer(budgets)))

  methods <- unique(vapply(methods, bg_match_allocation_method, character(1L), USE.NAMES = FALSE))
  methods <- vapply(methods, bg_canonicalize_allocation_method, character(1L), USE.NAMES = FALSE)
  rollout_policy <- bg_match_rollout_policy(rollout_policy)
  dice_mode <- match.arg(dice_mode)
  bg_assert_scalar_flag(crn, "crn")

  rows <- list()
  row_id <- 1L
  for (method in methods) {
    for (budget in budgets) {
      eval <- bg_evaluate_actions_method(
        board = board,
        roll = roll,
        method = method,
        total_budget = budget,
        rollout_policy = rollout_policy,
        max_rollout_turns = max_rollout_turns,
        dice_mode = dice_mode,
        crn = crn,
        seed = bg_derive_seed(seed, "budget_stability", method, budget, dice_mode, crn)
      )
      rows[[row_id]] <- data.frame(
        method = method,
        total_budget = budget,
        recommended_index = eval$recommended_index,
        recommended_label = if (nrow(eval$results) > 0L) {
          eval$results$move_label[eval$results$recommended][1L]
        } else {
          "<pass>"
        },
        recommended_estimate = if (nrow(eval$results) > 0L) {
          eval$results$estimate[eval$results$recommended][1L]
        } else {
          NA_real_
        },
        stringsAsFactors = FALSE
      )
      row_id <- row_id + 1L
    }
  }

  out <- do.call(rbind, rows)
  methods_ord <- unique(out$method)
  cols <- grDevices::hcl.colors(length(methods_ord), "Set 2")

  ylim <- range(out$recommended_index, na.rm = TRUE)
  if (diff(ylim) == 0) {
    ylim <- ylim + c(-0.2, 0.2)
  }
  graphics::plot(
    NA,
    xlim = range(out$total_budget),
    ylim = ylim,
    xlab = "Total rollout budget",
    ylab = "Recommended candidate index",
    main = "Budget stability"
  )
  for (i in seq_along(methods_ord)) {
    rows_i <- out[out$method == methods_ord[[i]], , drop = FALSE]
    rows_i <- rows_i[order(rows_i$total_budget), , drop = FALSE]
    graphics::lines(rows_i$total_budget, rows_i$recommended_index, type = "b", pch = 16, lwd = 2, col = cols[[i]])
  }
  graphics::legend("topleft", legend = methods_ord, col = cols, lwd = 2, pch = 16, bty = "n")

  invisible(out)
}

bg_plot_benchmark_summary <- function(
    x,
    metric = c(
      "probability_correct_selection",
      "mean_simple_regret",
      "mean_mse",
      "mean_runtime_seconds"
    ),
    ...) {
  if (!inherits(x, "bg_allocation_benchmark")) {
    stop("`x` must inherit from class 'bg_allocation_benchmark'.", call. = FALSE)
  }
  metric <- match.arg(metric)
  summary_df <- x$summary
  if (nrow(summary_df) == 0L) {
    graphics::plot.new()
    graphics::title("Empty benchmark summary")
    return(invisible(summary_df))
  }
  if (!metric %in% names(summary_df)) {
    stop(sprintf("Metric `%s` not found in benchmark summary.", metric), call. = FALSE)
  }

  if ("total_budget" %in% names(summary_df)) {
    lines_df <- summary_df
    lines_df$series <- lines_df$method
    if ("dice_mode" %in% names(lines_df)) {
      lines_df$series <- paste0(lines_df$series, " | ", lines_df$dice_mode)
    }
    if ("crn" %in% names(lines_df)) {
      lines_df$series <- paste0(lines_df$series, " | crn=", as.character(lines_df$crn))
    }

    series <- unique(lines_df$series)
    cols <- grDevices::hcl.colors(length(series), "Dark 2")
    ylim <- range(lines_df[[metric]], na.rm = TRUE)
    if (diff(ylim) == 0) {
      ylim <- ylim + c(-0.01, 0.01)
    }
    graphics::plot(
      NA,
      xlim = range(lines_df$total_budget, na.rm = TRUE),
      ylim = ylim,
      xlab = "Budget",
      ylab = metric,
      main = paste("Benchmark summary:", metric)
    )
    for (i in seq_along(series)) {
      rows <- lines_df[lines_df$series == series[[i]], , drop = FALSE]
      rows <- rows[order(rows$total_budget), , drop = FALSE]
      graphics::lines(rows$total_budget, rows[[metric]], type = "b", pch = 16, col = cols[[i]], lwd = 2)
    }
    graphics::legend("topright", legend = series, col = cols, lwd = 2, pch = 16, cex = 0.72, bty = "n")
    return(invisible(lines_df))
  }

  bar_x <- graphics::barplot(
    height = summary_df[[metric]],
    names.arg = summary_df$method,
    col = "#5b8fd1",
    border = "#1a1a1a",
    las = 2,
    ylab = metric,
    main = paste("Benchmark summary:", metric)
  )
  invisible(cbind(summary_df, bar_x = bar_x))
}

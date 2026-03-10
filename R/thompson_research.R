bg_match_thompson_method <- function(method) {
  match.arg(method, choices = c("thompson", "ttts"))
}

bg_extract_reference_best_cols <- function(tab) {
  best_idx_col <- if ("reference_best_index" %in% names(tab)) "reference_best_index" else "truth_best_index"
  best_label_col <- if ("reference_best_move_label" %in% names(tab)) "reference_best_move_label" else "truth_best_move_label"
  list(index = best_idx_col, label = best_label_col)
}

#' Trace Thompson-sampling allocation dynamics
#'
#' Runs Thompson sampling (or top-two Thompson sampling) with tracing enabled and
#' returns checkpoint-level allocation and posterior summaries over time.
#'
#' This function is intended for algorithm-dynamics analysis under finite
#' simulation budgets.
#'
#' Statistical question addressed:
#'
#' - How does Thompson's allocation shift over time as posterior evidence
#'   accumulates?
#' - Does the leader stabilize, or does leadership flip repeatedly in hard
#'   near-tie settings?
#'
#' @param board A `bg_board` object.
#' @param roll A `bg_roll` object.
#' @param method Thompson-family allocation method (`"thompson"` or `"ttts"`).
#' @param total_budget Integer-like scalar total rollout budget.
#' @param ttts_beta Numeric scalar in `(0, 1]` used when `method = "ttts"`.
#' @inheritParams evaluate_actions_thompson
#'
#' @return A list of class `bg_thompson_trace` with elements:
#'   - `evaluation`: full `bg_action_evaluation` output;
#'   - `trace`: candidate-level checkpoint trace rows;
#'   - `checkpoint_summary`: one row per checkpoint with leader/selection info.
#'
#' Interpretation notes:
#'
#' - Persistent concentration on one action can be encouraging when the
#'   reference top-two gap is moderate/large.
#' - Repeated leader switches at late checkpoints can indicate a hard state or
#'   insufficient budget.
#' @export
trace_thompson_allocation <- function(
    board,
    roll,
    method = c("thompson", "ttts"),
    total_budget = 512L,
    ttts_beta = 0.5,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    unresolved_value = 0.5,
    initial_allocations = 1L,
    prior_alpha = 1,
    prior_beta = 1,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    fast_diagnostics = TRUE,
    trace_every = 1L,
    seed = NULL) {
  method <- bg_match_thompson_method(method)

  ev <- if (method == "thompson") {
    evaluate_actions_thompson(
      board = board,
      roll = roll,
      total_budget = total_budget,
      rollout_policy = rollout_policy,
      max_rollout_turns = max_rollout_turns,
      unresolved_value = unresolved_value,
      initial_allocations = initial_allocations,
      prior_alpha = prior_alpha,
      prior_beta = prior_beta,
      dice_mode = dice_mode,
      crn = crn,
      fast_diagnostics = fast_diagnostics,
      trace = TRUE,
      trace_every = trace_every,
      seed = seed
    )
  } else {
    evaluate_actions_ttts(
      board = board,
      roll = roll,
      total_budget = total_budget,
      rollout_policy = rollout_policy,
      max_rollout_turns = max_rollout_turns,
      unresolved_value = unresolved_value,
      initial_allocations = initial_allocations,
      ttts_beta = ttts_beta,
      prior_alpha = prior_alpha,
      prior_beta = prior_beta,
      dice_mode = dice_mode,
      crn = crn,
      fast_diagnostics = fast_diagnostics,
      trace = TRUE,
      trace_every = trace_every,
      seed = seed
    )
  }

  tr <- ev$trace
  if (is.null(tr) || nrow(tr) == 0L) {
    stop("Trace output is empty. Re-run with `trace = TRUE` and valid budget.", call. = FALSE)
  }

  move_label_lookup <- stats::setNames(as.character(ev$results$move_label), ev$results$candidate_index)
  tr$candidate_move_label <- unname(move_label_lookup[as.character(tr$candidate_index)])
  tr$leader_move_label <- unname(move_label_lookup[as.character(tr$leader_index)])

  checkpoints <- sort(unique(tr$checkpoint))
  checkpoint_rows <- vector("list", length(checkpoints))

  for (i in seq_along(checkpoints)) {
    ck <- checkpoints[[i]]
    rows_ck <- tr[tr$checkpoint == ck, , drop = FALSE]
    leader_index <- rows_ck$leader_index[[1L]]
    leader_row <- rows_ck[rows_ck$candidate_index == leader_index, , drop = FALSE]
    if (nrow(leader_row) == 0L) {
      leader_row <- rows_ck[which.max(rows_ck$estimate), , drop = FALSE]
      leader_index <- leader_row$candidate_index[[1L]]
    }

    selected_candidate <- rows_ck$selected_candidate[[1L]]
    checkpoint_rows[[i]] <- data.frame(
      checkpoint = ck,
      selected_candidate = selected_candidate,
      selected_move_label = unname(move_label_lookup[[as.character(selected_candidate)]]),
      leader_index = leader_index,
      leader_move_label = unname(move_label_lookup[[as.character(leader_index)]]),
      leader_estimate = leader_row$estimate[[1L]],
      leader_posterior_sd = leader_row$posterior_sd[[1L]],
      leader_allocation_count = leader_row$allocation_count[[1L]],
      stringsAsFactors = FALSE
    )
  }

  out <- list(
    evaluation = ev,
    trace = tr,
    checkpoint_summary = do.call(rbind, checkpoint_rows),
    settings = list(
      method = method,
      total_budget = total_budget,
      trace_every = trace_every,
      ttts_beta = if (method == "ttts") as.numeric(ttts_beta) else NA_real_,
      seed = if (is.null(seed)) NULL else bg_coerce_integerish(seed, "seed", 1L)
    )
  )

  class(out) <- "bg_thompson_trace"
  out
}

#' Compare a finite-budget Thompson run to a high-budget reference estimate
#'
#' Computes a finite-budget Thompson-family evaluation and compares it to a
#' high-budget reference estimate (proxy truth) on the same decision instance.
#'
#' This is the primary one-instance evaluator for Thompson behavior: it answers
#' whether a finite-budget recommendation agrees with a high-budget proxy and
#' quantifies regret/error/runtime tradeoffs.
#'
#' @param board A `bg_board` object.
#' @param roll A `bg_roll` object.
#' @param method Thompson-family method (`"thompson"` or `"ttts"`).
#' @param total_budget Integer-like finite simulation budget for Thompson.
#' @param reference_budget Integer-like high simulation budget for the reference
#'   estimate.
#' @param reference Optional precomputed `bg_action_evaluation` object from
#'   [approximate_action_reference()] or [approximate_action_truth()]. When
#'   supplied, the function reuses it instead of recomputing reference
#'   rollouts.
#' @param reference_certificate Optional precomputed `bg_reference_certificate`
#'   object from [certify_reference_truth()]. Supply this with `reference` to
#'   avoid recomputing certification.
#' @param ttts_beta Numeric scalar in `(0, 1]` used when `method = "ttts"`.
#' @param reference_dice_mode Dice mode used for reference estimation.
#' @param reference_crn Logical scalar for reference CRN use.
#' @inheritParams evaluate_actions_thompson
#'
#' @return A list of class `bg_thompson_reference_comparison` with:
#'   - `summary`: one-row decision-quality summary;
#'   - `action_table`: action-wise finite/reference comparison table;
#'   - `evaluation`: finite-budget evaluation;
#'   - `reference`: high-budget reference estimate;
#'   - `reference_certificate`: output of [certify_reference_truth()].
#'
#' Interpretation notes:
#'
#' - `proxy_pcs = 1` means the finite recommendation matches the
#'   reference-best action.
#' - `simple_regret` close to 0 is encouraging even when proxy PCS is 0 in very
#'   near-tie settings.
#' - Use `reference_certified`/`difficulty_label` to gauge how confidently the
#'   reference separates the top actions.
#' @export
compare_thompson_to_reference <- function(
    board,
    roll,
    method = c("thompson", "ttts"),
    total_budget = 512L,
    reference_budget = 8192L,
    reference = NULL,
    reference_certificate = NULL,
    ttts_beta = 0.5,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    unresolved_value = 0.5,
    initial_allocations = 1L,
    prior_alpha = 1,
    prior_beta = 1,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    fast_diagnostics = TRUE,
    reference_dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    reference_crn = FALSE,
    seed = NULL) {
  method <- bg_match_thompson_method(method)
  total_budget <- bg_coerce_integerish(total_budget, "total_budget", 1L)
  if (total_budget < 1L) {
    stop("`total_budget` must be at least 1.", call. = FALSE)
  }
  if (!is.null(reference) && !inherits(reference, "bg_action_evaluation")) {
    stop("`reference` must inherit from class 'bg_action_evaluation' when supplied.", call. = FALSE)
  }
  if (!is.null(reference_certificate) && !inherits(reference_certificate, "bg_reference_certificate")) {
    stop("`reference_certificate` must inherit from class 'bg_reference_certificate' when supplied.", call. = FALSE)
  }

  if (is.null(reference)) {
    reference_budget <- bg_coerce_integerish(reference_budget, "reference_budget", 1L)
    if (reference_budget < 1L) {
      stop("`reference_budget` must be at least 1.", call. = FALSE)
    }
    if (reference_budget < total_budget) {
      stop("`reference_budget` should be at least `total_budget`.", call. = FALSE)
    }
  } else {
    if (!is.null(reference$settings$total_budget)) {
      reference_budget <- bg_coerce_integerish(reference$settings$total_budget, "reference$settings$total_budget", 1L)
    } else {
      reference_budget <- bg_coerce_integerish(reference_budget, "reference_budget", 1L)
    }
    if (reference_budget < total_budget) {
      stop(
        "The supplied `reference` appears to use a smaller budget than `total_budget`; ",
        "use a higher-budget reference for finite-vs-reference comparison.",
        call. = FALSE
      )
    }
  }

  legal_moves <- bg_legal_moves(board, roll)

  finite <- if (method == "thompson") {
    evaluate_actions_thompson(
      board = board,
      roll = roll,
      legal_moves = legal_moves,
      total_budget = total_budget,
      rollout_policy = rollout_policy,
      max_rollout_turns = max_rollout_turns,
      unresolved_value = unresolved_value,
      initial_allocations = initial_allocations,
      prior_alpha = prior_alpha,
      prior_beta = prior_beta,
      dice_mode = dice_mode,
      crn = crn,
      fast_diagnostics = fast_diagnostics,
      seed = bg_derive_seed(seed, "compare_thompson_to_reference", "finite", method, total_budget)
    )
  } else {
    evaluate_actions_ttts(
      board = board,
      roll = roll,
      legal_moves = legal_moves,
      total_budget = total_budget,
      rollout_policy = rollout_policy,
      max_rollout_turns = max_rollout_turns,
      unresolved_value = unresolved_value,
      initial_allocations = initial_allocations,
      ttts_beta = ttts_beta,
      prior_alpha = prior_alpha,
      prior_beta = prior_beta,
      dice_mode = dice_mode,
      crn = crn,
      fast_diagnostics = fast_diagnostics,
      seed = bg_derive_seed(seed, "compare_thompson_to_reference", "finite", method, total_budget)
    )
  }

  if (is.null(reference)) {
    reference <- approximate_action_truth(
      board = board,
      roll = roll,
      legal_moves = legal_moves,
      truth_budget = reference_budget,
      rollout_policy = rollout_policy,
      max_rollout_turns = max_rollout_turns,
      unresolved_value = unresolved_value,
      prior_alpha = prior_alpha,
      prior_beta = prior_beta,
      dice_mode = reference_dice_mode,
      crn = reference_crn,
      # Use the user-facing seed directly here so standalone
      # `approximate_action_reference(..., seed = s)` matches this reference run.
      seed = if (is.null(seed)) NULL else bg_coerce_integerish(seed, "seed", 1L)
    )
  }

  finite_tab <- finite$results[order(finite$results$candidate_index), , drop = FALSE]
  reference_tab <- reference$results[order(reference$results$candidate_index), , drop = FALSE]

  if (!identical(finite_tab$candidate_index, reference_tab$candidate_index)) {
    stop("Internal error: candidate sets differ between finite and reference runs.", call. = FALSE)
  }

  chosen_index <- finite$recommended_index
  reference_best_index <- reference$recommended_index
  chosen_row <- bg_recommended_row(finite)

  ref_lookup <- stats::setNames(reference_tab$estimate, reference_tab$candidate_index)
  ref_label_lookup <- stats::setNames(as.character(reference_tab$move_label), reference_tab$candidate_index)
  chosen_reference_value <- ref_lookup[[as.character(chosen_index)]]
  reference_best_value <- max(reference_tab$estimate)

  action_error <- finite_tab$estimate - reference_tab$estimate
  action_table <- data.frame(
    candidate_index = finite_tab$candidate_index,
    move_label = finite_tab$move_label,
    finite_recommended = finite_tab$candidate_index == chosen_index,
    reference_best = finite_tab$candidate_index == reference_best_index,
    finite_allocation_count = finite_tab$allocation_count,
    finite_estimate = finite_tab$estimate,
    finite_posterior_sd = finite_tab$posterior_sd,
    finite_prob_best = if ("prob_best" %in% names(finite_tab)) finite_tab$prob_best else NA_real_,
    finite_expected_regret = if ("posterior_expected_regret" %in% names(finite_tab)) {
      finite_tab$posterior_expected_regret
    } else {
      NA_real_
    },
    reference_estimate = reference_tab$estimate,
    reference_posterior_sd = reference_tab$posterior_sd,
    estimation_error = action_error,
    abs_error = abs(action_error),
    squared_error = action_error^2,
    stringsAsFactors = FALSE
  )

  reference_cert <- if (is.null(reference_certificate)) {
    certify_reference_truth(reference = reference)
  } else {
    reference_certificate
  }
  cert_row <- reference_cert$certificate

  summary <- data.frame(
    method = method,
    total_budget = total_budget,
    reference_budget = reference_budget,
    chosen_index = chosen_index,
    chosen_move_label = chosen_row$move_label[[1L]],
    chosen_allocation_count = chosen_row$allocation_count[[1L]],
    chosen_estimate = chosen_row$estimate[[1L]],
    chosen_prob_best = if ("prob_best" %in% names(chosen_row)) chosen_row$prob_best[[1L]] else NA_real_,
    chosen_expected_regret = if ("posterior_expected_regret" %in% names(chosen_row)) {
      chosen_row$posterior_expected_regret[[1L]]
    } else {
      NA_real_
    },
    reference_best_index = reference_best_index,
    reference_best_move_label = ref_label_lookup[[as.character(reference_best_index)]],
    proxy_pcs = compute_probability_of_correct_selection(chosen_index, reference_best_index),
    simple_regret = compute_simple_regret(chosen_reference_value, reference_best_value),
    mse = compute_mse(finite_tab$estimate, reference_tab$estimate),
    finite_runtime_seconds = finite$runtime_seconds,
    reference_runtime_seconds = reference$runtime_seconds,
    reference_gap_estimate = cert_row$top_two_gap_estimate[[1L]],
    reference_gap_lower_95 = cert_row$top_two_gap_lower_95[[1L]],
    reference_gap_upper_95 = cert_row$top_two_gap_upper_95[[1L]],
    reference_certified = cert_row$certified[[1L]],
    difficulty_label = cert_row$difficulty_label[[1L]],
    stringsAsFactors = FALSE
  )

  out <- list(
    summary = summary,
    action_table = action_table,
    evaluation = finite,
    reference = reference,
    reference_certificate = reference_cert
  )
  class(out) <- "bg_thompson_reference_comparison"
  out
}

#' Certify a high-budget reference estimate
#'
#' Computes a simple top-two-gap certification diagnostic for a high-budget
#' reference estimate (proxy truth).
#'
#' The returned certificate is not a proof of exact optimality. It quantifies
#' how separated the top two reference-estimated actions are, using an
#' approximate uncertainty calculation.
#'
#' This helps prevent overconfident interpretation of proxy truth on hard states
#' with tiny estimated gaps.
#'
#' @param reference Optional `bg_action_evaluation` object produced by
#'   [approximate_action_truth()].
#' @param board Optional `bg_board` object. Used only when `reference` is NULL.
#' @param roll Optional `bg_roll` object. Used only when `reference` is NULL.
#' @param reference_budget Integer-like budget used when constructing `reference`
#'   internally.
#' @inheritParams approximate_action_truth
#'
#' @return A list of class `bg_reference_certificate` with:
#'   - `certificate`: one-row data frame;
#'   - `reference`: the reference evaluation object.
#' @export
certify_reference_truth <- function(
    reference = NULL,
    board = NULL,
    roll = NULL,
    reference_budget = 8192L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    unresolved_value = 0.5,
    prior_alpha = 1,
    prior_beta = 1,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    seed = NULL) {
  if (is.null(reference)) {
    if (is.null(board) || is.null(roll)) {
      stop("Supply either `reference` or both `board` and `roll`.", call. = FALSE)
    }

    reference <- approximate_action_truth(
      board = board,
      roll = roll,
      truth_budget = reference_budget,
      rollout_policy = rollout_policy,
      max_rollout_turns = max_rollout_turns,
      unresolved_value = unresolved_value,
      prior_alpha = prior_alpha,
      prior_beta = prior_beta,
      dice_mode = dice_mode,
      crn = crn,
      seed = seed
    )
  }

  if (!inherits(reference, "bg_action_evaluation")) {
    stop("`reference` must inherit from class 'bg_action_evaluation'.", call. = FALSE)
  }

  tab <- reference$results[order(-reference$results$estimate, reference$results$candidate_index), , drop = FALSE]
  if (nrow(tab) < 1L) {
    cert <- data.frame(
      n_actions = 0L,
      reference_best_index = NA_integer_,
      reference_best_move_label = NA_character_,
      reference_second_index = NA_integer_,
      reference_second_move_label = NA_character_,
      top_two_gap_estimate = NA_real_,
      top_two_gap_se = NA_real_,
      top_two_gap_lower_95 = NA_real_,
      top_two_gap_upper_95 = NA_real_,
      certified = NA,
      difficulty_score = NA_real_,
      difficulty_label = NA_character_,
      stringsAsFactors = FALSE
    )
    out <- list(certificate = cert, reference = reference)
    class(out) <- "bg_reference_certificate"
    return(out)
  }

  best <- tab[1L, , drop = FALSE]
  second <- if (nrow(tab) >= 2L) tab[2L, , drop = FALSE] else tab[1L, , drop = FALSE]

  gap <- best$estimate[[1L]] - second$estimate[[1L]]
  gap_se <- sqrt(best$posterior_sd[[1L]]^2 + second$posterior_sd[[1L]]^2)
  lower_95 <- gap - 1.96 * gap_se
  upper_95 <- gap + 1.96 * gap_se
  certified <- is.finite(lower_95) && lower_95 > 0

  difficulty_score <- if (is.na(gap) || gap <= 0) Inf else 1 / gap
  difficulty_label <- if (is.infinite(difficulty_score)) {
    "extreme"
  } else if (gap < 0.01) {
    "very_hard"
  } else if (gap < 0.03) {
    "hard"
  } else if (gap < 0.07) {
    "moderate"
  } else {
    "easy"
  }

  cert <- data.frame(
    n_actions = nrow(tab),
    reference_best_index = best$candidate_index[[1L]],
    reference_best_move_label = best$move_label[[1L]],
    reference_second_index = second$candidate_index[[1L]],
    reference_second_move_label = second$move_label[[1L]],
    top_two_gap_estimate = gap,
    top_two_gap_se = gap_se,
    top_two_gap_lower_95 = lower_95,
    top_two_gap_upper_95 = upper_95,
    certified = certified,
    difficulty_score = difficulty_score,
    difficulty_label = difficulty_label,
    stringsAsFactors = FALSE
  )

  out <- list(certificate = cert, reference = reference)
  class(out) <- "bg_reference_certificate"
  out
}

#' Benchmark Thompson sampling against baseline allocation methods
#'
#' Runs crossed budget/method benchmarking with Thompson sampling as the primary
#' method and additional allocation rules as baselines.
#'
#' Statistical question addressed:
#'
#' - Across benchmark cases and budgets, when does Thompson improve proxy PCS
#'   and regret relative to baselines, and at what runtime cost?
#'
#' @param cases A list of `bg_benchmark_case` objects.
#' @param budgets Integer-like budget vector.
#' @param baselines Character vector of baseline methods.
#' @param include_ttts Logical scalar; include top-two Thompson in baselines.
#' @param reference_budget Integer-like high budget used for reference estimates.
#' @inheritParams benchmark_allocation_methods
#'
#' @return A `bg_thompson_benchmark` object (inherits from
#'   `bg_allocation_benchmark`) with an added `thompson_focus` summary.
#' @export
benchmark_thompson <- function(
    cases,
    budgets = c(128L, 512L, 2048L),
    baselines = c("equal", "ucb", "ocba", "greedy"),
    include_ttts = TRUE,
    reference_budget = 8192L,
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
  methods <- c("thompson", baselines)
  if (isTRUE(include_ttts)) {
    methods <- c(methods, "ttts")
  }
  methods <- unique(vapply(methods, bg_match_allocation_method, character(1L), USE.NAMES = FALSE))

  bm <- benchmark_allocation_methods(
    cases = cases,
    methods = methods,
    budgets = budgets,
    truth_budget = reference_budget,
    dice_modes = dice_modes,
    crn_values = crn_values,
    truth_dice_mode = truth_dice_mode,
    truth_crn = truth_crn,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns,
    unresolved_value = unresolved_value,
    initial_allocations = initial_allocations,
    ucb_exploration = ucb_exploration,
    prior_alpha = prior_alpha,
    prior_beta = prior_beta,
    seed = seed
  )

  bm$thompson_focus <- summarize_thompson_benchmark(bm)
  class(bm) <- c("bg_thompson_benchmark", class(bm))
  bm
}

#' Summarize Thompson-focused benchmark results
#'
#' Produces Thompson-centered summary tables for proxy PCS, simple regret, MSE,
#' runtime, and difficulty strata.
#'
#' Positive findings and cautions should both be expected: Thompson may
#' outperform baselines on some case/budget cells and underperform on others,
#' especially in very hard states.
#'
#' @param x A `bg_thompson_benchmark` or `bg_allocation_benchmark` object.
#'
#' @return A list of class `bg_thompson_benchmark_summary` containing
#'   Thompson-specific and baseline comparison tables.
#' @export
summarize_thompson_benchmark <- function(x) {
  if (!inherits(x, "bg_allocation_benchmark")) {
    stop("`x` must inherit from class 'bg_allocation_benchmark'.", call. = FALSE)
  }

  summary_df <- x$summary
  if (!"correct_selection_rate" %in% names(summary_df) && "probability_correct_selection" %in% names(summary_df)) {
    summary_df$correct_selection_rate <- summary_df$probability_correct_selection
  }

  th <- summary_df[summary_df$method == "thompson", , drop = FALSE]
  baselines <- summary_df[summary_df$method != "thompson", , drop = FALSE]

  join_keys <- intersect(c("total_budget", "dice_mode", "crn"), names(summary_df))
  rel_rows <- vector("list", nrow(baselines))

  for (i in seq_len(nrow(baselines))) {
    b <- baselines[i, , drop = FALSE]
    idx <- rep(TRUE, nrow(th))
    for (k in join_keys) {
      idx <- idx & (th[[k]] == b[[k]])
    }
    if (!any(idx)) {
      rel_rows[[i]] <- data.frame(
        baseline_method = b$method,
        total_budget = b$total_budget,
        dice_mode = if ("dice_mode" %in% names(b)) b$dice_mode else NA_character_,
        crn = if ("crn" %in% names(b)) b$crn else NA,
        thompson_advantage_pcs = NA_real_,
        thompson_advantage_regret = NA_real_,
        thompson_advantage_mse = NA_real_,
        thompson_runtime_ratio = NA_real_,
        stringsAsFactors = FALSE
      )
      next
    }

    th_row <- th[which(idx)[1L], , drop = FALSE]
    rel_rows[[i]] <- data.frame(
      baseline_method = b$method,
      total_budget = b$total_budget,
      dice_mode = if ("dice_mode" %in% names(b)) b$dice_mode else NA_character_,
      crn = if ("crn" %in% names(b)) b$crn else NA,
      thompson_advantage_pcs = th_row$correct_selection_rate - b$correct_selection_rate,
      thompson_advantage_regret = b$mean_simple_regret - th_row$mean_simple_regret,
      thompson_advantage_mse = b$mean_mse - th_row$mean_mse,
      thompson_runtime_ratio = th_row$mean_runtime_seconds / b$mean_runtime_seconds,
      stringsAsFactors = FALSE
    )
  }

  rel <- if (length(rel_rows) > 0L) do.call(rbind, rel_rows) else data.frame()

  diff_df <- data.frame()
  if ("difficulty_label" %in% names(x$results) && nrow(x$results) > 0L) {
    split_key <- interaction(x$results$method, x$results$difficulty_label, drop = TRUE)
    split_rows <- split(x$results, split_key)
    diff_rows <- lapply(split_rows, function(df) {
      data.frame(
        method = as.character(df$method[[1L]]),
        difficulty_label = as.character(df$difficulty_label[[1L]]),
        n_cases = nrow(df),
        proxy_pcs = if (all(is.na(df$correct_selection))) NA_real_ else mean(df$correct_selection, na.rm = TRUE),
        mean_simple_regret = if (all(is.na(df$simple_regret))) NA_real_ else mean(df$simple_regret, na.rm = TRUE),
        mean_mse = if (all(is.na(df$mse))) NA_real_ else mean(df$mse, na.rm = TRUE),
        mean_runtime_seconds = if (all(is.na(df$runtime_seconds))) NA_real_ else mean(df$runtime_seconds, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    })
    diff_df <- do.call(rbind, diff_rows)
    rownames(diff_df) <- NULL
  }

  out <- list(
    thompson = th,
    baselines = baselines,
    relative_to_thompson = rel,
    by_difficulty = diff_df
  )
  class(out) <- "bg_thompson_benchmark_summary"
  out
}

#' Plot Thompson convergence over allocation checkpoints
#'
#' @param x A `bg_thompson_trace` object or `bg_action_evaluation` with trace.
#' @param top_n Number of top candidates (by final estimate) to plot.
#' @param metric Trace metric to display.
#' @param show_leader Logical scalar; if `TRUE`, overlays current leader estimate.
#' @param ... Unused.
#'
#' @return The plotted trace data frame, invisibly.
#' @export
plot_thompson_convergence <- function(
    x,
    top_n = 4L,
    metric = c("estimate", "allocation_count", "selection_score"),
    show_leader = TRUE,
    ...) {
  metric <- match.arg(metric)
  top_n <- bg_coerce_integerish(top_n, "top_n", 1L)
  bg_assert_scalar_flag(show_leader, "show_leader")

  ev <- if (inherits(x, "bg_thompson_trace")) {
    x$evaluation
  } else if (inherits(x, "bg_action_evaluation")) {
    x
  } else {
    stop("`x` must be `bg_thompson_trace` or `bg_action_evaluation`.", call. = FALSE)
  }

  if (is.null(ev$trace) || nrow(ev$trace) == 0L) {
    stop("No trace found. Re-run evaluation with `trace = TRUE`.", call. = FALSE)
  }

  tr <- ev$trace
  final_checkpoint <- max(tr$checkpoint)
  final_rows <- tr[tr$checkpoint == final_checkpoint, , drop = FALSE]
  final_rows <- final_rows[order(-final_rows$estimate, final_rows$candidate_index), , drop = FALSE]
  keep <- head(final_rows$candidate_index, top_n)

  plot_df <- tr[tr$candidate_index %in% keep, , drop = FALSE]
  plot_df <- plot_df[order(plot_df$candidate_index, plot_df$checkpoint), , drop = FALSE]

  y <- plot_df[[metric]]
  ylim <- range(y, na.rm = TRUE)
  if (!is.finite(ylim[[1L]]) || !is.finite(ylim[[2L]])) {
    ylim <- c(0, 1)
  }
  if (diff(ylim) == 0) {
    ylim <- ylim + c(-0.01, 0.01)
  }

  graphics::plot(
    NA,
    xlim = range(plot_df$checkpoint, na.rm = TRUE),
    ylim = ylim,
    xlab = "Checkpoint",
    ylab = metric,
    main = paste0("Thompson convergence: ", ev$method)
  )

  ids <- sort(unique(plot_df$candidate_index))
  cols <- grDevices::hcl.colors(length(ids), "Dark 3")
  for (i in seq_along(ids)) {
    rows <- plot_df[plot_df$candidate_index == ids[[i]], , drop = FALSE]
    graphics::lines(rows$checkpoint, rows[[metric]], type = "l", lwd = 2, col = cols[[i]])
  }

  if (isTRUE(show_leader) && metric == "estimate") {
    leader_df <- do.call(rbind, lapply(split(tr, tr$checkpoint), function(df) {
      leader_idx <- df$leader_index[[1L]]
      row <- df[df$candidate_index == leader_idx, , drop = FALSE]
      if (nrow(row) == 0L) {
        row <- df[which.max(df$estimate), , drop = FALSE]
      }
      data.frame(checkpoint = row$checkpoint[[1L]], leader_estimate = row$estimate[[1L]], stringsAsFactors = FALSE)
    }))
    leader_df <- leader_df[order(leader_df$checkpoint), , drop = FALSE]
    graphics::lines(leader_df$checkpoint, leader_df$leader_estimate, lwd = 2.5, lty = 2, col = "#000000")
  }

  graphics::legend(
    "topleft",
    legend = paste0("candidate ", ids),
    col = cols,
    lwd = 2,
    bty = "n",
    cex = 0.8
  )

  invisible(plot_df)
}

#' Plot Thompson versus baseline methods on benchmark summaries
#'
#' @param x A `bg_thompson_benchmark` or `bg_allocation_benchmark` object.
#' @param metric Summary metric to compare.
#' @param ... Unused.
#'
#' @return Benchmark summary rows used in the plot, invisibly.
#' @export
plot_thompson_vs_baselines <- function(
    x,
    metric = c("correct_selection_rate", "mean_simple_regret", "mean_mse", "mean_runtime_seconds"),
    ...) {
  if (!inherits(x, "bg_allocation_benchmark")) {
    stop("`x` must inherit from class 'bg_allocation_benchmark'.", call. = FALSE)
  }

  metric <- match.arg(metric)
  df <- x$summary
  if (!"correct_selection_rate" %in% names(df) && "probability_correct_selection" %in% names(df)) {
    df$correct_selection_rate <- df$probability_correct_selection
  }
  if (!metric %in% names(df)) {
    stop(sprintf("Metric `%s` not found in benchmark summary.", metric), call. = FALSE)
  }
  if (!"total_budget" %in% names(df)) {
    stop("Benchmark summary must include `total_budget`.", call. = FALSE)
  }

  df <- df[order(df$total_budget, df$method), , drop = FALSE]
  methods <- unique(df$method)
  cols <- grDevices::hcl.colors(length(methods), "Set 2")
  names(cols) <- methods

  ylim <- range(df[[metric]], na.rm = TRUE)
  if (diff(ylim) == 0) {
    ylim <- ylim + c(-0.01, 0.01)
  }

  graphics::plot(
    NA,
    xlim = range(df$total_budget, na.rm = TRUE),
    ylim = ylim,
    xlab = "Total rollout budget",
    ylab = metric,
    main = paste("Thompson vs baselines:", metric)
  )

  for (m in methods) {
    rows <- df[df$method == m, , drop = FALSE]
    rows <- rows[order(rows$total_budget), , drop = FALSE]
    is_th <- identical(m, "thompson")
    graphics::lines(
      rows$total_budget,
      rows[[metric]],
      type = "b",
      pch = if (is_th) 16 else 1,
      lwd = if (is_th) 3 else 1.8,
      col = cols[[m]],
      lty = if (is_th) 1 else 2
    )
  }

  graphics::legend("topright", legend = methods, col = cols[methods], lwd = ifelse(methods == "thompson", 3, 1.8), pch = ifelse(methods == "thompson", 16, 1), lty = ifelse(methods == "thompson", 1, 2), cex = 0.8, bty = "n")

  invisible(df)
}

#' Print a Thompson-allocation trace object
#'
#' @param x A `bg_thompson_trace` object.
#' @param n Integer-like scalar controlling how many checkpoint rows to print.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.bg_thompson_trace <- function(x, n = 10L, ...) {
  if (!inherits(x, "bg_thompson_trace")) {
    stop("`x` must inherit from class 'bg_thompson_trace'.", call. = FALSE)
  }

  cat("<bg_thompson_trace>\n", sep = "")
  cat("method:       ", x$settings$method, "\n", sep = "")
  cat("total_budget: ", x$settings$total_budget, "\n", sep = "")
  cat("checkpoints:  ", length(unique(x$trace$checkpoint)), "\n", sep = "")
  compact <- x$checkpoint_summary[, intersect(
    c(
      "checkpoint",
      "selected_candidate",
      "selected_move_label",
      "leader_index",
      "leader_move_label",
      "leader_estimate",
      "leader_posterior_sd",
      "leader_allocation_count"
    ),
    names(x$checkpoint_summary)
  ), drop = FALSE]
  compact <- bg_truncate_rows(compact, n, "n")
  print(compact, row.names = FALSE)
  if (nrow(x$checkpoint_summary) > nrow(compact)) {
    cat("showing_first: ", nrow(compact), " of ", nrow(x$checkpoint_summary), " checkpoints\n", sep = "")
  }
  invisible(x)
}

#' Print a Thompson-vs-reference comparison
#'
#' @param x A `bg_thompson_reference_comparison` object.
#' @param n_actions Integer-like scalar controlling how many action rows from
#'   the finite-vs-reference table to print.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.bg_thompson_reference_comparison <- function(x, n_actions = 10L, ...) {
  if (!inherits(x, "bg_thompson_reference_comparison")) {
    stop("`x` must inherit from class 'bg_thompson_reference_comparison'.", call. = FALSE)
  }

  cat("<bg_thompson_reference_comparison>\n", sep = "")
  summary_keep <- intersect(
    c(
      "method",
      "total_budget",
      "reference_budget",
      "chosen_move_label",
      "reference_best_move_label",
      "chosen_estimate",
      "chosen_prob_best",
      "chosen_expected_regret",
      "proxy_pcs",
      "simple_regret",
      "mse",
      "finite_runtime_seconds",
      "reference_runtime_seconds",
      "difficulty_label",
      "reference_certified"
    ),
    names(x$summary)
  )
  summary_tab <- x$summary[, summary_keep, drop = FALSE]
  if ("chosen_prob_best" %in% names(summary_tab) && all(is.na(summary_tab$chosen_prob_best))) {
    summary_tab$chosen_prob_best <- NULL
  }
  if ("chosen_expected_regret" %in% names(summary_tab) && all(is.na(summary_tab$chosen_expected_regret))) {
    summary_tab$chosen_expected_regret <- NULL
  }
  summary_rename <- c(
    chosen_move_label = "recommended_action",
    reference_best_move_label = "reference_best_action",
    chosen_estimate = "recommended_estimate",
    chosen_prob_best = "recommended_prob_best",
    chosen_expected_regret = "recommended_exp_regret",
    finite_runtime_seconds = "finite_runtime_s",
    reference_runtime_seconds = "reference_runtime_s"
  )
  for (old_nm in names(summary_rename)) {
    if (old_nm %in% names(summary_tab)) {
      names(summary_tab)[names(summary_tab) == old_nm] <- summary_rename[[old_nm]]
    }
  }
  print(summary_tab, row.names = FALSE)

  tab <- x$action_table
  if (nrow(tab) > 0L) {
    tab <- tab[order(-tab$reference_best, -tab$finite_recommended, -tab$reference_estimate, tab$candidate_index), , drop = FALSE]
    tab <- bg_truncate_rows(tab, n_actions, "n_actions")
    tab_keep <- intersect(
      c(
        "candidate_index",
        "move_label",
        "finite_recommended",
        "reference_best",
        "finite_allocation_count",
        "finite_estimate",
        "reference_estimate",
        "abs_error",
        "finite_prob_best",
        "finite_expected_regret"
      ),
      names(tab)
    )
    if ("finite_prob_best" %in% tab_keep && all(is.na(tab$finite_prob_best))) {
      tab_keep <- setdiff(tab_keep, "finite_prob_best")
    }
    if ("finite_expected_regret" %in% tab_keep && all(is.na(tab$finite_expected_regret))) {
      tab_keep <- setdiff(tab_keep, "finite_expected_regret")
    }
    tab_print <- tab[, tab_keep, drop = FALSE]
    tab_rename <- c(
      candidate_index = "action_id",
      move_label = "action",
      finite_recommended = "recommended",
      finite_allocation_count = "alloc_n",
      finite_estimate = "estimate",
      reference_estimate = "reference_estimate",
      finite_prob_best = "prob_best",
      finite_expected_regret = "exp_regret"
    )
    for (old_nm in names(tab_rename)) {
      if (old_nm %in% names(tab_print)) {
        names(tab_print)[names(tab_print) == old_nm] <- tab_rename[[old_nm]]
      }
    }
    cat("action_table:\n", sep = "")
    print(tab_print, row.names = FALSE)
    if (nrow(x$action_table) > nrow(tab)) {
      cat("showing_first: ", nrow(tab), " of ", nrow(x$action_table), " actions\n", sep = "")
    }
  }
  invisible(x)
}

#' Print a reference-certificate object
#'
#' @param x A `bg_reference_certificate` object.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.bg_reference_certificate <- function(x, ...) {
  if (!inherits(x, "bg_reference_certificate")) {
    stop("`x` must inherit from class 'bg_reference_certificate'.", call. = FALSE)
  }

  cat("<bg_reference_certificate>\n", sep = "")
  keep <- intersect(
    c(
      "reference_best_move_label",
      "reference_second_move_label",
      "top_two_gap_estimate",
      "top_two_gap_lower_95",
      "top_two_gap_upper_95",
      "certified",
      "difficulty_label"
    ),
    names(x$certificate)
  )
  print(x$certificate[, keep, drop = FALSE], row.names = FALSE)
  invisible(x)
}

#' Print a Thompson-focused benchmark object
#'
#' @param x A `bg_thompson_benchmark` object.
#' @param n Integer-like scalar controlling how many summary rows to print.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.bg_thompson_benchmark <- function(x, n = 20L, ...) {
  if (!inherits(x, "bg_thompson_benchmark")) {
    stop("`x` must inherit from class 'bg_thompson_benchmark'.", call. = FALSE)
  }

  cat("<bg_thompson_benchmark>\n", sep = "")
  cat("methods:      ", paste(x$settings$methods, collapse = ", "), "\n", sep = "")
  cat("budgets:      ", paste(x$settings$budgets, collapse = ", "), "\n", sep = "")
  cat("reference_budget: ", x$settings$truth_budget, "\n", sep = "")
  if (!is.null(x$thompson_focus) && !is.null(x$thompson_focus$thompson)) {
    cat("\nThompson rows:\n", sep = "")
    print(bg_compact_benchmark_summary(x$thompson_focus$thompson, n = n), row.names = FALSE)
  } else {
    print(bg_compact_benchmark_summary(x$summary, n = n), row.names = FALSE)
  }
  invisible(x)
}

#' Print a Thompson benchmark summary object
#'
#' @param x A `bg_thompson_benchmark_summary` object.
#' @param n Integer-like scalar controlling how many rows to print per table.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.bg_thompson_benchmark_summary <- function(x, n = 20L, ...) {
  if (!inherits(x, "bg_thompson_benchmark_summary")) {
    stop("`x` must inherit from class 'bg_thompson_benchmark_summary'.", call. = FALSE)
  }

  cat("<bg_thompson_benchmark_summary>\n", sep = "")
  cat("\nThompson cells:\n", sep = "")
  print(bg_compact_benchmark_summary(x$thompson, n = n), row.names = FALSE)
  if (nrow(x$relative_to_thompson) > 0L) {
    cat("\nRelative-to-thompson deltas:\n", sep = "")
    print(bg_truncate_rows(x$relative_to_thompson, n, "n"), row.names = FALSE)
  }
  if (nrow(x$by_difficulty) > 0L) {
    cat("\nBy difficulty:\n", sep = "")
    print(bg_truncate_rows(x$by_difficulty, n, "n"), row.names = FALSE)
  }
  invisible(x)
}

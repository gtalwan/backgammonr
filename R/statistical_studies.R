bg_normalize_study_methods <- function(methods, arg_name = "methods") {
  if (!is.character(methods) || length(methods) < 1L || anyNA(methods)) {
    stop(sprintf("`%s` must be a non-empty character vector.", arg_name), call. = FALSE)
  }

  matched <- vapply(methods, bg_match_allocation_method, character(1L), USE.NAMES = FALSE)
  unique(vapply(matched, bg_canonicalize_allocation_method, character(1L), USE.NAMES = FALSE))
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

bg_recommended_row <- function(evaluation) {
  idx <- match(evaluation$recommended_index, evaluation$results$candidate_index)
  if (is.na(idx)) {
    stop("Internal error: recommended candidate was not found in evaluation results.", call. = FALSE)
  }
  evaluation$results[idx, , drop = FALSE]
}

#' Compare allocation methods on one fixed decision problem
#'
#' Runs several rollout-allocation methods on the same board + roll with the
#' same total budget, then reports which move each method recommends, the
#' corresponding estimated value, and runtime.
#'
#' This is the most direct "same budget, different allocation logic" comparison
#' for the package's core statistical question.
#'
#' Statistical question addressed:
#'
#' - At the same budget on the same decision instance, which allocation rule
#'   gives the strongest recommendation quality/runtime tradeoff?
#' - Do adaptive methods spend budget differently than equal allocation?
#'
#' @param board A `bg_board` object.
#' @param roll A `bg_roll` object.
#' @param methods Character vector of allocation methods.
#' @param total_budget Integer-like scalar total rollout budget per method.
#' @inheritParams evaluate_actions_equal
#'
#' @return A list of class `bg_method_comparison` with components:
#'   \describe{
#'     \item{`summary`}{Method-level table with recommended move, estimate, and runtime.}
#'     \item{`evaluations`}{Named list of full `bg_action_evaluation` objects.}
#'     \item{`legal_moves`}{Legal candidate move sequences used by all methods.}
#'     \item{`settings`}{Study settings.}
#'   }
#'
#' Interpretation notes:
#'
#' - Use `recommended_estimate` and `recommended_posterior_sd` together to
#'   avoid over-reading noisy point estimates.
#' - Use `recommended_prob_best` / `recommended_expected_regret` when available
#'   (`fast_diagnostics = FALSE`).
#' - Runtime should be interpreted jointly with regret/PCS metrics.
#' @export
#'
#' @examples
#' board <- bg_initial_board()
#' roll <- bg_roll(1L, 6L)
#' study <- compare_methods_on_position(
#'   board = board,
#'   roll = roll,
#'   methods = c("equal", "ucb", "thompson"),
#'   total_budget = 120L,
#'   rollout_policy = "random",
#'   max_rollout_turns = 200L,
#'   fast_diagnostics = TRUE,
#'   seed = 1L
#' )
#' study$summary
compare_methods_on_position <- function(
    board,
    roll,
    methods = c("thompson", "ttts", "equal", "ucb", "ocba", "greedy"),
    total_budget = 512L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    unresolved_value = 0.5,
    initial_allocations = 1L,
    ucb_exploration = 1,
    prior_alpha = 1,
    prior_beta = 1,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    fast_diagnostics = TRUE,
    seed = NULL) {
  if (!is_bg_board(board)) {
    stop("`board` must inherit from class 'bg_board'.", call. = FALSE)
  }
  bg_validate_board(board)

  roll <- bg_as_roll(roll)
  methods <- bg_normalize_study_methods(methods)
  total_budget <- bg_coerce_integerish(total_budget, "total_budget", 1L)
  if (total_budget < 1L) {
    stop("`total_budget` must be at least 1.", call. = FALSE)
  }

  legal_moves <- bg_legal_moves(board, roll)
  evaluations <- vector("list", length(methods))
  names(evaluations) <- methods
  rows <- vector("list", length(methods))

  for (i in seq_along(methods)) {
    method <- methods[[i]]
    method_seed <- bg_derive_seed(seed, "compare_methods_on_position", method, total_budget)

    ev <- bg_evaluate_actions_method(
      board = board,
      method = method,
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
      seed = method_seed
    )
    runtime <- bg_action_runtime_seconds(ev)
    evaluations[[i]] <- ev

    if (nrow(ev$results) == 0L) {
      rows[[i]] <- data.frame(
        method = method,
        n_legal_moves = 0L,
        recommended_index = NA_integer_,
        recommended_move_label = NA_character_,
        recommended_posterior_sd = NA_real_,
        recommended_estimate = NA_real_,
        recommended_prob_best = NA_real_,
        recommended_expected_regret = NA_real_,
        recommended_allocation_count = NA_integer_,
        estimate_gap_top2 = NA_real_,
        runtime_seconds = runtime,
        stringsAsFactors = FALSE
      )
      next
    }

    best_row <- bg_recommended_row(ev)
    estimate_gap_top2 <- bg_top_two_gap(ev$results$estimate)

    rows[[i]] <- data.frame(
      method = method,
      n_legal_moves = nrow(ev$results),
      recommended_index = ev$recommended_index,
      recommended_move_label = best_row$move_label[[1L]],
      recommended_posterior_sd = best_row$posterior_sd[[1L]],
      recommended_estimate = best_row$estimate[[1L]],
      recommended_prob_best = best_row$prob_best[[1L]],
      recommended_expected_regret = best_row$posterior_expected_regret[[1L]],
      recommended_allocation_count = best_row$allocation_count[[1L]],
      estimate_gap_top2 = estimate_gap_top2,
      runtime_seconds = runtime,
      stringsAsFactors = FALSE
    )
  }

  summary <- do.call(rbind, rows)
  rownames(summary) <- NULL

  out <- list(
    board = board,
    roll = roll,
    legal_moves = legal_moves,
    summary = summary,
    evaluations = evaluations,
    settings = list(
      methods = methods,
      total_budget = total_budget,
      rollout_policy = bg_match_rollout_policy(rollout_policy),
      max_rollout_turns = bg_coerce_integerish(max_rollout_turns, "max_rollout_turns", 1L),
      unresolved_value = as.numeric(unresolved_value),
      initial_allocations = bg_coerce_integerish(initial_allocations, "initial_allocations", 1L),
      ucb_exploration = as.numeric(ucb_exploration),
      prior_alpha = as.numeric(prior_alpha),
      prior_beta = as.numeric(prior_beta),
      dice_mode = match.arg(dice_mode),
      crn = isTRUE(crn),
      fast_diagnostics = isTRUE(fast_diagnostics),
      seed = if (is.null(seed)) NULL else bg_coerce_integerish(seed, "seed", 1L)
    )
  )

  class(out) <- "bg_method_comparison"
  out
}

#' Study budget-vs-accuracy tradeoff for one allocation method
#'
#' Evaluates the same method over a vector of budgets and compares each decision
#' to a large-budget proxy truth. This is the main single-position workflow for
#' studying speed/accuracy tradeoffs.
#'
#' Statistical question addressed:
#'
#' - How quickly does decision quality improve as simulation budget increases?
#' - Is there a practical budget region where gains begin to plateau?
#'
#' @param board A `bg_board` object.
#' @param roll A `bg_roll` object.
#' @param method Allocation method to evaluate across budgets.
#' @param budgets Integer-like budget vector.
#' @param truth_budget Integer-like budget used for the reference truth run.
#' @param truth_dice_mode Dice mode used for the truth run.
#' @param truth_crn Logical scalar indicating CRN use in truth construction.
#' @inheritParams evaluate_actions_equal
#'
#' @return A list of class `bg_budget_tradeoff` with components:
#'   \describe{
#'     \item{`results`}{Budget-level table with recommendation quality and runtime.}
#'     \item{`truth`}{The high-budget `bg_action_evaluation` reference object.}
#'     \item{`evaluations`}{Named list of per-budget `bg_action_evaluation` objects.}
#'     \item{`settings`}{Study settings.}
#'   }
#'
#' Interpretation notes:
#'
#' - `correct_selection` is a proxy PCS indicator against the reference-best
#'   action.
#' - `simple_regret` quantifies the value loss from the selected action.
#' - `mse` measures whole-vector estimation quality, not only recommendation.
#' - Improvements can be non-monotone at finite budgets due to simulation noise.
#' @export
#'
#' @examples
#' board <- bg_initial_board()
#' roll <- bg_roll(1L, 6L)
#' tradeoff <- study_budget_tradeoff(
#'   board = board,
#'   roll = roll,
#'   method = "thompson",
#'   budgets = c(120L, 360L, 900L),
#'   truth_budget = 2400L,
#'   rollout_policy = "random",
#'   max_rollout_turns = 200L,
#'   fast_diagnostics = TRUE,
#'   seed = 1L
#' )
#' tradeoff$results
study_budget_tradeoff <- function(
    board,
    roll,
    method = c("thompson", "ttts", "ocba", "ucb", "equal", "greedy"),
    budgets = c(128L, 512L, 2048L),
    truth_budget = 8192L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    unresolved_value = 0.5,
    initial_allocations = 1L,
    ucb_exploration = 1,
    prior_alpha = 1,
    prior_beta = 1,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    fast_diagnostics = TRUE,
    truth_dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    truth_crn = FALSE,
    seed = NULL) {
  method <- match.arg(method)
  if (!is_bg_board(board)) {
    stop("`board` must inherit from class 'bg_board'.", call. = FALSE)
  }
  bg_validate_board(board)

  roll <- bg_as_roll(roll)
  method <- bg_canonicalize_allocation_method(bg_match_allocation_method(method))
  budgets <- sort(bg_normalize_study_budgets(budgets))
  truth_budget <- bg_coerce_integerish(truth_budget, "truth_budget", 1L)
  if (truth_budget < max(budgets)) {
    stop("`truth_budget` should be at least `max(budgets)`.", call. = FALSE)
  }

  truth_dice_mode <- match.arg(truth_dice_mode)
  bg_assert_scalar_flag(truth_crn, "truth_crn")

  legal_moves <- bg_legal_moves(board, roll)
  truth <- approximate_action_truth(
    board = board,
    roll = roll,
    legal_moves = legal_moves,
    truth_budget = truth_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns,
    unresolved_value = unresolved_value,
    prior_alpha = prior_alpha,
    prior_beta = prior_beta,
    dice_mode = truth_dice_mode,
    crn = truth_crn,
    seed = bg_derive_seed(seed, "study_budget_tradeoff", "truth")
  )

  truth_info <- bg_reference_snapshot(truth)
  truth_best_index <- truth$recommended_index

  rows <- vector("list", length(budgets))
  evaluations <- vector("list", length(budgets))
  names(evaluations) <- as.character(budgets)

  for (i in seq_along(budgets)) {
    budget <- budgets[[i]]
    run_seed <- bg_derive_seed(seed, "study_budget_tradeoff", method, budget)

    ev <- bg_evaluate_actions_method(
      board = board,
      method = method,
      roll = roll,
      legal_moves = legal_moves,
      total_budget = budget,
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
      seed = run_seed
    )
    runtime <- bg_action_runtime_seconds(ev)
    evaluations[[i]] <- ev
    metrics <- bg_action_reference_metrics(
      evaluation = ev,
      reference_snapshot = truth_info,
      reference_best_index = truth_best_index
    )

    if (metrics$n_legal_moves == 0L) {
      rows[[i]] <- data.frame(
        method = method,
        total_budget = budget,
        n_legal_moves = 0L,
        chosen_index = NA_integer_,
        chosen_move_label = NA_character_,
        truth_best_index = NA_integer_,
        truth_best_move_label = NA_character_,
        chosen_estimate = NA_real_,
        chosen_truth_value = NA_real_,
        truth_best_value = NA_real_,
        correct_selection = NA,
        simple_regret = NA_real_,
        mse = NA_real_,
        runtime_seconds = runtime,
        stringsAsFactors = FALSE
      )
      next
    }

    rows[[i]] <- data.frame(
      method = method,
      total_budget = budget,
      n_legal_moves = metrics$n_legal_moves,
      chosen_index = metrics$chosen_index,
      chosen_move_label = metrics$chosen_move_label,
      truth_best_index = truth_best_index,
      truth_best_move_label = truth_info$label_lookup[[as.character(truth_best_index)]],
      chosen_estimate = metrics$chosen_estimate,
      chosen_truth_value = metrics$chosen_reference_value,
      truth_best_value = truth_info$best_value,
      correct_selection = metrics$correct_selection,
      simple_regret = metrics$simple_regret,
      mse = metrics$mse,
      runtime_seconds = runtime,
      stringsAsFactors = FALSE
    )
  }

  results <- do.call(rbind, rows)
  rownames(results) <- NULL

  out <- list(
    board = board,
    roll = roll,
    results = results,
    truth = truth,
    evaluations = evaluations,
    settings = list(
      method = method,
      budgets = budgets,
      truth_budget = truth_budget,
      rollout_policy = bg_match_rollout_policy(rollout_policy),
      max_rollout_turns = bg_coerce_integerish(max_rollout_turns, "max_rollout_turns", 1L),
      unresolved_value = as.numeric(unresolved_value),
      initial_allocations = bg_coerce_integerish(initial_allocations, "initial_allocations", 1L),
      ucb_exploration = as.numeric(ucb_exploration),
      prior_alpha = as.numeric(prior_alpha),
      prior_beta = as.numeric(prior_beta),
      dice_mode = match.arg(dice_mode),
      crn = isTRUE(crn),
      fast_diagnostics = isTRUE(fast_diagnostics),
      truth_dice_mode = truth_dice_mode,
      truth_crn = isTRUE(truth_crn),
      seed = if (is.null(seed)) NULL else bg_coerce_integerish(seed, "seed", 1L)
    )
  )

  class(out) <- "bg_budget_tradeoff"
  out
}

#' Compare dice/CRN variance controls on one decision problem
#'
#' Evaluates a fixed allocation method across a crossed grid of `dice_mode` and
#' `crn` settings, then compares each configuration to a high-budget reference.
#'
#' This isolates variance-control effects while keeping the policy and budget
#' fixed.
#'
#' Statistical question addressed:
#'
#' - How sensitive is recommendation quality to variance-control design choices?
#' - Do stratification and common random numbers reduce regret/MSE for this
#'   decision problem?
#'
#' @param board A `bg_board` object.
#' @param roll A `bg_roll` object.
#' @param method Allocation method.
#' @param total_budget Integer-like rollout budget for each configuration.
#' @param dice_modes Character vector of dice modes to compare.
#' @param crn_values Logical vector of CRN settings to compare.
#' @param truth_budget Integer-like budget for reference truth.
#' @param truth_dice_mode Dice mode used for truth construction.
#' @param truth_crn Logical scalar indicating CRN use in truth construction.
#' @inheritParams evaluate_actions_equal
#'
#' @return A list of class `bg_variance_control_study` with components:
#'   \describe{
#'     \item{`results`}{Configuration-level table with decision quality and runtime.}
#'     \item{`truth`}{The high-budget `bg_action_evaluation` reference object.}
#'     \item{`evaluations`}{Named list of per-configuration evaluations.}
#'     \item{`settings`}{Study settings.}
#'   }
#'
#' Interpretation notes:
#'
#' - Lower `mse` or `simple_regret` indicates more stable finite-budget
#'   estimation under the tested variance controls.
#' - Effects can be state-dependent; repeat on multiple benchmark cases before
#'   drawing broad conclusions.
#' @export
#'
#' @examples
#' board <- bg_initial_board()
#' roll <- bg_roll(1L, 6L)
#' var_study <- study_variance_controls(
#'   board = board,
#'   roll = roll,
#'   method = "thompson",
#'   total_budget = 320L,
#'   dice_modes = c("iid", "stratified_first_roll"),
#'   crn_values = c(FALSE, TRUE),
#'   truth_budget = 2000L,
#'   rollout_policy = "random",
#'   max_rollout_turns = 200L,
#'   fast_diagnostics = TRUE,
#'   seed = 1L
#' )
#' var_study$results
study_variance_controls <- function(
    board,
    roll,
    method = c("thompson", "ttts", "ocba", "ucb", "equal", "greedy"),
    total_budget = 1024L,
    dice_modes = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn_values = c(FALSE, TRUE),
    truth_budget = 8192L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    unresolved_value = 0.5,
    initial_allocations = 1L,
    ucb_exploration = 1,
    prior_alpha = 1,
    prior_beta = 1,
    fast_diagnostics = TRUE,
    truth_dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    truth_crn = FALSE,
    seed = NULL) {
  method <- match.arg(method)
  if (!is_bg_board(board)) {
    stop("`board` must inherit from class 'bg_board'.", call. = FALSE)
  }
  bg_validate_board(board)

  roll <- bg_as_roll(roll)
  method <- bg_canonicalize_allocation_method(bg_match_allocation_method(method))
  total_budget <- bg_coerce_integerish(total_budget, "total_budget", 1L)
  if (total_budget < 1L) {
    stop("`total_budget` must be at least 1.", call. = FALSE)
  }
  truth_budget <- bg_coerce_integerish(truth_budget, "truth_budget", 1L)
  if (truth_budget < total_budget) {
    stop("`truth_budget` should be at least `total_budget`.", call. = FALSE)
  }

  valid_dice_modes <- c("iid", "stratified_first_roll", "stratified_first_two_rolls")
  if (!is.character(dice_modes) || length(dice_modes) < 1L || anyNA(dice_modes)) {
    stop("`dice_modes` must be a non-empty character vector.", call. = FALSE)
  }
  if (!all(dice_modes %in% valid_dice_modes)) {
    stop(
      sprintf("`dice_modes` values must be one of: %s.", paste(valid_dice_modes, collapse = ", ")),
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

  legal_moves <- bg_legal_moves(board, roll)
  truth <- approximate_action_truth(
    board = board,
    roll = roll,
    legal_moves = legal_moves,
    truth_budget = truth_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns,
    unresolved_value = unresolved_value,
    prior_alpha = prior_alpha,
    prior_beta = prior_beta,
    dice_mode = truth_dice_mode,
    crn = truth_crn,
    seed = bg_derive_seed(seed, "study_variance_controls", "truth")
  )

  truth_info <- bg_reference_snapshot(truth)
  truth_best_index <- truth$recommended_index

  rows <- list()
  evaluations <- list()
  row_id <- 1L

  for (dice_mode in dice_modes) {
    for (crn in crn_values) {
      run_seed <- bg_derive_seed(seed, "study_variance_controls", method, total_budget, dice_mode, crn)
      key <- paste0(dice_mode, "_crn_", if (crn) "true" else "false")

      ev <- bg_evaluate_actions_method(
        board = board,
        method = method,
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
        seed = run_seed
      )
      runtime <- bg_action_runtime_seconds(ev)
      evaluations[[key]] <- ev
      metrics <- bg_action_reference_metrics(
        evaluation = ev,
        reference_snapshot = truth_info,
        reference_best_index = truth_best_index
      )

      if (metrics$n_legal_moves == 0L) {
        rows[[row_id]] <- data.frame(
          method = method,
          total_budget = total_budget,
          dice_mode = dice_mode,
          crn = crn,
          n_legal_moves = 0L,
          chosen_index = NA_integer_,
          chosen_move_label = NA_character_,
          truth_best_index = NA_integer_,
          truth_best_move_label = NA_character_,
          chosen_estimate = NA_real_,
          chosen_truth_value = NA_real_,
          truth_best_value = NA_real_,
          correct_selection = NA,
          simple_regret = NA_real_,
          mse = NA_real_,
          runtime_seconds = runtime,
          stringsAsFactors = FALSE
        )
        row_id <- row_id + 1L
        next
      }

      rows[[row_id]] <- data.frame(
        method = method,
        total_budget = total_budget,
        dice_mode = dice_mode,
        crn = crn,
        n_legal_moves = metrics$n_legal_moves,
        chosen_index = metrics$chosen_index,
        chosen_move_label = metrics$chosen_move_label,
        truth_best_index = truth_best_index,
        truth_best_move_label = truth_info$label_lookup[[as.character(truth_best_index)]],
        chosen_estimate = metrics$chosen_estimate,
        chosen_truth_value = metrics$chosen_reference_value,
        truth_best_value = truth_info$best_value,
        correct_selection = metrics$correct_selection,
        simple_regret = metrics$simple_regret,
        mse = metrics$mse,
        runtime_seconds = runtime,
        stringsAsFactors = FALSE
      )
      row_id <- row_id + 1L
    }
  }

  results <- do.call(rbind, rows)
  rownames(results) <- NULL

  out <- list(
    board = board,
    roll = roll,
    results = results,
    truth = truth,
    evaluations = evaluations,
    settings = list(
      method = method,
      total_budget = total_budget,
      dice_modes = dice_modes,
      crn_values = crn_values,
      truth_budget = truth_budget,
      rollout_policy = bg_match_rollout_policy(rollout_policy),
      max_rollout_turns = bg_coerce_integerish(max_rollout_turns, "max_rollout_turns", 1L),
      unresolved_value = as.numeric(unresolved_value),
      initial_allocations = bg_coerce_integerish(initial_allocations, "initial_allocations", 1L),
      ucb_exploration = as.numeric(ucb_exploration),
      prior_alpha = as.numeric(prior_alpha),
      prior_beta = as.numeric(prior_beta),
      fast_diagnostics = isTRUE(fast_diagnostics),
      truth_dice_mode = truth_dice_mode,
      truth_crn = isTRUE(truth_crn),
      seed = if (is.null(seed)) NULL else bg_coerce_integerish(seed, "seed", 1L)
    )
  )

  class(out) <- "bg_variance_control_study"
  out
}

#' Summarize a method-comparison study
#'
#' @param object A `bg_method_comparison` object.
#' @param ... Unused.
#'
#' @return A method-level summary data frame.
#' @export
summary.bg_method_comparison <- function(object, ...) {
  if (!inherits(object, "bg_method_comparison")) {
    stop("`object` must inherit from class 'bg_method_comparison'.", call. = FALSE)
  }
  object$summary
}

#' Print a method-comparison study
#'
#' @param x A `bg_method_comparison` object.
#' @param n Integer-like scalar controlling how many method rows to print.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.bg_method_comparison <- function(x, n = 20L, ...) {
  if (!inherits(x, "bg_method_comparison")) {
    stop("`x` must inherit from class 'bg_method_comparison'.", call. = FALSE)
  }

  cat("<bg_method_comparison>\n", sep = "")
  cat("methods:      ", paste(x$settings$methods, collapse = ", "), "\n", sep = "")
  cat("total_budget: ", x$settings$total_budget, "\n", sep = "")
  if (nrow(x$summary) == 0L) {
    cat("No legal moves were available.\n", sep = "")
    return(invisible(x))
  }
  compact <- bg_compact_method_comparison_table(x$summary, n = n)
  print(compact, row.names = FALSE)
  if (nrow(x$summary) > nrow(compact)) {
    cat("showing_first: ", nrow(compact), " of ", nrow(x$summary), " methods\n", sep = "")
  }
  if ("recommended_prob_best" %in% names(x$summary) && all(is.na(x$summary$recommended_prob_best))) {
    cat("note: probability-best and regret diagnostics are unavailable in this run (`fast_diagnostics = TRUE`).\n", sep = "")
  }
  invisible(x)
}

#' Summarize a budget-tradeoff study
#'
#' @param object A `bg_budget_tradeoff` object.
#' @param ... Unused.
#'
#' @return A budget-level results data frame.
#' @export
summary.bg_budget_tradeoff <- function(object, ...) {
  if (!inherits(object, "bg_budget_tradeoff")) {
    stop("`object` must inherit from class 'bg_budget_tradeoff'.", call. = FALSE)
  }
  object$results
}

#' Print a budget-tradeoff study
#'
#' @param x A `bg_budget_tradeoff` object.
#' @param n Integer-like scalar controlling how many budget rows to print.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.bg_budget_tradeoff <- function(x, n = 20L, ...) {
  if (!inherits(x, "bg_budget_tradeoff")) {
    stop("`x` must inherit from class 'bg_budget_tradeoff'.", call. = FALSE)
  }

  cat("<bg_budget_tradeoff>\n", sep = "")
  cat("method:       ", x$settings$method, "\n", sep = "")
  cat("budgets:      ", paste(x$settings$budgets, collapse = ", "), "\n", sep = "")
  cat("truth_budget: ", x$settings$truth_budget, "\n", sep = "")
  compact <- bg_compact_budget_tradeoff_table(x$results, n = n)
  print(compact, row.names = FALSE)
  if (nrow(x$results) > nrow(compact)) {
    cat("showing_first: ", nrow(compact), " of ", nrow(x$results), " budget rows\n", sep = "")
  }
  invisible(x)
}

#' Summarize a variance-control study
#'
#' @param object A `bg_variance_control_study` object.
#' @param ... Unused.
#'
#' @return A configuration-level results data frame.
#' @export
summary.bg_variance_control_study <- function(object, ...) {
  if (!inherits(object, "bg_variance_control_study")) {
    stop("`object` must inherit from class 'bg_variance_control_study'.", call. = FALSE)
  }
  object$results
}

#' Print a variance-control study
#'
#' @param x A `bg_variance_control_study` object.
#' @param n Integer-like scalar controlling how many configuration rows to
#'   print.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.bg_variance_control_study <- function(x, n = 20L, ...) {
  if (!inherits(x, "bg_variance_control_study")) {
    stop("`x` must inherit from class 'bg_variance_control_study'.", call. = FALSE)
  }

  cat("<bg_variance_control_study>\n", sep = "")
  cat("method:       ", x$settings$method, "\n", sep = "")
  cat("total_budget: ", x$settings$total_budget, "\n", sep = "")
  cat("truth_budget: ", x$settings$truth_budget, "\n", sep = "")
  compact <- bg_compact_variance_table(x$results, n = n)
  print(compact, row.names = FALSE)
  if (nrow(x$results) > nrow(compact)) {
    cat("showing_first: ", nrow(compact), " of ", nrow(x$results), " configurations\n", sep = "")
  }
  invisible(x)
}

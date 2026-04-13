# Truth/reference workflows, opening-truth helpers, and truth-stability studies.
#
# This file collects the package's proxy-truth layer in one place so the
# rollout-model estimand, cache logic, opening-roll helpers, and truth
# stability workflows can be read together.

# -----------------------------------------------------------------------------
# Source: bg_reference.R
# -----------------------------------------------------------------------------
# Proxy-reference construction and sufficient-stat accumulation.
#
# `bg_reference()` is the core proxy-truth builder underneath the higher-level
# truth cache helpers. This file is where rollout counts become one
# Monte-Carlo-based reference table for a local decision problem.

bg_default_workers_truth <- function(max_workers = 12L) {
  detected <- suppressWarnings(parallel::detectCores(logical = FALSE))
  if (!is.numeric(detected) || length(detected) != 1L || is.na(detected) || detected < 1L) {
    return(as.integer(1L))
  }
  as.integer(min(max_workers, detected))
}

bg_scored_outcome_columns <- function() {
  c(
    "single_loss",
    "gammon_loss",
    "backgammon_loss",
    "unresolved",
    "single_win",
    "gammon_win",
    "backgammon_win"
  )
}

bg_stats_table_template <- function(candidate_index) {
  candidate_index <- as.integer(candidate_index)
  n <- length(candidate_index)
  tab <- data.frame(
    candidate_index = candidate_index,
    allocation_count = integer(n),
    wins = integer(n),
    losses = integer(n),
    stringsAsFactors = FALSE
  )
  for (nm in bg_scored_outcome_columns()) {
    tab[[nm]] <- integer(n)
  }
  tab$reward_sum <- numeric(n)
  tab$reward_sum_sq <- numeric(n)
  tab
}

bg_reference_stats_columns <- function() {
  c(
    "candidate_index",
    "allocation_count",
    "wins",
    "losses",
    bg_scored_outcome_columns(),
    "reward_sum",
    "reward_sum_sq"
  )
}

bg_reference_carry_stats_table <- function(action_table) {
  needed <- bg_reference_stats_columns()
  missing <- setdiff(needed, names(action_table))
  if (length(missing) > 0L) {
    stop(
      "Reference action tables must carry the full sufficient-stat set. Missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  action_table[order(action_table$candidate_index), needed, drop = FALSE]
}

bg_reward_map_from_problem <- function(problem) {
  reward_model <- problem$settings$reward_model_canonical
  unresolved_value <- problem$settings$unresolved_value

  if (identical(reward_model, "categorical_outcome")) {
    payoff <- problem$settings$posterior_prior$payoff
    if (is.null(payoff)) {
      payoff <- bg_default_scored_payoff_map(unresolved_value)
    }
    payoff <- as.numeric(payoff)
    names(payoff) <- if (length(payoff) == 7L) bg_scored_outcome_columns() else c("loss", "unresolved", "win")
    return(payoff)
  }

  stats::setNames(
    c(0, 0, 0, unresolved_value, 1, 1, 1),
    bg_scored_outcome_columns()
  )
}

bg_recompute_rollout_rewards <- function(problem, tab) {
  if (nrow(tab) == 0L) {
    return(tab)
  }

  for (nm in setdiff(bg_scored_outcome_columns(), names(tab))) {
    tab[[nm]] <- 0L
  }

  tab$losses <- tab$single_loss + tab$gammon_loss + tab$backgammon_loss
  tab$wins <- tab$single_win + tab$gammon_win + tab$backgammon_win

  reward_map <- bg_reward_map_from_problem(problem)
  if (length(reward_map) == 7L) {
    reward_mat <- as.matrix(tab[, bg_scored_outcome_columns(), drop = FALSE])
    reward_vec <- as.numeric(reward_map[bg_scored_outcome_columns()])
    tab$reward_sum <- as.numeric(reward_mat %*% reward_vec)
    tab$reward_sum_sq <- as.numeric(reward_mat %*% (reward_vec^2))
  } else {
    tab$reward_sum <- (tab$wins * reward_map[[3L]]) + (tab$unresolved * reward_map[[2L]]) + (tab$losses * reward_map[[1L]])
    tab$reward_sum_sq <- (tab$wins * reward_map[[3L]]^2) + (tab$unresolved * reward_map[[2L]]^2) + (tab$losses * reward_map[[1L]]^2)
  }

  tab
}

bg_empty_reference_action_table <- function(problem) {
  tab <- problem$candidate_table
  stats <- bg_stats_table_template(tab$candidate_index)
  tab <- merge(tab, stats, by = "candidate_index", sort = FALSE, all.x = TRUE)
  tab <- tab[match(problem$candidate_table$candidate_index, tab$candidate_index), , drop = FALSE]
  tab$reference_mean <- numeric(nrow(tab))
  tab$sample_variance <- numeric(nrow(tab))
  tab$reference_se <- numeric(nrow(tab))
  tab$reference_mc_lower_95 <- numeric(nrow(tab))
  tab$reference_mc_upper_95 <- numeric(nrow(tab))
  tab$reference_interval_type <- rep(bg_reference_interval_type(), nrow(tab))
  tab$reference_alpha <- numeric(nrow(tab))
  tab$reference_beta <- numeric(nrow(tab))
  tab$unresolved_fraction <- numeric(nrow(tab))
  tab$rank <- integer(nrow(tab))
  tab
}

bg_empty_reference_object <- function(problem, reference_budget, reference_mode, workers_truth, truth_block_size) {
  structure(
    list(
      problem = problem,
      action_table = bg_empty_reference_action_table(problem),
      summary = data.frame(
        reference_budget = reference_budget,
        proxy_reference_best_index = NA_integer_,
        proxy_reference_best_move_label = NA_character_,
        top_two_gap_estimate = NA_real_,
        top_two_gap_se = NA_real_,
        top_two_gap_mc_lower_95 = NA_real_,
        top_two_gap_mc_upper_95 = NA_real_,
        top_two_gap_interval_type = bg_gap_interval_type(),
        mc_gap_excludes_zero = NA,
        difficulty_label = NA_character_,
        reference_mode = reference_mode,
        simulation_policy = problem$settings$simulation_policy,
        simulation_policy_engine = problem$settings$simulation_policy_engine,
        reward_model = problem$settings$reward_model,
        posterior_model = problem$settings$posterior_model,
        workers_truth = workers_truth,
        truth_block_size = truth_block_size,
        collapse_reduction = length(problem$legal_moves),
        stringsAsFactors = FALSE
      ),
      warnings = "No legal moves are available for this problem; the proxy-reference ranking is empty.",
      settings = list(
        reference_budget = reference_budget,
        reference_mode = reference_mode,
        workers_truth = workers_truth,
        truth_block_size = truth_block_size,
        prior_alpha = problem$settings$prior_alpha,
        prior_beta = problem$settings$prior_beta,
        reward_model = problem$settings$reward_model,
        posterior_model = problem$settings$posterior_model
      )
    ),
    class = "bg_reference"
  )
}

bg_reference_snapshot_public <- function(reference) {
  tab <- reference$action_table[order(reference$action_table$candidate_index), , drop = FALSE]
  if (nrow(tab) == 0L) {
    return(list(
      table = tab,
      value_lookup = numeric(0L),
      rank_lookup = integer(0L),
      label_lookup = character(0L),
      best_value = NA_real_,
      best_index = NA_integer_
    ))
  }
  value_lookup <- stats::setNames(tab$reference_mean, tab$candidate_index)
  rank_lookup <- stats::setNames(tab$rank, tab$candidate_index)
  label_lookup <- stats::setNames(tab$move_label, tab$candidate_index)
  best_value <- max(tab$reference_mean)
  list(
    table = tab,
    value_lookup = value_lookup,
    rank_lookup = rank_lookup,
    label_lookup = label_lookup,
    best_value = best_value,
    best_index = tab$candidate_index[[which.max(tab$reference_mean)]]
  )
}

bg_reference_finalize_from_stats <- function(
    problem,
    tab,
    prior_alpha,
    prior_beta,
    reference_budget,
    reference_mode,
    workers_truth,
    truth_block_size,
    dice_mode = "iid",
    crn = FALSE,
    warnings_extra = character()) {
  # Finalize one reference object from a full per-candidate sufficient-stat
  # table. This helper is shared by:
  # - ordinary truth building after new rollout blocks have been merged; and
  # - reward-system projection, where the scored outcome counts stay fixed but
  #   the reward map used to summarize them changes.
  reference_stats <- bg_cpp_reference_summary(
    allocation_count = as.integer(tab$allocation_count),
    unresolved = as.integer(tab$unresolved),
    reward_sum = as.numeric(tab$reward_sum),
    reward_sum_sq = as.numeric(tab$reward_sum_sq),
    prior_alpha = as.numeric(prior_alpha),
    prior_beta = as.numeric(prior_beta)
  )
  tab$reference_mean <- reference_stats$reference_mean
  tab$sample_variance <- reference_stats$sample_variance
  tab$reference_se <- reference_stats$reference_se
  tab$reference_mc_lower_95 <- reference_stats$reference_mc_lower_95
  tab$reference_mc_upper_95 <- reference_stats$reference_mc_upper_95
  tab$reference_interval_type <- reference_stats$reference_interval_type
  tab$reference_alpha <- reference_stats$reference_alpha
  tab$reference_beta <- reference_stats$reference_beta
  tab$unresolved_fraction <- reference_stats$unresolved_fraction
  tab <- tab[order(-tab$reference_mean, tab$candidate_index), , drop = FALSE]
  tab$rank <- seq_len(nrow(tab))
  rownames(tab) <- NULL

  top_two <- sort(tab$reference_mean, decreasing = TRUE)
  top_two <- c(top_two, rep(NA_real_, max(0L, 2L - length(top_two))))
  gap <- top_two[[1L]] - top_two[[2L]]
  top_rows <- head(tab, 2L)
  gap_se <- if (nrow(top_rows) >= 2L) {
    sqrt(sum(top_rows$reference_se^2))
  } else {
    NA_real_
  }
  lower_95 <- if (is.na(gap_se)) NA_real_ else gap - 1.96 * gap_se
  upper_95 <- if (is.na(gap_se)) NA_real_ else gap + 1.96 * gap_se
  difficulty_label <- stratify_positions_by_difficulty(gap)

  summary <- data.frame(
    reference_budget = reference_budget,
    proxy_reference_best_index = tab$candidate_index[[1L]],
    proxy_reference_best_move_label = tab$move_label[[1L]],
    top_two_gap_estimate = gap,
    top_two_gap_se = gap_se,
    top_two_gap_mc_lower_95 = lower_95,
    top_two_gap_mc_upper_95 = upper_95,
    top_two_gap_interval_type = bg_gap_interval_type(),
    mc_gap_excludes_zero = isTRUE(is.finite(lower_95) && lower_95 > 0),
    reference_is_approximate = identical(reference_mode, "focused"),
    difficulty_label = as.character(difficulty_label[[1L]]),
    reference_mode = reference_mode,
    simulation_policy = problem$settings$simulation_policy,
    simulation_policy_engine = problem$settings$simulation_policy_engine,
    reward_model = problem$settings$reward_model,
    posterior_model = problem$settings$posterior_model,
    workers_truth = workers_truth,
    truth_block_size = truth_block_size,
    dice_mode = dice_mode,
    crn = isTRUE(crn),
    collapse_reduction = length(problem$legal_moves) - nrow(problem$candidate_table),
    stringsAsFactors = FALSE
  )

  warnings <- character(0L)
  if (!summary$mc_gap_excludes_zero[[1L]]) {
    warnings <- c(
      warnings,
      "The proxy-reference top-two Monte Carlo gap interval does not exclude zero; treat the proxy-reference ranking cautiously."
    )
  }
  if (identical(reference_mode, "focused")) {
    warnings <- c(
      warnings,
      "Proxy reference used `reference_mode = 'focused'`; this is an acceleration strategy for proxy-reference construction rather than the package's most conservative equal-allocation reference."
    )
  }
  if (mean(tab$unresolved_fraction, na.rm = TRUE) > 0.1) {
    warnings <- c(
      warnings,
      "Proxy reference unresolved fraction is high; truncation may materially affect the model-relative ranking."
    )
  }
  if (problem$settings$simulation_policy != "random") {
    warnings <- c(
      warnings,
      sprintf(
        "Proxy reference uses simulation policy '%s' instead of the benchmark default 'random'.",
        problem$settings$simulation_policy
      )
    )
  }
  if (isTRUE(crn)) {
    warnings <- c(
      warnings,
      "Proxy reference used common random numbers. The reported top-gap Monte Carlo interval is still a simple approximation and does not model induced covariance."
    )
  }
  warnings <- unique(c(warnings, warnings_extra))

  structure(
    list(
      problem = problem,
      action_table = tab,
      summary = summary,
      warnings = warnings,
      settings = list(
        reference_budget = reference_budget,
        reference_mode = reference_mode,
        workers_truth = workers_truth,
        truth_block_size = truth_block_size,
        dice_mode = dice_mode,
        crn = isTRUE(crn),
        reference_is_approximate = identical(reference_mode, "focused"),
        prior_alpha = prior_alpha,
        prior_beta = prior_beta,
        reward_model = problem$settings$reward_model,
        posterior_model = problem$settings$posterior_model
      )
    ),
    class = "bg_reference"
  )
}

bg_merge_reference_results <- function(problem, previous, added, prior_alpha, prior_beta, reference_budget, reference_mode, workers_truth, truth_block_size, dice_mode, crn) {
  candidate_table <- problem$candidate_table[order(problem$candidate_table$candidate_index), , drop = FALSE]
  prev <- previous[order(previous$candidate_index), , drop = FALSE]
  add <- added[order(added$candidate_index), , drop = FALSE]

  tab <- candidate_table
  tab$allocation_count <- prev$allocation_count + add$added_allocation_count
  tab$wins <- prev$wins + add$wins
  tab$losses <- prev$losses + add$losses
  for (nm in bg_scored_outcome_columns()) {
    tab[[nm]] <- prev[[nm]] + add[[nm]]
  }
  tab$reward_sum <- prev$reward_sum + add$reward_sum
  tab$reward_sum_sq <- prev$reward_sum_sq + add$reward_sum_sq
  bg_reference_finalize_from_stats(
    problem = problem,
    tab = tab,
    prior_alpha = prior_alpha,
    prior_beta = prior_beta,
    reference_budget = reference_budget,
    reference_mode = reference_mode,
    workers_truth = workers_truth,
    truth_block_size = truth_block_size,
    dice_mode = dice_mode,
    crn = crn
  )
}

bg_truth_projection_posterior <- function(reward_model, posterior_model = NULL) {
  reward_model <- bg_match_reward_model_public(reward_model)
  if (!is.null(posterior_model)) {
    return(bg_match_posterior_model_public(posterior_model))
  }
  bg_recommended_posterior_models(reward_model)[[1L]]
}

bg_truth_align_requested_stack <- function(
    truth,
    reward_model,
    posterior_model,
    posterior_prior = NULL,
    unresolved_value = truth$problem$settings$unresolved_value) {
  # Cached truth files are indexed by rollout/truth identity, not by the
  # later posterior family. When a caller loads one reward-system cache but
  # asks for a different coherent posterior within that same reward system,
  # project the loaded truth object onto the requested stack before returning
  # it so downstream code sees the expected problem metadata.
  truth <- bg_normalize_loaded_truth_object(truth)
  reward_model <- bg_match_reward_model_public(reward_model)
  posterior_model <- bg_match_posterior_model_public(posterior_model)

  same_stack <- identical(truth$problem$settings$reward_model_canonical, reward_model) &&
    identical(truth$problem$settings$posterior_model_canonical, posterior_model) &&
    isTRUE(all.equal(
      truth$problem$settings$unresolved_value,
      unresolved_value,
      tolerance = 1e-12
    )) &&
    is.null(posterior_prior)

  if (same_stack) {
    return(truth)
  }

  projected <- bg_truth_project(
    x = truth,
    reward_model = reward_model,
    posterior_model = posterior_model,
    posterior_prior = posterior_prior,
    unresolved_value = unresolved_value
  )
  projected$save_path <- truth$save_path
  projected$summary$save_path <- truth$summary$save_path
  projected
}

#' Project a proxy reference onto a different reward system
#'
#' `bg_reference_project()` reuses the full scored outcome counts stored in a
#' high-budget reference object and rebuilds the reference summary under a new
#' coherent reward/posterior stack. No new rollouts are simulated.
#'
#' @param reference A `bg_reference` or `bg_truth_state` object.
#' @param reward_model Target reward definition.
#' @param posterior_model Target posterior family. Defaults to the package's
#'   primary posterior family for the chosen `reward_model`.
#' @param posterior_prior Optional named list overriding the projected prior.
#' @param unresolved_value Optional unresolved payoff override. Defaults to `0`
#'   for `win_loss` and otherwise preserves the source truth's unresolved
#'   value.
#' @param problem_id Optional projected problem identifier.
#'
#' @return A projected `bg_reference` object.
#' @export
bg_reference_project <- function(
    reference,
    reward_model = c("scalar_payoff", "win_loss", "categorical_outcome"),
    posterior_model = NULL,
    posterior_prior = NULL,
    unresolved_value = NULL,
    problem_id = NULL) {
  reference <- bg_normalize_truth_reference(reference, "reference")
  reward_model <- bg_match_reward_model_public(reward_model)
  posterior_model <- bg_truth_projection_posterior(reward_model, posterior_model)

  if (is.null(unresolved_value)) {
    unresolved_value <- if (identical(reward_model, "win_loss")) {
      0
    } else {
      reference$problem$settings$unresolved_value
    }
  }

  projected_problem <- bg_problem_clone_models(
    problem = reference$problem,
    reward_model = reward_model,
    posterior_model = posterior_model,
    posterior_prior = posterior_prior,
    unresolved_value = unresolved_value,
    problem_id = if (is.null(problem_id)) reference$problem$problem_id else problem_id,
    cache = FALSE
  )

  stats_only <- bg_reference_carry_stats_table(reference$action_table)
  tab <- merge(
    projected_problem$candidate_table,
    stats_only,
    by = "candidate_index",
    sort = FALSE,
    all.x = TRUE
  )
  tab <- tab[match(projected_problem$candidate_table$candidate_index, tab$candidate_index), , drop = FALSE]
  tab <- bg_recompute_rollout_rewards(projected_problem, tab)

  workers_truth <- if (!is.null(reference$settings$workers_truth)) {
    reference$settings$workers_truth
  } else {
    reference$summary$workers_truth[[1L]]
  }
  truth_block_size <- if (!is.null(reference$settings$truth_block_size)) {
    reference$settings$truth_block_size
  } else {
    reference$summary$truth_block_size[[1L]]
  }
  dice_mode <- if ("dice_mode" %in% names(reference$summary)) {
    reference$summary$dice_mode[[1L]]
  } else {
    "iid"
  }
  crn <- if ("crn" %in% names(reference$summary)) {
    isTRUE(reference$summary$crn[[1L]])
  } else {
    FALSE
  }

  bg_reference_finalize_from_stats(
    problem = projected_problem,
    tab = tab,
    prior_alpha = projected_problem$settings$prior_alpha,
    prior_beta = projected_problem$settings$prior_beta,
    reference_budget = reference$summary$reference_budget[[1L]],
    reference_mode = reference$summary$reference_mode[[1L]],
    workers_truth = workers_truth,
    truth_block_size = truth_block_size,
    dice_mode = dice_mode,
    crn = crn,
    warnings_extra = sprintf(
      "Projected from cached scored-outcome counts under `%s + %s`; no new rollouts were simulated.",
      reference$problem$settings$reward_model,
      reference$problem$settings$posterior_model
    )
  )
}

#' Project a truth object onto a different reward system
#'
#' `bg_truth_project()` reuses the stored scored outcome counts from an existing
#' truth object and rebuilds the truth under a new coherent reward/posterior
#' stack. It can project either one truth state or an opening truth battery.
#'
#' @param x A `bg_truth_state` or `bg_truth_battery` object.
#' @param reward_model Target reward definition.
#' @param posterior_model Target posterior family. Defaults to the package's
#'   primary posterior family for the chosen `reward_model`.
#' @param posterior_prior Optional named list overriding the projected prior.
#' @param unresolved_value Optional unresolved payoff override.
#' @param save_path Optional path used to save the projected object.
#' @param overwrite Logical scalar controlling replacement of `save_path`.
#'
#' @return A projected truth object of the same class as `x`.
#' @export
bg_truth_project <- function(
    x,
    reward_model = c("scalar_payoff", "win_loss", "categorical_outcome"),
    posterior_model = NULL,
    posterior_prior = NULL,
    unresolved_value = NULL,
    save_path = NULL,
    overwrite = FALSE) {
  reward_model <- bg_match_reward_model_public(reward_model)
  posterior_model <- bg_truth_projection_posterior(reward_model, posterior_model)
  bg_assert_scalar_flag(overwrite, "overwrite")

  if (inherits(x, "bg_truth_battery")) {
    projected_truths <- lapply(
      x$truths,
      function(truth_i) {
        bg_truth_project(
          x = truth_i,
          reward_model = reward_model,
          posterior_model = posterior_model,
          posterior_prior = posterior_prior,
          unresolved_value = unresolved_value,
          save_path = NULL,
          overwrite = overwrite
        )
      }
    )
    out <- bg_opening_truth_battery_from_truths(projected_truths)
    out$settings$projection_reward_model <- reward_model
    out$settings$projection_posterior_model <- posterior_model
    if (!is.null(save_path)) {
      bg_truth_save(out, save_path, overwrite = overwrite)
    }
    return(out)
  }

  if (!inherits(x, "bg_truth_state")) {
    stop("`x` must be a `bg_truth_state` or `bg_truth_battery` object.", call. = FALSE)
  }

  projected_reference <- bg_reference_project(
    reference = x$reference,
    reward_model = reward_model,
    posterior_model = posterior_model,
    posterior_prior = posterior_prior,
    unresolved_value = unresolved_value,
    problem_id = x$problem$problem_id
  )
  projected_truth <- bg_make_truth_state(
    problem = projected_reference$problem,
    reference = projected_reference,
    seed = x$metadata$seed,
    n_cores = x$metadata$n_cores,
    parallel = isTRUE(x$metadata$parallel),
    save_path = save_path
  )
  projected_truth$metadata$projection_source_hash <- x$metadata$truth_hash
  projected_truth$metadata$projection_source_reward_model <- x$problem$settings$reward_model
  projected_truth$metadata$projection_source_posterior_model <- x$problem$settings$posterior_model

  if (!is.null(save_path)) {
    bg_truth_save(projected_truth, save_path, overwrite = overwrite)
  }
  projected_truth
}

bg_call_rollout_blocks <- function(
    problem,
    candidate_index,
    block_rollouts,
    start_counts,
    task_block_size,
    dice_mode,
    crn,
    seed) {
  out <- bg_cpp_rollout_blocks(
    unclass(problem$board),
    lapply(problem$legal_moves, bg_unclass_move_sequence),
    as.integer(candidate_index),
    as.integer(block_rollouts),
    as.integer(start_counts),
    problem$settings$simulation_policy_engine,
    problem$settings$max_rollout_turns,
    problem$settings$unresolved_value,
    dice_mode,
    crn,
    as.integer(task_block_size),
    if (is.null(seed)) 0L else bg_coerce_integerish(seed, "seed", 1L),
    !is.null(seed)
  )

  bg_recompute_rollout_rewards(
    problem,
    as.data.frame(out$results, stringsAsFactors = FALSE)
  )
}

bg_reference_equal_targets <- function(candidate_index, budget) {
  candidate_index <- as.integer(candidate_index)
  n_candidates <- length(candidate_index)
  if (n_candidates == 0L) {
    return(integer(0L))
  }

  base <- budget %/% n_candidates
  remainder <- budget %% n_candidates
  target <- rep.int(as.integer(base), n_candidates)
  if (remainder > 0L) {
    target[seq_len(remainder)] <- target[seq_len(remainder)] + 1L
  }
  names(target) <- candidate_index
  target
}

bg_reference_empty_results <- function(problem) {
  bg_stats_table_template(problem$candidate_table$candidate_index)
}

#' Build or extend a high-budget proxy reference
#'
#' `bg_reference()` constructs the package's practical proxy-reference object.
#' It is explicitly **not** exact backgammon truth. It is a high-budget,
#' model-relative reference estimate for the same rollout model defined by the
#' `bg_problem`.
#'
#' The default equal-reference mode preserves the same rollout reward semantics
#' while using parallel block simulation for speed. Focused reference mode is
#' available as a clearly labeled approximation that screens broadly, then
#' spends most of the remaining budget on plausible top actions.
#'
#' @param problem A `bg_problem` object.
#' @param budget Integer-like total proxy-reference budget.
#' @param workers_truth Number of workers used for block simulation.
#' @param truth_block_size Integer-like chunk size for parallel rollout blocks.
#' @param reference_mode Either `"equal"` or `"focused"`. `"equal"` is the
#'   conservative default. `"focused"` is an acceleration strategy for
#'   proxy-reference construction and should be treated as approximate.
#' @param extend_existing_reference Optional `bg_reference` object to extend.
#' @param dice_mode Dice mode for proxy-reference rollouts.
#' @param crn Logical scalar; if `TRUE`, use common random numbers in the
#'   reference engine.
#' @param focus_top Integer-like number of plausible top actions retained in
#'   focused mode after the pilot screen.
#' @param focus_share Numeric fraction of total budget allocated to the focused
#'   second stage.
#' @param seed Optional integer-like seed.
#'
#' @return A `bg_reference` object.
#' @export
bg_reference <- function(
    problem,
    budget = 4096L,
    workers_truth = bg_default_workers_truth(),
    truth_block_size = 128L,
    reference_mode = c("equal", "focused"),
    extend_existing_reference = NULL,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    focus_top = 4L,
    focus_share = 0.75,
    seed = NULL) {
  if (!inherits(problem, "bg_problem")) {
    stop("`problem` must inherit from class 'bg_problem'.", call. = FALSE)
  }
  if (!is.null(extend_existing_reference) && !inherits(extend_existing_reference, "bg_reference")) {
    stop("`extend_existing_reference` must inherit from class 'bg_reference'.", call. = FALSE)
  }

  budget <- bg_coerce_integerish(budget, "budget", 1L)
  workers_truth <- bg_coerce_integerish(workers_truth, "workers_truth", 1L)
  truth_block_size <- bg_coerce_integerish(truth_block_size, "truth_block_size", 1L)
  reference_mode <- match.arg(reference_mode)
  dice_mode <- match.arg(dice_mode)
  focus_top <- bg_coerce_integerish(focus_top, "focus_top", 1L)
  if (!is.numeric(focus_share) || length(focus_share) != 1L || is.na(focus_share) || focus_share <= 0 || focus_share >= 1) {
    stop("`focus_share` must be a numeric scalar in (0, 1).", call. = FALSE)
  }

  old_threads <- RcppParallel::defaultNumThreads()
  on.exit(RcppParallel::setThreadOptions(numThreads = old_threads), add = TRUE)
  RcppParallel::setThreadOptions(numThreads = workers_truth)

  previous <- if (is.null(extend_existing_reference)) {
    bg_reference_empty_results(problem)
  } else {
    bg_reference_carry_stats_table(extend_existing_reference$action_table)
  }
  previous_budget <- sum(previous$allocation_count)

  if (nrow(problem$candidate_table) == 0L) {
    return(bg_empty_reference_object(
      problem = problem,
      reference_budget = budget,
      reference_mode = reference_mode,
      workers_truth = workers_truth,
      truth_block_size = truth_block_size
    ))
  }

  if (budget <= previous_budget) {
    return(extend_existing_reference)
  }

  candidate_ids <- problem$candidate_table$candidate_index

  if (reference_mode == "equal") {
    target_counts <- bg_reference_equal_targets(candidate_ids, budget)
    current_counts <- previous$allocation_count
    names(current_counts) <- previous$candidate_index
    add_counts <- pmax(unname(target_counts[as.character(candidate_ids)]) - unname(current_counts[as.character(candidate_ids)]), 0L)

    added <- bg_call_rollout_blocks(
      problem = problem,
      candidate_index = candidate_ids,
      block_rollouts = add_counts,
      start_counts = current_counts[as.character(candidate_ids)],
      task_block_size = truth_block_size,
      dice_mode = dice_mode,
      crn = crn,
      seed = seed
    )
  } else {
    pilot_budget <- max(
      previous_budget,
      min(budget, max(length(candidate_ids) * 8L, floor(budget * (1 - focus_share))))
    )
    pilot_reference <- if (is.null(extend_existing_reference)) {
      bg_reference(
        problem = problem,
        budget = pilot_budget,
        workers_truth = workers_truth,
        truth_block_size = truth_block_size,
        reference_mode = "equal",
        extend_existing_reference = NULL,
        dice_mode = dice_mode,
        crn = crn,
        seed = bg_derive_seed(seed, "focused-pilot")
      )
    } else {
      bg_reference(
        problem = problem,
        budget = pilot_budget,
        workers_truth = workers_truth,
        truth_block_size = truth_block_size,
        reference_mode = "equal",
        extend_existing_reference = extend_existing_reference,
        dice_mode = dice_mode,
        crn = crn,
        seed = bg_derive_seed(seed, "focused-pilot")
      )
    }

    previous <- bg_reference_carry_stats_table(pilot_reference$action_table)
    remaining_budget <- budget - sum(previous$allocation_count)
    ranked <- pilot_reference$action_table[order(pilot_reference$action_table$rank), , drop = FALSE]
    focus_ids <- head(ranked$candidate_index, min(focus_top, nrow(ranked)))
    focus_counts <- rep.int(0L, length(candidate_ids))
    if (remaining_budget > 0L) {
      base <- remaining_budget %/% length(focus_ids)
      remainder <- remaining_budget %% length(focus_ids)
      focus_match <- match(focus_ids, candidate_ids)
      focus_counts[focus_match] <- base
      if (remainder > 0L) {
        focus_counts[focus_match[seq_len(remainder)]] <- focus_counts[focus_match[seq_len(remainder)]] + 1L
      }
    }
    current_counts <- previous$allocation_count
    names(current_counts) <- previous$candidate_index
    added <- bg_call_rollout_blocks(
      problem = problem,
      candidate_index = candidate_ids,
      block_rollouts = focus_counts,
      start_counts = current_counts[as.character(candidate_ids)],
      task_block_size = truth_block_size,
      dice_mode = dice_mode,
      crn = crn,
      seed = bg_derive_seed(seed, "focused-top-stage")
    )
  }

  bg_merge_reference_results(
    problem = problem,
    previous = previous,
    added = added,
    prior_alpha = problem$settings$prior_alpha,
    prior_beta = problem$settings$prior_beta,
    reference_budget = budget,
    reference_mode = reference_mode,
    workers_truth = workers_truth,
    truth_block_size = truth_block_size,
    dice_mode = dice_mode,
    crn = crn
  )
}

# -----------------------------------------------------------------------------
# Source: bg_truth.R
# -----------------------------------------------------------------------------
# Proxy-truth objects, caching, and certification helpers.
#
# This file owns the persistent truth layer that sits on top of
# `bg_reference()`. The goal is to make the package's Monte Carlo proxy-truth
# workflow explicit and auditable:
# - build or extend one truth object for one local decision problem;
# - save/load those truths cleanly from disk;
# - normalize older saved objects so cached studies remain readable; and
# - summarize whether a proxy reference looks separated enough to support
#   comparisons without overstating it as external truth.
bg_new_truth_state <- function(x) {
  x$problem <- x$problem
  x$reference <- bg_normalize_truth_reference(x$reference, "x$reference")
  x$summary <- bg_truth_state_summary_aliases(x$summary)
  structure(x, class = "bg_truth_state")
}

bg_truth_metadata_hash <- function(
    problem,
    reference_mode = c("equal", "focused"),
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE) {
  bg_truth_problem_hash(
    problem = problem,
    reference_mode = reference_mode,
    dice_mode = dice_mode,
    crn = crn
  )
}

bg_truth_object_hash <- function(x) {
  if (!inherits(x, "bg_truth_state")) {
    stop("`x` must inherit from class 'bg_truth_state'.", call. = FALSE)
  }

  ref_summary <- x$reference$summary[1L, , drop = FALSE]
  reference_mode <- if ("reference_mode" %in% names(ref_summary)) ref_summary$reference_mode[[1L]] else "equal"
  dice_mode <- if ("dice_mode" %in% names(ref_summary)) ref_summary$dice_mode[[1L]] else "iid"
  crn <- if ("crn" %in% names(ref_summary)) isTRUE(ref_summary$crn[[1L]]) else FALSE

  bg_truth_metadata_hash(
    problem = x$problem,
    reference_mode = reference_mode,
    dice_mode = dice_mode,
    crn = crn
  )
}

# Older saved truth objects used `state_id`; newer code prefers `problem_id`.
# Keep both aliases available so cached files remain readable.
bg_truth_state_summary_aliases <- function(x) {
  out <- as.data.frame(x, stringsAsFactors = FALSE)
  if ("state_id" %in% names(out) && !"problem_id" %in% names(out)) {
    out$problem_id <- out$state_id
  }
  if ("problem_id" %in% names(out) && !"state_id" %in% names(out)) {
    out$state_id <- out$problem_id
  }
  out
}

bg_normalize_loaded_truth_object <- function(x) {
  if (inherits(x, "bg_truth_state")) {
    x$summary <- bg_truth_state_summary_aliases(x$summary)
    x$reference <- bg_normalize_truth_reference(x$reference, "x$reference")
    if (is.null(x$metadata)) {
      x$metadata <- list()
    }
    truth_hash <- bg_truth_object_hash(x)
    x$metadata$truth_hash <- truth_hash
    x$metadata$problem_hash <- truth_hash
    return(bg_new_truth_state(x))
  }

  if (inherits(x, "bg_truth_battery")) {
    x$truths <- lapply(x$truths, bg_normalize_loaded_truth_object)
    x$summary <- bg_truth_state_summary_aliases(x$summary)
    return(x)
  }

  x
}

bg_truth_state_summary_row <- function(problem, reference, seed, n_cores, parallel, save_path = NULL) {
  ref_summary <- reference$summary[1L, , drop = FALSE]
  data.frame(
    state_id = problem$problem_id,
    problem_id = problem$problem_id,
    roll = bg_truth_roll_label(problem$roll),
    n_legal_moves = nrow(problem$candidate_table),
    reference_budget = ref_summary$reference_budget[[1L]],
    reference_mode = ref_summary$reference_mode[[1L]],
    reference_is_approximate = isTRUE(ref_summary$reference_is_approximate[[1L]]),
    proxy_reference_best_move_label = ref_summary$proxy_reference_best_move_label[[1L]],
    top_two_gap_estimate = ref_summary$top_two_gap_estimate[[1L]],
    top_two_gap_mc_lower_95 = ref_summary$top_two_gap_mc_lower_95[[1L]],
    mc_gap_excludes_zero = ref_summary$mc_gap_excludes_zero[[1L]],
    difficulty_label = ref_summary$difficulty_label[[1L]],
    simulation_policy = problem$settings$simulation_policy,
    reward_model = problem$settings$reward_model,
    posterior_model = problem$settings$posterior_model,
    seed = if (is.null(seed)) NA_integer_ else bg_coerce_integerish(seed, "seed", 1L),
    n_cores = n_cores,
    parallel = isTRUE(parallel),
    save_path = if (is.null(save_path)) NA_character_ else normalizePath(save_path, mustWork = FALSE),
    stringsAsFactors = FALSE
  )
}

bg_make_truth_state <- function(problem, reference, seed, n_cores, parallel, save_path = NULL) {
  reference_summary <- reference$summary[1L, , drop = FALSE]
  truth_hash <- bg_truth_metadata_hash(
    problem = problem,
    reference_mode = reference_summary$reference_mode[[1L]],
    dice_mode = if ("dice_mode" %in% names(reference_summary)) reference_summary$dice_mode[[1L]] else "iid",
    crn = if ("crn" %in% names(reference_summary)) isTRUE(reference_summary$crn[[1L]]) else FALSE
  )
  environment <- bg_truth_environment_metadata(problem)

  bg_new_truth_state(list(
    problem = problem,
    reference = reference,
    summary = bg_truth_state_summary_row(
      problem = problem,
      reference = reference,
      seed = seed,
      n_cores = n_cores,
      parallel = parallel,
      save_path = save_path
    ),
    metadata = list(
      truth_hash = truth_hash,
      problem_hash = truth_hash,
      created_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
      seed = if (is.null(seed)) NULL else bg_coerce_integerish(seed, "seed", 1L),
      n_cores = n_cores,
      parallel = isTRUE(parallel),
      package_version = bg_local_package_version(),
      environment = environment
    ),
    save_path = if (is.null(save_path)) NULL else normalizePath(save_path, mustWork = FALSE)
  ))
}

# Turn a full reference action table into the compact truth-side summaries used
# in later comparisons and teaching workflows.
bg_truth_reference_metrics <- function(reference, near_optimal_tol = 0.01) {
  ref <- bg_normalize_truth_reference(reference, "reference")
  tab <- ref$action_table[order(ref$action_table$rank), , drop = FALSE]
  if (nrow(tab) == 0L) {
    return(list(
      summary = ref$summary,
      move_table = tab
    ))
  }

  best_mean <- max(tab$reference_mean, na.rm = TRUE)
  best_lower <- max(tab$reference_mc_lower_95, na.rm = TRUE)
  tab$gap_to_best <- best_mean - tab$reference_mean
  tab$near_optimal <- tab$gap_to_best <= near_optimal_tol
  tab$mc_not_separated_from_best <- tab$reference_mc_upper_95 >= best_lower

  summary <- data.frame(
    problem_id = ref$problem$problem_id,
    reference_budget = ref$summary$reference_budget[[1L]],
    reference_mode = ref$summary$reference_mode[[1L]],
    reference_is_approximate = isTRUE(ref$summary$reference_is_approximate[[1L]]),
    n_moves = nrow(tab),
    best_move_label = ref$summary$proxy_reference_best_move_label[[1L]],
    top_two_gap_estimate = ref$summary$top_two_gap_estimate[[1L]],
    top_two_gap_mc_lower_95 = ref$summary$top_two_gap_mc_lower_95[[1L]],
    top_two_gap_mc_upper_95 = ref$summary$top_two_gap_mc_upper_95[[1L]],
    mc_gap_excludes_zero = ref$summary$mc_gap_excludes_zero[[1L]],
    difficulty_label = ref$summary$difficulty_label[[1L]],
    reward_model = ref$problem$settings$reward_model,
    posterior_model = ref$problem$settings$posterior_model,
    n_near_optimal = sum(tab$near_optimal, na.rm = TRUE),
    mc_not_separated_from_best_set_size = sum(tab$mc_not_separated_from_best, na.rm = TRUE),
    mean_reference_se = mean(tab$reference_se, na.rm = TRUE),
    max_reference_se = max(tab$reference_se, na.rm = TRUE),
    mean_unresolved_fraction = mean(tab$unresolved_fraction, na.rm = TRUE),
    stringsAsFactors = FALSE
  )

  list(summary = summary, move_table = tab)
}

bg_truth_battery_summary <- function(truths) {
  rows <- lapply(
    truths,
    function(x) {
      diag <- bg_truth_reference_metrics(x$reference)
      out <- diag$summary
      out$roll <- bg_truth_roll_label(x$problem$roll)
      out
    }
  )

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# -----------------------------------------------------------------------------
# Truth-object persistence
# -----------------------------------------------------------------------------

#' Save a truth object to disk
#'
#' @param x A `bg_truth_state` or `bg_truth_battery` object.
#' @param path Output path.
#' @param overwrite Logical scalar; if `FALSE`, existing files cause an error.
#'
#' @return The input object, invisibly.
#' @export
bg_truth_save <- function(x, path, overwrite = FALSE) {
  if (!inherits(x, "bg_truth_state") && !inherits(x, "bg_truth_battery")) {
    stop("`x` must be a `bg_truth_state` or `bg_truth_battery` object.", call. = FALSE)
  }
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("`path` must be a non-empty character scalar.", call. = FALSE)
  }
  bg_assert_scalar_flag(overwrite, "overwrite")

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path) && !isTRUE(overwrite)) {
    stop("`path` already exists. Set `overwrite = TRUE` to replace it.", call. = FALSE)
  }

  saveRDS(x, path)
  invisible(x)
}

#' Load a truth object from disk
#'
#' @param path Path created by `bg_truth_save()`.
#'
#' @return A `bg_truth_state` or `bg_truth_battery` object.
#' @export
bg_truth_load <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("`path` must be a non-empty character scalar.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("`path` does not exist.", call. = FALSE)
  }

  out <- readRDS(path)
  if (!inherits(out, "bg_truth_state") && !inherits(out, "bg_truth_battery")) {
    stop("The object stored at `path` is not a supported truth object.", call. = FALSE)
  }
  bg_normalize_loaded_truth_object(out)
}

#' Build or extend a cached proxy-truth object for one state
#'
#' `bg_truth_state()` is the research-facing wrapper around [bg_reference()]. It
#' keeps the rollout-model estimand explicit, supports persistent caching, and
#' stores metadata needed to audit large proxy-truth objects.
#'
#' @param state A `bg_board` object.
#' @param roll A `bg_roll` object.
#' @param problem Optional pre-built `bg_problem` object.
#' @param budget Integer-like proxy-truth budget.
#' @param simulation_policy Continuation policy inside rollouts when `problem`
#'   is not supplied.
#' @param heuristic_policy Heuristic continuation policy used when
#'   `simulation_policy = "heuristic"`.
#' @param max_rollout_turns Integer-like rollout horizon when `problem` is not
#'   supplied.
#' @param unresolved_value Numeric unresolved payoff when `problem` is not
#'   supplied.
#' @param prior_alpha Prior alpha used by the wrapped `bg_problem`.
#' @param prior_beta Prior beta used by the wrapped `bg_problem`.
#' @param reward_model Reward definition used when `problem` is built inside the
#'   function. The default is the explicit scalar-payoff path; coherent
#'   alternatives include `"categorical_outcome"` and `"win_loss"`.
#' @param posterior_model Posterior family used when `problem` is built inside
#'   the function. The default is the explicit pseudo-Beta scalar baseline; use
#'   a compatible alternative when changing `reward_model`.
#' @param n_cores Integer-like worker count used by the proxy-reference engine.
#' @param parallel Logical scalar; if `FALSE`, force `n_cores = 1`.
#' @param truth_block_size Integer-like block size passed to [bg_reference()].
#' @param reference_mode Reference allocation mode passed to [bg_reference()].
#'   `"equal"` remains the main proxy-reference mode; `"focused"` is an
#'   approximation used to accelerate large proxy-reference builds.
#' @param cache Logical scalar; if `TRUE`, reuse an existing saved truth object
#'   when possible.
#' @param cache_dir Optional cache directory used when `save_path` is omitted.
#' @param save_path Optional explicit `.rds` path.
#' @param overwrite Logical scalar; if `TRUE`, ignore any existing saved object.
#' @param dice_mode Dice mode passed to [bg_reference()].
#' @param crn Logical scalar; if `TRUE`, use common random numbers.
#' @param seed Optional integer seed.
#' @param problem_id Optional identifier used when `problem` is built inside the
#'   function.
#'
#' @return A `bg_truth_state` object.
#' @export
bg_truth_state <- function(
    state = NULL,
    roll = NULL,
    problem = NULL,
    budget = 8192L,
    simulation_policy = c("random", "heuristic", "aggressive", "defensive", "ts_local"),
    heuristic_policy = c("aggressive", "defensive"),
    max_rollout_turns = 220L,
    unresolved_value = 0.5,
    prior_alpha = 1,
    prior_beta = 1,
    reward_model = c("scalar_payoff"),
    posterior_model = c("beta_pseudo"),
    n_cores = bg_default_workers_truth(),
    parallel = TRUE,
    truth_block_size = 128L,
    reference_mode = c("equal", "focused"),
    cache = FALSE,
    cache_dir = NULL,
    save_path = NULL,
    overwrite = FALSE,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    seed = NULL,
    problem_id = NULL) {
  budget <- bg_coerce_integerish(budget, "budget", 1L)
  n_cores <- bg_coerce_integerish(n_cores, "n_cores", 1L)
  bg_assert_scalar_flag(parallel, "parallel")
  bg_assert_scalar_flag(cache, "cache")
  bg_assert_scalar_flag(overwrite, "overwrite")
  truth_block_size <- bg_coerce_integerish(truth_block_size, "truth_block_size", 1L)
  reference_mode <- match.arg(reference_mode)
  dice_mode <- match.arg(dice_mode)

  if (is.null(problem)) {
    if (is.null(state) || is.null(roll)) {
      stop("Supply either `problem` or both `state` and `roll`.", call. = FALSE)
    }
    problem <- bg_problem(
      state = state,
      roll = roll,
      simulation_policy = simulation_policy,
      heuristic_policy = heuristic_policy,
      max_rollout_turns = max_rollout_turns,
      unresolved_value = unresolved_value,
      prior_alpha = prior_alpha,
      prior_beta = prior_beta,
      reward_model = reward_model,
      posterior_model = posterior_model,
      problem_id = problem_id
    )
  }

  if (!inherits(problem, "bg_problem")) {
    stop("`problem` must inherit from class 'bg_problem'.", call. = FALSE)
  }

  workers_truth <- if (isTRUE(parallel)) n_cores else 1L
  target_path <- save_path
  if (is.null(target_path) && isTRUE(cache)) {
    target_path <- bg_truth_storage_path(
      problem = problem,
      scope = "state",
      cache_dir = cache_dir,
      reference_mode = reference_mode,
      dice_mode = dice_mode,
      crn = crn
    )
  }

  # Reuse an existing saved truth object when the stored problem hash still
  # matches the current problem definition.
  cached_truth <- NULL
  if (!is.null(target_path) && file.exists(target_path) && !isTRUE(overwrite)) {
    maybe_cached <- bg_truth_load(target_path)
    cached_hash <- if (!is.null(maybe_cached$metadata$truth_hash)) {
      maybe_cached$metadata$truth_hash
    } else {
      maybe_cached$metadata$problem_hash
    }
    if (inherits(maybe_cached, "bg_truth_state") &&
        identical(
          cached_hash,
          bg_truth_metadata_hash(
            problem = problem,
            reference_mode = reference_mode,
            dice_mode = dice_mode,
            crn = crn
          )
        )) {
      cached_truth <- maybe_cached
      cached_budget <- cached_truth$reference$summary$reference_budget[[1L]]
      if (cached_budget >= budget) {
        return(cached_truth)
      }
    }
  }

  reference <- bg_reference(
    problem = problem,
    budget = budget,
    workers_truth = workers_truth,
    truth_block_size = truth_block_size,
    reference_mode = reference_mode,
    extend_existing_reference = if (is.null(cached_truth)) NULL else cached_truth$reference,
    dice_mode = dice_mode,
    crn = crn,
    seed = seed
  )

  out <- bg_make_truth_state(
    problem = problem,
    reference = reference,
    seed = seed,
    n_cores = workers_truth,
    parallel = parallel,
    save_path = target_path
  )

  if (!is.null(target_path)) {
    bg_truth_save(out, target_path, overwrite = TRUE)
  }

  out
}

#' Build one reusable master truth object from scored outcomes
#'
#' `bg_master_truth_state()` is the simplest "simulate once, reuse across
#' models" front door in the package. It always builds truth under the full
#' scored-outcome representation:
#'
#' - `reward_model = "categorical_outcome"`
#' - `posterior_model = "dirichlet_multinomial"`
#'
#' The resulting truth object stores the full scored outcome counts needed to
#' derive coherent `scalar_payoff`, `win_loss`, and `categorical_outcome` views
#' later via [bg_truth_project()] without simulating again.
#'
#' @inheritParams bg_truth_state
#'
#' @return A `bg_truth_state` object built under the master scored-outcome
#'   representation.
#' @export
bg_master_truth_state <- function(
    state = NULL,
    roll = NULL,
    problem = NULL,
    budget = 8192L,
    simulation_policy = c("random", "heuristic", "aggressive", "defensive", "ts_local"),
    heuristic_policy = c("aggressive", "defensive"),
    max_rollout_turns = 220L,
    unresolved_value = 0.5,
    prior_alpha = 1,
    prior_beta = 1,
    n_cores = bg_default_workers_truth(),
    parallel = TRUE,
    truth_block_size = 128L,
    reference_mode = c("equal", "focused"),
    cache = FALSE,
    cache_dir = NULL,
    save_path = NULL,
    overwrite = FALSE,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    seed = NULL,
    problem_id = NULL) {
  if (!is.null(problem)) {
    problem <- bg_problem_clone_models(
      problem = problem,
      reward_model = "categorical_outcome",
      posterior_model = "dirichlet_multinomial",
      unresolved_value = problem$settings$unresolved_value,
      problem_id = problem$problem_id,
      cache = FALSE
    )
  }

  bg_truth_state(
    state = state,
    roll = roll,
    problem = problem,
    budget = budget,
    simulation_policy = simulation_policy,
    heuristic_policy = heuristic_policy,
    max_rollout_turns = max_rollout_turns,
    unresolved_value = unresolved_value,
    prior_alpha = prior_alpha,
    prior_beta = prior_beta,
    reward_model = "categorical_outcome",
    posterior_model = "dirichlet_multinomial",
    n_cores = n_cores,
    parallel = parallel,
    truth_block_size = truth_block_size,
    reference_mode = reference_mode,
    cache = cache,
    cache_dir = cache_dir,
    save_path = save_path,
    overwrite = overwrite,
    dice_mode = dice_mode,
    crn = crn,
    seed = seed,
    problem_id = problem_id
  )
}

#' Build a battery of cached proxy truths
#'
#' @param problems A `bg_problem` object or list of them.
#' @param budget Integer-like proxy-truth budget per state.
#' @param n_cores Integer-like worker count used inside each truth build.
#' @param parallel Logical scalar; if `FALSE`, force one worker.
#' @param truth_block_size Integer-like block size passed to [bg_reference()].
#' @param reference_mode Reference allocation mode passed to [bg_reference()].
#'   `"equal"` remains the main proxy-reference mode; `"focused"` is an
#'   approximation used to accelerate large proxy-reference builds.
#' @param cache Logical scalar; if `TRUE`, save or reuse per-state truth files.
#' @param cache_dir Optional cache directory used for per-state truth files.
#' @param save_path Optional path for the whole battery object.
#' @param overwrite Logical scalar controlling whether existing saves may be
#'   replaced.
#' @param dice_mode Dice mode passed to [bg_reference()].
#' @param crn Logical scalar indicating common-random-number use.
#' @param seed Optional integer seed used to derive per-state seeds.
#' @param verbose Logical scalar; if `TRUE`, display a progress bar.
#'
#' @return A `bg_truth_battery` object.
#' @export
bg_truth_battery <- function(
    problems,
    budget = 8192L,
    n_cores = bg_default_workers_truth(),
    parallel = TRUE,
    truth_block_size = 128L,
    reference_mode = c("equal", "focused"),
    cache = FALSE,
    cache_dir = NULL,
    save_path = NULL,
    overwrite = FALSE,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    seed = NULL,
    verbose = interactive()) {
  if (inherits(problems, "bg_problem")) {
    problems <- list(problems)
  }
  if (!is.list(problems) || length(problems) < 1L || !all(vapply(problems, inherits, logical(1L), what = "bg_problem"))) {
    stop("`problems` must be a `bg_problem` object or a non-empty list of them.", call. = FALSE)
  }

  bg_assert_scalar_flag(verbose, "verbose")
  reference_mode <- match.arg(reference_mode)
  dice_mode <- match.arg(dice_mode)

  pb <- if (isTRUE(verbose)) {
    utils::txtProgressBar(min = 0, max = length(problems), style = 3)
  } else {
    NULL
  }
  on.exit(if (!is.null(pb)) close(pb), add = TRUE)

  truths <- vector("list", length(problems))
  names(truths) <- vapply(problems, function(x) x$problem_id, character(1L))

  # Batteries are just repeated state-level truth builds plus one combined
  # summary table.
  for (i in seq_along(problems)) {
    problem <- problems[[i]]
    state_path <- NULL
    if (isTRUE(cache)) {
      state_path <- bg_truth_storage_path(
        problem = problem,
        scope = "battery",
        cache_dir = cache_dir,
        reference_mode = reference_mode,
        dice_mode = dice_mode,
        crn = crn
      )
    }
    truths[[i]] <- bg_truth_state(
      problem = problem,
      budget = budget,
      n_cores = n_cores,
      parallel = parallel,
      truth_block_size = truth_block_size,
      reference_mode = reference_mode,
      cache = cache,
      cache_dir = cache_dir,
      save_path = state_path,
      overwrite = overwrite,
      dice_mode = dice_mode,
      crn = crn,
      seed = bg_derive_seed(seed, "truth-battery", problem$problem_id)
    )
    if (!is.null(pb)) {
      utils::setTxtProgressBar(pb, i)
    }
  }

  out <- structure(
    list(
      truths = truths,
      summary = bg_truth_battery_summary(truths),
      settings = list(
        budget = budget,
        n_cores = if (isTRUE(parallel)) n_cores else 1L,
        parallel = isTRUE(parallel),
        truth_block_size = truth_block_size,
        reference_mode = reference_mode,
        cache = isTRUE(cache),
        cache_dir = cache_dir,
        dice_mode = dice_mode,
        crn = isTRUE(crn),
        seed = seed
      )
    ),
    class = "bg_truth_battery"
  )

  if (!is.null(save_path)) {
    bg_truth_save(out, save_path, overwrite = overwrite)
  }

  out
}

#' Build and cache proxy truths for opening-roll problems
#'
#' @param rolls Optional list of `bg_roll` objects. By default all 21 unordered
#'   opening rolls are used when `include_doubles = TRUE`.
#' @param include_doubles Logical scalar; if `TRUE`, include the six opening
#'   doubles along with the 15 non-double opening rolls.
#' @param budget Integer-like proxy-truth budget per opening roll.
#' @param simulation_policy Continuation policy used in the rollout model.
#' @param reward_model Reward definition used when building opening problems.
#'   The default is the explicit scalar-payoff path; coherent alternatives
#'   include `"categorical_outcome"` and `"win_loss"`.
#' @param posterior_model Posterior family used when building opening
#'   problems. The default is the explicit pseudo-Beta scalar baseline; use a
#'   compatible alternative when changing `reward_model`.
#' @param n_cores Integer-like worker count used inside each truth build.
#' @param parallel Logical scalar; if `FALSE`, force one worker.
#' @param truth_block_size Integer-like block size passed to [bg_reference()].
#' @param reference_mode Reference allocation mode passed to [bg_reference()].
#'   `"equal"` remains the main proxy-reference mode; `"focused"` is an
#'   approximation used to accelerate large proxy-reference builds.
#' @param cache Logical scalar; if `TRUE`, save or reuse per-opening truth
#'   files.
#' @param cache_dir Optional cache directory.
#' @param save_path Optional path for the whole battery object.
#' @param overwrite Logical scalar controlling whether existing saves may be
#'   replaced.
#' @param dice_mode Dice mode passed to [bg_reference()].
#' @param crn Logical scalar indicating common-random-number use.
#' @param seed Optional integer seed used to derive per-opening seeds.
#' @param verbose Logical scalar; if `TRUE`, display a progress bar.
#'
#' @return A `bg_truth_battery` object.
#' @export
bg_truth_opening <- function(
    rolls = NULL,
    include_doubles = TRUE,
    budget = 8192L,
    simulation_policy = c("random", "heuristic", "aggressive", "defensive", "ts_local"),
    reward_model = c("scalar_payoff"),
    posterior_model = c("beta_pseudo"),
    n_cores = bg_default_workers_truth(),
    parallel = TRUE,
    truth_block_size = 128L,
    reference_mode = c("equal", "focused"),
    cache = FALSE,
    cache_dir = NULL,
    save_path = NULL,
    overwrite = FALSE,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    seed = NULL,
    verbose = interactive()) {
  simulation_policy <- bg_match_simulation_policy_public(simulation_policy)
  bg_assert_scalar_flag(include_doubles, "include_doubles")

  opening_entries <- if (is.null(rolls)) {
    # By default the opening battery is the 21 unordered opening rolls:
    # 15 non-doubles plus 6 doubles.
    bg_opening_roll_grid(include_doubles = include_doubles)
  } else {
    lapply(
      rolls,
      function(roll) {
        roll <- bg_as_roll(roll)
        list(
          roll = roll,
          roll_label = bg_truth_roll_label(roll),
          die1 = roll$dice[[1L]],
          die2 = roll$dice[[2L]],
          is_double = isTRUE(roll$is_double),
          roll_group = if (isTRUE(roll$is_double)) "double" else "non_double"
        )
      }
    )
  }

  problems <- lapply(
    opening_entries,
    function(entry) {
      bg_problem(
        state = bg_initial_board(),
        roll = entry$roll,
        simulation_policy = simulation_policy,
        reward_model = reward_model,
        posterior_model = posterior_model,
        problem_id = paste0("opening_", entry$roll_label)
      )
    }
  )

  out <- bg_truth_battery(
    problems = problems,
    budget = budget,
    n_cores = n_cores,
    parallel = parallel,
    truth_block_size = truth_block_size,
    reference_mode = reference_mode,
    cache = cache,
    cache_dir = if (is.null(cache_dir)) bg_default_truth_cache_dir("opening") else cache_dir,
    save_path = NULL,
    overwrite = overwrite,
    dice_mode = dice_mode,
    crn = crn,
    seed = seed,
    verbose = verbose
  )

  lookup <- do.call(
    rbind,
    lapply(
      opening_entries,
      function(entry) {
        data.frame(
          problem_id = paste0("opening_", entry$roll_label),
          opening_roll = entry$roll_label,
          die1 = entry$die1,
          die2 = entry$die2,
          is_double = entry$is_double,
          roll_group = entry$roll_group,
          stringsAsFactors = FALSE
        )
      }
    )
  )
  lookup <- lookup[order(lookup$die1, lookup$die2), , drop = FALSE]

  idx <- match(out$summary$problem_id, lookup$problem_id)
  out$summary <- cbind(
    out$summary,
    lookup[idx, c("opening_roll", "die1", "die2", "is_double", "roll_group"), drop = FALSE],
    stringsAsFactors = FALSE
  )
  out$summary <- out$summary[order(out$summary$die1, out$summary$die2), , drop = FALSE]
  out$settings$include_doubles <- include_doubles
  if (!is.null(save_path)) {
    bg_truth_save(out, save_path, overwrite = overwrite)
  }
  out
}

#' Summarize a truth object for display and diagnostics
#'
#' @param x A `bg_truth_state`, `bg_truth_battery`, or `bg_reference` object.
#' @param top_n Integer-like number of actions to show for single-state truth
#'   diagnostics.
#' @param near_optimal_tol Numeric tolerance used to count near-optimal moves.
#'
#' @return A data frame for battery objects, or a list with `summary` and
#'   `move_table` for single-state objects.
#' @export
bg_truth_diagnostics <- function(x, top_n = 8L, near_optimal_tol = 0.01) {
  top_n <- bg_coerce_integerish(top_n, "top_n", 1L)
  if (!is.numeric(near_optimal_tol) || length(near_optimal_tol) != 1L || is.na(near_optimal_tol) || near_optimal_tol < 0) {
    stop("`near_optimal_tol` must be a nonnegative numeric scalar.", call. = FALSE)
  }

  if (inherits(x, "bg_truth_battery")) {
    # Battery diagnostics stay compact: one ordered row per problem.
    return(x$summary[order(x$summary$top_two_gap_estimate), , drop = FALSE])
  }

  # Single-state diagnostics keep both the summary row and a trimmed move table.
  ref <- bg_normalize_truth_reference(x, "x")
  out <- bg_truth_reference_metrics(ref, near_optimal_tol = near_optimal_tol)
  out$move_table <- bg_round_display_table(
    utils::head(
      out$move_table[, c(
        "rank",
        "move_label",
        "reference_mean",
        "reference_mc_lower_95",
        "reference_mc_upper_95",
        "reference_se",
        "gap_to_best",
        "near_optimal",
        "mc_not_separated_from_best",
        "allocation_count",
        "unresolved_fraction"
      ), drop = FALSE],
      top_n
    )
  )
  out
}

# -----------------------------------------------------------------------------
# Proxy-truth certification
# -----------------------------------------------------------------------------

bg_truth_certification_label <- function(
    gap_lower,
    gap_estimate,
    n_near_optimal,
    mc_not_separated_from_best_set_size) {
  if (!is.finite(gap_estimate)) {
    return("uncertain")
  }

  if (isTRUE(is.finite(gap_lower) && gap_lower > 0) &&
      isTRUE(n_near_optimal <= 1L) &&
      isTRUE(mc_not_separated_from_best_set_size <= 1L)) {
    return("clear")
  }

  if (gap_estimate <= 0.01 || isTRUE(n_near_optimal >= 3L)) {
    return("hard")
  }

  if (!isTRUE(is.finite(gap_lower) && gap_lower > 0) ||
      isTRUE(mc_not_separated_from_best_set_size >= 2L)) {
    return("ambiguous")
  }

  "uncertain"
}

#' Certify how separated a proxy truth appears to be
#'
#' `bg_truth_certify()` screens a proxy truth for whether the top move looks
#' separated enough to support method comparisons under the package's rollout
#' model. It does not claim exact backgammon truth.
#'
#' @param x A `bg_reference`, `bg_truth_state`, or `bg_truth_battery` object.
#' @param near_optimal_tol Numeric tolerance for counting near-optimal moves.
#'
#' @return A data frame with top-gap and separation diagnostics.
#' @export
bg_truth_certify <- function(x, near_optimal_tol = 0.01) {
  if (inherits(x, "bg_truth_battery")) {
    rows <- lapply(x$truths, bg_truth_certify, near_optimal_tol = near_optimal_tol)
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    return(out)
  }

  reference <- bg_normalize_truth_reference(x, "x")
  diag <- bg_truth_reference_metrics(reference, near_optimal_tol = near_optimal_tol)
  summary <- diag$summary

  summary$certification <- mapply(
    FUN = bg_truth_certification_label,
    gap_lower = summary$top_two_gap_mc_lower_95,
    gap_estimate = summary$top_two_gap_estimate,
    n_near_optimal = summary$n_near_optimal,
    mc_not_separated_from_best_set_size = summary$mc_not_separated_from_best_set_size,
    USE.NAMES = FALSE
  )
  summary$repeated_seed_stability <- NA_real_

  summary[, c(
    "problem_id",
    "reference_budget",
    "reference_mode",
    "reference_is_approximate",
    "best_move_label",
    "top_two_gap_estimate",
    "top_two_gap_mc_lower_95",
    "top_two_gap_mc_upper_95",
    "mc_gap_excludes_zero",
    "n_near_optimal",
    "mc_not_separated_from_best_set_size",
    "difficulty_label",
    "certification",
    "repeated_seed_stability"
  ), drop = FALSE]
}

# -----------------------------------------------------------------------------
# Source: bg_truth_stability.R
# -----------------------------------------------------------------------------
# Proxy-reference stability workflows.
#
# This file owns the repeated-build layer that sits above `bg_reference()` and
# `bg_truth_certify()`. Its job is to separate two ideas that are easy to
# confuse in rollout studies:
# - the proxy-reference process may still be unstable because Monte Carlo error
#   is large at the chosen budget; or
# - the reference may have stabilized, but the decision is still genuinely hard
#   because several moves remain near-tied under the rollout model.

bg_truth_stability_problem_list <- function(x) {
  if (inherits(x, "bg_problem")) {
    return(list(x))
  }
  if (inherits(x, "bg_truth_state")) {
    return(list(x$problem))
  }
  if (inherits(x, "bg_truth_battery")) {
    return(lapply(x$truths, `[[`, "problem"))
  }
  if (is.list(x) && length(x) > 0L) {
    if (all(vapply(x, inherits, logical(1L), what = "bg_problem"))) {
      return(x)
    }
    if (all(vapply(x, inherits, logical(1L), what = "bg_truth_state"))) {
      return(lapply(x, `[[`, "problem"))
    }
  }

  stop(
    "`x` must be a `bg_problem`, `bg_truth_state`, `bg_truth_battery`, or a non-empty list of problems/truth states.",
    call. = FALSE
  )
}

bg_truth_stability_default_budgets <- function(x) {
  reference_budgets <- numeric(0L)
  if (inherits(x, "bg_truth_state")) {
    reference_budgets <- x$reference$summary$reference_budget
  } else if (inherits(x, "bg_truth_battery")) {
    reference_budgets <- x$summary$reference_budget
  } else if (is.list(x) && length(x) > 0L && all(vapply(x, inherits, logical(1L), what = "bg_truth_state"))) {
    reference_budgets <- vapply(x, function(obj) obj$reference$summary$reference_budget[[1L]], numeric(1L))
  }

  if (length(reference_budgets) == 0L) {
    return(c(256L, 512L, 1024L))
  }

  out <- unique(unlist(lapply(
    reference_budgets,
    function(budget) {
      as.integer(unique(pmax(32L, round(c(0.25, 0.5, 1) * budget))))
    }
  )))
  sort(out)
}

bg_truth_stability_row <- function(problem, reference, budget, seed) {
  cert <- bg_truth_certify(reference)
  ref_tab <- reference$action_table[order(reference$action_table$rank), , drop = FALSE]
  top2_labels <- ref_tab$move_label[seq_len(min(2L, nrow(ref_tab)))]
  mean_unresolved <- if ("unresolved_fraction" %in% names(ref_tab)) {
    mean(ref_tab$unresolved_fraction, na.rm = TRUE)
  } else {
    NA_real_
  }

  data.frame(
    problem_id = problem$problem_id,
    roll = bg_truth_roll_label(problem$roll),
    budget = budget,
    seed = seed,
    reward_model = problem$settings$reward_model_canonical,
    posterior_model = problem$settings$posterior_model_canonical,
    reference_mode = reference$summary$reference_mode[[1L]],
    reference_is_approximate = isTRUE(reference$summary$reference_is_approximate[[1L]]),
    dice_mode = reference$summary$dice_mode[[1L]],
    crn = isTRUE(reference$summary$crn[[1L]]),
    top1_move_label = cert$best_move_label[[1L]],
    top2_signature = paste(top2_labels, collapse = " || "),
    top_two_gap_estimate = cert$top_two_gap_estimate[[1L]],
    top_two_gap_mc_lower_95 = cert$top_two_gap_mc_lower_95[[1L]],
    top_two_gap_mc_upper_95 = cert$top_two_gap_mc_upper_95[[1L]],
    mc_gap_excludes_zero = isTRUE(cert$mc_gap_excludes_zero[[1L]]),
    difficulty_label = cert$difficulty_label[[1L]],
    certification = cert$certification[[1L]],
    n_near_optimal = cert$n_near_optimal[[1L]],
    mc_not_separated_from_best_set_size = cert$mc_not_separated_from_best_set_size[[1L]],
    mean_unresolved_fraction = mean_unresolved,
    warning_count = length(reference$warnings),
    has_warning = length(reference$warnings) > 0L,
    stringsAsFactors = FALSE
  )
}

bg_truth_modal_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    return(NA)
  }
  tab <- sort(table(x), decreasing = TRUE)
  names(tab)[[1L]]
}

bg_truth_modal_share <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    return(NA_real_)
  }
  max(table(x)) / length(x)
}

bg_truth_stability_summary <- function(results, stable_share = 0.8) {
  split_key <- interaction(results$problem_id, results$budget, drop = TRUE, lex.order = TRUE)
  rows <- lapply(
    split(results, split_key),
    function(df) {
      df <- df[order(df$seed), , drop = FALSE]
      top1_modal <- bg_truth_modal_value(df$top1_move_label)
      top2_modal <- bg_truth_modal_value(df$top2_signature)
      difficulty_modal <- bg_truth_modal_value(df$difficulty_label)
      certification_modal <- bg_truth_modal_value(df$certification)
      top1_share <- bg_truth_modal_share(df$top1_move_label)
      top2_share <- bg_truth_modal_share(df$top2_signature)
      difficulty_share <- bg_truth_modal_share(df$difficulty_label)
      certification_share <- bg_truth_modal_share(df$certification)
      gap_clear_rate <- mean(df$mc_gap_excludes_zero, na.rm = TRUE)

      data.frame(
        problem_id = df$problem_id[[1L]],
        roll = df$roll[[1L]],
        budget = df$budget[[1L]],
        reward_model = df$reward_model[[1L]],
        posterior_model = df$posterior_model[[1L]],
        reference_mode = df$reference_mode[[1L]],
        crn = df$crn[[1L]],
        n_builds = nrow(df),
        n_distinct_top1 = length(unique(df$top1_move_label[!is.na(df$top1_move_label)])),
        n_distinct_top2 = length(unique(df$top2_signature[!is.na(df$top2_signature)])),
        modal_top1_move_label = top1_modal,
        modal_top1_share = top1_share,
        modal_top2_signature = top2_modal,
        modal_top2_share = top2_share,
        top_two_gap_mean = mean(df$top_two_gap_estimate, na.rm = TRUE),
        top_two_gap_sd = bg_eval_safe_range_stat(df$top_two_gap_estimate, fn = "sd"),
        gap_clear_rate = gap_clear_rate,
        modal_difficulty_label = difficulty_modal,
        modal_difficulty_share = difficulty_share,
        modal_certification = certification_modal,
        modal_certification_share = certification_share,
        mean_n_near_optimal = mean(df$n_near_optimal, na.rm = TRUE),
        mean_mc_not_separated_from_best_set_size = mean(df$mc_not_separated_from_best_set_size, na.rm = TRUE),
        mean_unresolved_fraction = mean(df$mean_unresolved_fraction, na.rm = TRUE),
        any_warning_rate = mean(df$has_warning, na.rm = TRUE),
        reference_stability_label = if (isTRUE(
          top1_share >= stable_share &&
            top2_share >= stable_share &&
            difficulty_share >= stable_share
        )) {
          "reference_stable"
        } else {
          "reference_unstable"
        },
        decision_screen_label = if (identical(certification_modal, "clear") && isTRUE(gap_clear_rate >= 0.5)) {
          "decision_clear"
        } else {
          "decision_ambiguous"
        },
        stringsAsFactors = FALSE
      )
    }
  )

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

bg_truth_stability_problem_summary <- function(summary_df) {
  split_key <- interaction(summary_df$problem_id, drop = TRUE)
  rows <- lapply(
    split(summary_df, split_key),
    function(df) {
      df <- df[order(df$budget), , drop = FALSE]
      stable_idx <- which(df$reference_stability_label == "reference_stable")
      clear_idx <- which(df$decision_screen_label == "decision_clear")

      data.frame(
        problem_id = df$problem_id[[1L]],
        roll = df$roll[[1L]],
        reward_model = df$reward_model[[1L]],
        posterior_model = df$posterior_model[[1L]],
        n_budgets = nrow(df),
        first_reference_stable_budget = if (length(stable_idx) < 1L) NA_integer_ else df$budget[[stable_idx[[1L]]]],
        first_decision_clear_budget = if (length(clear_idx) < 1L) NA_integer_ else df$budget[[clear_idx[[1L]]]],
        final_reference_stability_label = df$reference_stability_label[[nrow(df)]],
        final_decision_screen_label = df$decision_screen_label[[nrow(df)]],
        final_modal_top1_move_label = df$modal_top1_move_label[[nrow(df)]],
        final_top_two_gap_mean = df$top_two_gap_mean[[nrow(df)]],
        stringsAsFactors = FALSE
      )
    }
  )

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Rebuild proxy references to screen reference stability
#'
#' `bg_truth_stability()` repeatedly rebuilds proxy references over a budget
#' ladder and seed replicates. It is designed to separate two ideas that should
#' not be conflated:
#'
#' - whether the proxy-reference process itself stabilized; and
#' - whether the local decision remains substantively ambiguous even after the
#'   proxy reference has stabilized.
#'
#' The output therefore reports both a `reference_stability_label` and a
#' `decision_screen_label`. These are screening summaries, not proof.
#'
#' @param x A `bg_problem`, `bg_truth_state`, `bg_truth_battery`, or a non-empty
#'   list of problems/truth states.
#' @param budgets Integer-like budget ladder. When omitted, a ladder is derived
#'   from the supplied truth object if possible.
#' @param seeds Integer-like replicate seed vector.
#' @param n_cores Integer-like worker count used inside each reference build.
#' @param parallel Logical scalar; if `TRUE`, parallelize over problem-seed
#'   tasks.
#' @param truth_block_size Integer-like block size passed to [bg_reference()].
#' @param reference_mode Reference allocation mode. Use `"equal"` for headline
#'   stability studies.
#' @param dice_mode Dice mode passed to [bg_reference()].
#' @param crn Logical scalar indicating common-random-number use.
#' @param progress Logical scalar; if `TRUE`, display task progress.
#' @param stable_share Modal-share threshold used for the
#'   `reference_stability_label`.
#' @param save_path Optional `.rds` path for the returned object.
#' @param overwrite Logical scalar controlling replacement of an existing save.
#' @param seed Optional master seed used to derive per-problem/per-budget seeds.
#'
#' @return A `bg_truth_stability` object with raw `results`, per-budget
#'   `summary`, and a compact `problem_summary`.
#' @export
bg_truth_stability <- function(
    x,
    budgets = NULL,
    seeds = 1:10,
    n_cores = bg_default_workers_truth(),
    parallel = FALSE,
    truth_block_size = 128L,
    reference_mode = c("equal", "focused"),
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    progress = interactive(),
    stable_share = 0.8,
    save_path = NULL,
    overwrite = FALSE,
    seed = NULL) {
  cached <- bg_maybe_load_saved_study(save_path, overwrite = overwrite)
  if (!is.null(cached)) {
    return(cached)
  }

  problems <- bg_truth_stability_problem_list(x)
  budgets <- if (is.null(budgets)) bg_truth_stability_default_budgets(x) else bg_normalize_study_budgets(budgets)
  seeds <- sort(unique(bg_coerce_integerish(seeds, "seeds", length(seeds))))
  n_cores <- bg_coerce_integerish(n_cores, "n_cores", 1L)
  truth_block_size <- bg_coerce_integerish(truth_block_size, "truth_block_size", 1L)
  reference_mode <- match.arg(reference_mode)
  dice_mode <- match.arg(dice_mode)
  bg_assert_scalar_flag(parallel, "parallel")
  bg_assert_scalar_flag(progress, "progress")
  bg_assert_scalar_flag(crn, "crn")
  bg_assert_scalar_flag(overwrite, "overwrite")
  if (!is.numeric(stable_share) || length(stable_share) != 1L || is.na(stable_share) || stable_share <= 0 || stable_share > 1) {
    stop("`stable_share` must be a numeric scalar in (0, 1].", call. = FALSE)
  }
  workers_truth <- if (isTRUE(parallel)) 1L else n_cores

  task_grid <- expand.grid(
    problem_idx = seq_along(problems),
    seed = seeds,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  tasks <- split(task_grid, seq_len(nrow(task_grid)))

  task_rows <- bg_task_apply(
    tasks = tasks,
    n_cores = n_cores,
    parallel = parallel,
    progress = progress,
    progress_label = "truth-stability builds",
    worker = function(task) {
      problem <- problems[[task$problem_idx[[1L]]]]
      seed_i <- task$seed[[1L]]
      ref_i <- NULL
      rows <- vector("list", length(budgets))

      for (budget_idx in seq_along(budgets)) {
        budget_i <- budgets[[budget_idx]]
        ref_i <- bg_reference(
          problem = problem,
          budget = budget_i,
          workers_truth = workers_truth,
          truth_block_size = truth_block_size,
          reference_mode = reference_mode,
          extend_existing_reference = ref_i,
          dice_mode = dice_mode,
          crn = crn,
          seed = bg_derive_seed(seed, "truth-stability", problem$problem_id, seed_i, budget_i)
        )
        rows[[budget_idx]] <- bg_truth_stability_row(
          problem = problem,
          reference = ref_i,
          budget = budget_i,
          seed = seed_i
        )
      }

      do.call(rbind, rows)
    }
  )

  results <- do.call(rbind, task_rows)
  rownames(results) <- NULL
  summary <- bg_truth_stability_summary(results, stable_share = stable_share)
  problem_summary <- bg_truth_stability_problem_summary(summary)

  out <- structure(
    list(
      problems = problems,
      results = results,
      summary = summary,
      problem_summary = problem_summary,
      settings = list(
        budgets = budgets,
        seeds = seeds,
        n_cores = n_cores,
        parallel = isTRUE(parallel),
        workers_truth = workers_truth,
        truth_block_size = truth_block_size,
        reference_mode = reference_mode,
        dice_mode = dice_mode,
        crn = isTRUE(crn),
        stable_share = stable_share,
        seed = seed
      )
    ),
    class = "bg_truth_stability"
  )

  if (!is.null(save_path)) {
    bg_study_save(out, save_path, overwrite = overwrite)
  }
  out
}

# -----------------------------------------------------------------------------
# Source: bg_openings.R
# -----------------------------------------------------------------------------
# Opening-roll research workflows.
#
# This file owns the package's cleanest experimental laboratory: the 21
# unordered opening rolls from the initial board. It provides:
# - the roll grid itself;
# - one-roll and full-battery proxy-truth helpers;
# - cache loading/index helpers for previously built truths; and
# - the opening-study comparison workflow used in the research layer.

bg_as_opening_roll <- function(roll) {
  if (is.character(roll) && length(roll) == 1L && grepl("^[1-6][-:][1-6]$", roll)) {
    parts <- strsplit(roll, "[-:]")[[1L]]
    return(bg_roll(as.integer(parts[[1L]]), as.integer(parts[[2L]])))
  }

  bg_as_roll(roll)
}

bg_opening_roll_grid <- function(include_doubles = TRUE) {
  bg_assert_scalar_flag(include_doubles, "include_doubles")

  rows <- vector("list", 0L)
  for (die1 in 1:6) {
    for (die2 in die1:6) {
      if (!isTRUE(include_doubles) && die1 == die2) {
        next
      }

      rows[[length(rows) + 1L]] <- data.frame(
        opening_roll = paste0(die1, "-", die2),
        die1 = die1,
        die2 = die2,
        is_double = die1 == die2,
        roll_group = if (die1 == die2) "double" else "non_double",
        stringsAsFactors = FALSE
      )
    }
  }

  out <- do.call(rbind, rows)
  out$roll <- lapply(seq_len(nrow(out)), function(i) bg_roll(out$die1[[i]], out$die2[[i]]))
  out
}

#' List the 21 unordered opening rolls
#'
#' @param include_doubles Logical scalar; if `FALSE`, return only the 15
#'   non-double opening rolls.
#'
#' @return A data frame with one row per opening roll and a `roll` list-column.
#' @export
bg_opening_rolls <- function(include_doubles = TRUE) {
  bg_opening_roll_grid(include_doubles = include_doubles)
}

#' Build one opening-roll decision problem
#'
#' @param roll An opening roll, supplied as a `bg_roll` object or a string such
#'   as `"1-6"`.
#' @param problem_id Optional problem identifier.
#' @param ... Passed to [bg_problem()].
#'
#' @return A `bg_problem` object for the initial board and the chosen roll.
#' @export
bg_opening_problem <- function(roll, problem_id = NULL, ...) {
  roll <- bg_as_opening_roll(roll)
  label <- bg_truth_roll_label(roll)

  bg_problem(
    state = bg_initial_board(),
    roll = roll,
    problem_id = if (is.null(problem_id)) paste0("opening_", gsub("-", "_", label, fixed = TRUE)) else problem_id,
    ...
  )
}

bg_is_default_opening_truth_identity <- function(
    problem,
    reference_mode = c("equal", "focused"),
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE) {
  reference_mode <- match.arg(reference_mode)
  dice_mode <- match.arg(dice_mode)

  identical(problem$settings$simulation_policy_engine, "random") &&
    identical(problem$settings$reward_model_canonical, "scalar_payoff") &&
    isTRUE(all.equal(problem$settings$unresolved_value, 0.5, tolerance = 1e-12)) &&
    identical(problem$settings$max_rollout_turns, 220L) &&
    identical(reference_mode, "equal") &&
    identical(dice_mode, "iid") &&
    !isTRUE(crn)
}

bg_opening_truth_file_stem <- function(
    problem,
    reference_mode = c("equal", "focused"),
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE) {
  reference_mode <- match.arg(reference_mode)
  dice_mode <- match.arg(dice_mode)

  parts <- c(
    paste0("opening_", gsub("-", "_", bg_truth_roll_label(problem$roll), fixed = TRUE)),
    bg_safe_file_label(reference_mode)
  )

  if (!bg_is_default_opening_truth_identity(
    problem = problem,
    reference_mode = reference_mode,
    dice_mode = dice_mode,
    crn = crn
  )) {
    parts <- c(
      parts,
      paste0(
        "truth_",
        substr(bg_truth_metadata_hash(
          problem = problem,
          reference_mode = reference_mode,
          dice_mode = dice_mode,
          crn = crn
        ), 1L, 8L)
      )
    )
  }

  paste(parts, collapse = "_")
}

bg_opening_truth_write_path <- function(
    problem,
    cache_dir = NULL,
    reference_mode = c("equal", "focused"),
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE) {
  cache_dir <- bg_resolve_opening_truth_cache_dir(cache_dir)
  file.path(
    cache_dir,
    paste0(
      bg_opening_truth_file_stem(
        problem = problem,
        reference_mode = reference_mode,
        dice_mode = dice_mode,
        crn = crn
      ),
      ".rds"
    )
  )
}

#' Build or load one cached opening-roll proxy truth
#'
#' @param roll An opening roll, supplied as a `bg_roll` object or a string such
#'   as `"1-6"`.
#' @param budget Integer-like proxy-truth budget.
#' @param simulation_policy Continuation policy used in the rollout model.
#' @param heuristic_policy Heuristic continuation policy used when
#'   `simulation_policy = "heuristic"`.
#' @param max_rollout_turns Integer-like rollout horizon.
#' @param unresolved_value Numeric unresolved payoff.
#' @param prior_alpha Prior alpha used by the wrapped opening problem.
#' @param prior_beta Prior beta used by the wrapped opening problem.
#' @param reward_model Reward definition used by the opening problem.
#' @param posterior_model Posterior family used by the opening problem.
#' @param posterior_prior Optional named list overriding the default prior.
#' @param n_cores Integer-like worker count used inside the truth build.
#' @param parallel Logical scalar; if `FALSE`, force one worker.
#' @param truth_block_size Integer-like block size passed to [bg_reference()].
#' @param reference_mode Reference allocation mode passed to [bg_reference()].
#' @param cache Logical scalar; if `TRUE`, save or reuse the cached opening
#'   truth object.
#' @param cache_dir Optional cache directory. Defaults to the package opening
#'   truth cache.
#' @param save_path Optional explicit `.rds` path.
#' @param overwrite Logical scalar controlling replacement of an existing
#'   `save_path`.
#' @param dice_mode Dice mode passed to [bg_reference()].
#' @param crn Logical scalar indicating common-random-number use.
#' @param seed Optional integer seed.
#'
#' @return A `bg_truth_state` object.
#' @export
bg_opening_truth_build_one <- function(
    roll,
    budget = 8192L,
    simulation_policy = c("random", "heuristic", "aggressive", "defensive", "ts_local"),
    heuristic_policy = c("aggressive", "defensive"),
    max_rollout_turns = 220L,
    unresolved_value = 0.5,
    prior_alpha = 1,
    prior_beta = 1,
    reward_model = c("scalar_payoff"),
    posterior_model = c("beta_pseudo"),
    posterior_prior = NULL,
    n_cores = bg_default_workers_truth(),
    parallel = TRUE,
    truth_block_size = 128L,
    reference_mode = c("equal", "focused"),
    cache = TRUE,
    cache_dir = NULL,
    save_path = NULL,
    overwrite = FALSE,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    seed = NULL) {
  reference_mode <- match.arg(reference_mode)
  dice_mode <- match.arg(dice_mode)

  problem <- bg_opening_problem(
    roll = roll,
    simulation_policy = simulation_policy,
    heuristic_policy = heuristic_policy,
    max_rollout_turns = max_rollout_turns,
    unresolved_value = unresolved_value,
    prior_alpha = prior_alpha,
    prior_beta = prior_beta,
    reward_model = reward_model,
    posterior_model = posterior_model,
    posterior_prior = posterior_prior
  )
  if (is.null(save_path) && isTRUE(cache)) {
    save_path <- bg_opening_truth_resolve_path(
      problem = problem,
      cache_dir = cache_dir,
      reference_mode = reference_mode,
      dice_mode = dice_mode,
      crn = crn
    )
  }

  bg_truth_state(
    problem = problem,
    budget = budget,
    n_cores = n_cores,
    parallel = parallel,
    truth_block_size = truth_block_size,
    reference_mode = reference_mode,
    cache = cache,
    cache_dir = cache_dir,
    save_path = save_path,
    overwrite = overwrite,
    dice_mode = dice_mode,
    crn = crn,
    seed = seed
  )
}

#' Build one reusable master opening-roll truth
#'
#' `bg_opening_master_truth_build_one()` is the opening-roll version of
#' [bg_master_truth_state()]. It builds one high-budget truth object for an
#' opening roll under the full scored-outcome representation, which can later
#' be projected into the package's other reward systems without rerunning the
#' rollout simulation.
#'
#' @inheritParams bg_opening_truth_build_one
#'
#' @return A `bg_truth_state` object built under
#'   `categorical_outcome + dirichlet_multinomial`.
#' @export
bg_opening_master_truth_build_one <- function(
    roll,
    budget = 8192L,
    simulation_policy = c("random", "heuristic", "aggressive", "defensive", "ts_local"),
    heuristic_policy = c("aggressive", "defensive"),
    max_rollout_turns = 220L,
    unresolved_value = 0.5,
    prior_alpha = 1,
    prior_beta = 1,
    n_cores = bg_default_workers_truth(),
    parallel = TRUE,
    truth_block_size = 128L,
    reference_mode = c("equal", "focused"),
    cache = TRUE,
    cache_dir = NULL,
    save_path = NULL,
    overwrite = FALSE,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    seed = NULL) {
  bg_opening_truth_build_one(
    roll = roll,
    budget = budget,
    simulation_policy = simulation_policy,
    heuristic_policy = heuristic_policy,
    max_rollout_turns = max_rollout_turns,
    unresolved_value = unresolved_value,
    prior_alpha = prior_alpha,
    prior_beta = prior_beta,
    reward_model = "categorical_outcome",
    posterior_model = "dirichlet_multinomial",
    n_cores = n_cores,
    parallel = parallel,
    truth_block_size = truth_block_size,
    reference_mode = reference_mode,
    cache = cache,
    cache_dir = cache_dir,
    save_path = save_path,
    overwrite = overwrite,
    dice_mode = dice_mode,
    crn = crn,
    seed = seed
  )
}

bg_opening_truth_summary_with_lookup <- function(truth) {
  if (!inherits(truth, "bg_truth_battery")) {
    stop("`truth` must inherit from class 'bg_truth_battery'.", call. = FALSE)
  }

  lookup <- bg_opening_rolls()
  idx <- match(truth$summary$roll, lookup$opening_roll)
  cbind(
    truth$summary,
    lookup[idx, c("opening_roll", "die1", "die2", "is_double", "roll_group"), drop = FALSE],
    stringsAsFactors = FALSE
  )
}

bg_opening_truth_resolve_path <- function(
    problem,
    cache_dir = NULL,
    reference_mode = c("equal", "focused"),
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE) {
  # Opening truths are cached as long-lived research artifacts. Prefer an
  # exact identity match, whether that file came from the preserved repo cache
  # or from a newer managed cache path.
  reference_mode <- match.arg(reference_mode)
  dice_mode <- match.arg(dice_mode)
  search_dir <- bg_resolve_opening_truth_cache_dir(cache_dir)

  preferred_paths <- unique(c(
    bg_opening_truth_write_path(
      problem = problem,
      cache_dir = search_dir,
      reference_mode = reference_mode,
      dice_mode = dice_mode,
      crn = crn
    ),
    bg_truth_storage_path(
      problem = problem,
      scope = "opening",
      cache_dir = search_dir,
      reference_mode = reference_mode,
      dice_mode = dice_mode,
      crn = crn
    )
  ))

  existing_preferred <- preferred_paths[file.exists(preferred_paths)]
  if (length(existing_preferred) > 0L) {
    return(existing_preferred[[1L]])
  }

  if (!dir.exists(search_dir)) {
    return(preferred_paths[[1L]])
  }

  roll_label <- bg_truth_roll_label(problem$roll)
  stem <- paste0("opening_", gsub("-", "_", roll_label, fixed = TRUE))
  candidates <- list.files(
    search_dir,
    pattern = paste0("^", stem, ".*\\.rds$"),
    full.names = TRUE
  )

  if (length(candidates) < 1L) {
    return(preferred_paths[[1L]])
  }

  target_hash <- bg_truth_metadata_hash(
    problem = problem,
    reference_mode = reference_mode,
    dice_mode = dice_mode,
    crn = crn
  )
  matching <- vapply(
    candidates,
    function(path) {
      truth <- tryCatch(bg_truth_load(path), error = function(e) NULL)
      inherits(truth, "bg_truth_state") && identical(bg_truth_object_hash(truth), target_hash)
    },
    logical(1L)
  )
  matches <- candidates[matching]

  if (length(matches) < 1L) {
    return(preferred_paths[[1L]])
  }

  budgets <- suppressWarnings(as.numeric(sub(".*_budget_([0-9]+)\\.rds$", "\\1", basename(matches))))
  if (any(is.finite(budgets))) {
    matches <- matches[order(budgets, file.info(matches)$mtime, decreasing = TRUE)]
    return(matches[[1L]])
  }

  matches[order(file.info(matches)$mtime, decreasing = TRUE)][[1L]]
}

bg_opening_truth_battery_from_truths <- function(truths) {
  if (inherits(truths, "bg_truth_battery")) {
    out <- truths
    if (!all(c("opening_roll", "die1", "die2", "is_double", "roll_group") %in% names(out$summary))) {
      out$summary <- bg_opening_truth_summary_with_lookup(out)
    }
    out$summary <- out$summary[order(out$summary$die1, out$summary$die2), , drop = FALSE]
    rownames(out$summary) <- NULL
    return(out)
  }

  if (inherits(truths, "bg_truth_state")) {
    truths <- list(truths)
  }
  if (!is.list(truths) || length(truths) < 1L || !all(vapply(truths, inherits, logical(1L), what = "bg_truth_state"))) {
    stop("`truths` must be a `bg_truth_battery`, `bg_truth_state`, or a non-empty list of truth states.", call. = FALSE)
  }

  if (is.null(names(truths)) || any(!nzchar(names(truths)))) {
    names(truths) <- vapply(truths, function(x) x$problem$problem_id, character(1L))
  }

  out <- structure(
    list(
      truths = truths,
      summary = bg_truth_battery_summary(truths),
      settings = list(source = "loaded_truth_states")
    ),
    class = "bg_truth_battery"
  )
  out$summary <- bg_opening_truth_summary_with_lookup(out)
  out$summary <- out$summary[order(out$summary$die1, out$summary$die2), , drop = FALSE]
  rownames(out$summary) <- NULL
  out
}

#' Build or load the full opening-roll truth battery
#'
#' @param rolls Optional vector/list of opening rolls. When omitted, the full 21
#'   unordered opening rolls are used.
#' @param include_doubles Logical scalar controlling whether doubles are part of
#'   the default battery.
#' @inheritParams bg_opening_truth_build_one
#' @param verbose Logical scalar; if `TRUE`, display a progress bar.
#' @param cache_dir Optional cache directory. Defaults to the package opening
#'   truth cache.
#' @param save_path Optional path for the battery object.
#'
#' @return A `bg_truth_battery` object.
#' @export
bg_opening_truth_build_all <- function(
    rolls = NULL,
    include_doubles = TRUE,
    budget = 8192L,
    simulation_policy = c("random", "heuristic", "aggressive", "defensive", "ts_local"),
    heuristic_policy = c("aggressive", "defensive"),
    max_rollout_turns = 220L,
    unresolved_value = 0.5,
    prior_alpha = 1,
    prior_beta = 1,
    reward_model = c("scalar_payoff"),
    posterior_model = c("beta_pseudo"),
    posterior_prior = NULL,
    n_cores = bg_default_workers_truth(),
    parallel = TRUE,
    truth_block_size = 128L,
    reference_mode = c("equal", "focused"),
    cache = TRUE,
    cache_dir = NULL,
    save_path = NULL,
    overwrite = FALSE,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    seed = NULL,
    verbose = interactive()) {
  if (is.null(rolls)) {
    grid <- bg_opening_rolls(include_doubles = include_doubles)
    rolls <- grid$roll
  } else {
    rolls <- lapply(rolls, bg_as_opening_roll)
  }

  truths <- bg_task_apply(
    tasks = as.list(rolls),
    worker = function(roll_i) {
      bg_opening_truth_build_one(
        roll = roll_i,
        budget = budget,
        simulation_policy = simulation_policy,
        heuristic_policy = heuristic_policy,
        max_rollout_turns = max_rollout_turns,
        unresolved_value = unresolved_value,
        prior_alpha = prior_alpha,
        prior_beta = prior_beta,
        reward_model = reward_model,
        posterior_model = posterior_model,
        posterior_prior = posterior_prior,
        n_cores = n_cores,
        parallel = parallel,
        truth_block_size = truth_block_size,
        reference_mode = reference_mode,
        cache = cache,
        cache_dir = cache_dir,
        overwrite = overwrite,
        dice_mode = dice_mode,
        crn = crn,
        seed = bg_derive_seed(seed, "opening-truth", bg_truth_roll_label(roll_i))
      )
    },
    n_cores = 1L,
    parallel = FALSE,
    progress = verbose,
    progress_label = "opening truths"
  )
  names(truths) <- vapply(truths, function(x) x$problem$problem_id, character(1L))

  truth <- structure(
    list(
      truths = truths,
      summary = bg_truth_battery_summary(truths),
      settings = list(
        budget = budget,
        n_cores = if (isTRUE(parallel)) n_cores else 1L,
        parallel = isTRUE(parallel),
        truth_block_size = truth_block_size,
        reference_mode = match.arg(reference_mode),
        cache = isTRUE(cache),
        cache_dir = cache_dir,
        dice_mode = match.arg(dice_mode),
        crn = isTRUE(crn),
        seed = seed
      )
    ),
    class = "bg_truth_battery"
  )
  truth$summary <- bg_opening_truth_summary_with_lookup(truth)
  if (!is.null(save_path)) {
    bg_truth_save(truth, save_path, overwrite = overwrite)
  }
  truth
}

#' Load one cached opening-roll proxy truth
#'
#' @param roll An opening roll, supplied as a `bg_roll` object or a string such
#'   as `"1-6"`.
#' @param simulation_policy Continuation policy used in the rollout model.
#' @param heuristic_policy Heuristic continuation policy used when
#'   `simulation_policy = "heuristic"`.
#' @param max_rollout_turns Integer-like rollout horizon.
#' @param unresolved_value Numeric unresolved payoff.
#' @param prior_alpha Prior alpha used by the wrapped opening problem.
#' @param prior_beta Prior beta used by the wrapped opening problem.
#' @param reward_model Reward definition used by the opening problem.
#' @param posterior_model Posterior family used by the opening problem.
#' @param posterior_prior Optional named list overriding the default prior.
#' @param reference_mode Reference allocation mode used when the truth was
#'   built.
#' @param dice_mode Dice mode used when the truth was built.
#' @param crn Logical scalar indicating whether the truth was built with common
#'   random numbers.
#' @param cache_dir Optional cache directory. Defaults to the package opening
#'   truth cache.
#'
#' @return A `bg_truth_state` object.
#' @export
bg_opening_truth_load_one <- function(
    roll,
    simulation_policy = c("random", "heuristic", "aggressive", "defensive", "ts_local"),
    heuristic_policy = c("aggressive", "defensive"),
    max_rollout_turns = 220L,
    unresolved_value = 0.5,
    prior_alpha = 1,
    prior_beta = 1,
    reward_model = c("scalar_payoff"),
    posterior_model = c("beta_pseudo"),
    posterior_prior = NULL,
    reference_mode = c("equal", "focused"),
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    cache_dir = NULL) {
  reference_mode <- match.arg(reference_mode)
  dice_mode <- match.arg(dice_mode)

  problem <- bg_opening_problem(
    roll = roll,
    simulation_policy = simulation_policy,
    heuristic_policy = heuristic_policy,
    max_rollout_turns = max_rollout_turns,
    unresolved_value = unresolved_value,
    prior_alpha = prior_alpha,
    prior_beta = prior_beta,
    reward_model = reward_model,
    posterior_model = posterior_model,
      posterior_prior = posterior_prior
  )
  path <- bg_opening_truth_resolve_path(
    problem = problem,
    cache_dir = cache_dir,
    reference_mode = reference_mode,
    dice_mode = dice_mode,
    crn = crn
  )
  truth <- bg_truth_load(path)
  bg_truth_align_requested_stack(
    truth = truth,
    reward_model = problem$settings$reward_model,
    posterior_model = problem$settings$posterior_model,
    posterior_prior = posterior_prior,
    unresolved_value = problem$settings$unresolved_value
  )
}

#' Index cached opening-roll proxy truths
#'
#' @param cache_dir Optional cache directory. Defaults to the package opening
#'   truth cache.
#' @param include_doubles Logical scalar controlling whether doubles are part of
#'   the index.
#' @inheritParams bg_opening_truth_load_one
#'
#' @return A data frame summarizing cache status and certification fields for
#'   each opening roll.
#' @export
bg_opening_truth_index <- function(
    cache_dir = NULL,
    include_doubles = TRUE,
    simulation_policy = c("random", "heuristic", "aggressive", "defensive", "ts_local"),
    heuristic_policy = c("aggressive", "defensive"),
    max_rollout_turns = 220L,
    unresolved_value = 0.5,
    prior_alpha = 1,
    prior_beta = 1,
    reward_model = c("scalar_payoff"),
    posterior_model = c("beta_pseudo"),
    posterior_prior = NULL,
    reference_mode = c("equal", "focused"),
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE) {
  grid <- bg_opening_rolls(include_doubles = include_doubles)
  reference_mode <- match.arg(reference_mode)
  dice_mode <- match.arg(dice_mode)

  rows <- lapply(
    seq_len(nrow(grid)),
    function(i) {
      problem <- bg_opening_problem(
        roll = grid$roll[[i]],
        simulation_policy = simulation_policy,
        heuristic_policy = heuristic_policy,
        max_rollout_turns = max_rollout_turns,
        unresolved_value = unresolved_value,
        prior_alpha = prior_alpha,
        prior_beta = prior_beta,
        reward_model = reward_model,
        posterior_model = posterior_model,
        posterior_prior = posterior_prior
      )
      path <- bg_opening_truth_resolve_path(
        problem = problem,
        cache_dir = cache_dir,
        reference_mode = reference_mode,
        dice_mode = dice_mode,
        crn = crn
      )
      exists <- file.exists(path)

      if (!exists) {
        return(data.frame(
          opening_roll = grid$opening_roll[[i]],
          die1 = grid$die1[[i]],
          die2 = grid$die2[[i]],
          is_double = grid$is_double[[i]],
          roll_group = grid$roll_group[[i]],
          path = normalizePath(path, mustWork = FALSE),
          status = "missing",
          reference_budget = NA_integer_,
          top_two_gap_estimate = NA_real_,
          mc_gap_excludes_zero = NA,
          difficulty_label = NA_character_,
          stringsAsFactors = FALSE
        ))
      }

      truth <- bg_truth_load(path)
      cert <- bg_truth_certify(truth)
      data.frame(
        opening_roll = grid$opening_roll[[i]],
        die1 = grid$die1[[i]],
        die2 = grid$die2[[i]],
        is_double = grid$is_double[[i]],
        roll_group = grid$roll_group[[i]],
        path = normalizePath(path, mustWork = FALSE),
        status = "cached",
        reference_budget = cert$reference_budget[[1L]],
        top_two_gap_estimate = cert$top_two_gap_estimate[[1L]],
        mc_gap_excludes_zero = cert$mc_gap_excludes_zero[[1L]],
        difficulty_label = cert$certification[[1L]],
        stringsAsFactors = FALSE
      )
    }
  )

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[order(out$die1, out$die2), , drop = FALSE]
}

#' @keywords internal
#' @noRd
bg_truth_opening <- function(...) {
  bg_opening_truth_build_all(...)
}
# Opening-roll comparison workflows centered on TS vs TTTS.

bg_opening_bootstrap_summary <- function(opening_summary, metric_cols, bootstrap_reps = 200L, seed = NULL) {
  bootstrap_reps <- bg_coerce_integerish(bootstrap_reps, "bootstrap_reps", 1L)
  if (bootstrap_reps < 1L) {
    stop("`bootstrap_reps` must be at least 1.", call. = FALSE)
  }

  split_key <- interaction(opening_summary$allocation_policy, opening_summary$checkpoint, drop = TRUE, lex.order = TRUE)
  rows <- lapply(
    split(opening_summary, split_key),
    function(df) {
      openings <- unique(df$opening_roll)
      point <- colMeans(df[, metric_cols, drop = FALSE], na.rm = TRUE)

      if (length(openings) <= 1L) {
        lower <- point
        upper <- point
      } else {
        boot <- bg_ts_with_seed(
          seed,
          replicate(
            bootstrap_reps,
            {
              sampled_openings <- sample(openings, size = length(openings), replace = TRUE)
              sampled <- do.call(
                rbind,
                lapply(sampled_openings, function(label) df[df$opening_roll == label, , drop = FALSE])
              )
              colMeans(sampled[, metric_cols, drop = FALSE], na.rm = TRUE)
            }
          )
        )
        if (is.null(dim(boot))) {
          boot <- matrix(boot, nrow = 1L)
        }
        lower <- apply(boot, 1L, stats::quantile, probs = 0.025, na.rm = TRUE)
        upper <- apply(boot, 1L, stats::quantile, probs = 0.975, na.rm = TRUE)
      }

      data.frame(
        allocation_policy = df$allocation_policy[[1L]],
        checkpoint = df$checkpoint[[1L]],
        n_openings = length(openings),
        metric = metric_cols,
        estimate = as.numeric(point[metric_cols]),
        lower_95 = as.numeric(lower[metric_cols]),
        upper_95 = as.numeric(upper[metric_cols]),
        stringsAsFactors = FALSE
      )
    }
  )

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Load the cached opening-roll truth battery
#'
#' `bg_opening_truth_load_all()` is the read-only counterpart to
#' [bg_opening_truth_build_all()]. Use it when the opening truths already exist
#' on disk and you want one coherent battery object without triggering any new
#' builds.
#'
#' @param rolls Optional vector/list of opening rolls. When omitted, the full 21
#'   unordered opening rolls are loaded.
#' @param include_doubles Logical scalar controlling whether doubles are part of
#'   the default battery.
#' @inheritParams bg_opening_truth_load_one
#'
#' @return A `bg_truth_battery` object with opening metadata in `summary`.
#' @export
bg_opening_truth_load_all <- function(
    rolls = NULL,
    include_doubles = TRUE,
    simulation_policy = c("random", "heuristic", "aggressive", "defensive", "ts_local"),
    heuristic_policy = c("aggressive", "defensive"),
    max_rollout_turns = 220L,
    unresolved_value = 0.5,
    prior_alpha = 1,
    prior_beta = 1,
    reward_model = c("scalar_payoff"),
    posterior_model = c("beta_pseudo"),
    posterior_prior = NULL,
    reference_mode = c("equal", "focused"),
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    cache_dir = NULL) {
  if (is.null(rolls)) {
    grid <- bg_opening_rolls(include_doubles = include_doubles)
    rolls <- grid$roll
  } else {
    rolls <- lapply(rolls, bg_as_opening_roll)
  }

  truths <- lapply(
    rolls,
    function(roll_i) {
      bg_opening_truth_load_one(
        roll = roll_i,
        simulation_policy = simulation_policy,
        heuristic_policy = heuristic_policy,
        max_rollout_turns = max_rollout_turns,
        unresolved_value = unresolved_value,
        prior_alpha = prior_alpha,
        prior_beta = prior_beta,
        reward_model = reward_model,
        posterior_model = posterior_model,
        posterior_prior = posterior_prior,
        reference_mode = reference_mode,
        dice_mode = dice_mode,
        crn = crn,
        cache_dir = cache_dir
      )
    }
  )

  bg_opening_truth_battery_from_truths(truths)
}

# -----------------------------------------------------------------------------
# Opening study workflows
# -----------------------------------------------------------------------------

bg_opening_contrast_table <- function(opening_summary) {
  if (!all(c("thompson", "top_two_thompson") %in% opening_summary$allocation_policy)) {
    return(data.frame(
      opening_roll = character(),
      checkpoint = integer(),
      metric = character(),
      ttts_minus_ts = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  metric_cols <- c(
    "mean_top1_match",
    "mean_simple_regret",
    "mean_share_top2_truth",
    "mean_share_mc_screened_suboptimal",
    "mean_gap_weighted_wasted_allocation",
    "mean_truth_top2_hit",
    "mean_truth_top_k_hit",
    "mean_restricted_pairwise_ordering_accuracy"
  )

  ts <- opening_summary[opening_summary$allocation_policy == "thompson", c("opening_roll", "checkpoint", metric_cols), drop = FALSE]
  ttts <- opening_summary[opening_summary$allocation_policy == "top_two_thompson", c("opening_roll", "checkpoint", metric_cols), drop = FALSE]
  merged <- merge(ts, ttts, by = c("opening_roll", "checkpoint"), suffixes = c("_ts", "_ttts"), sort = FALSE)
  if (nrow(merged) < 1L) {
    return(data.frame(
      opening_roll = character(),
      checkpoint = integer(),
      metric = character(),
      ttts_minus_ts = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- vector("list", length(metric_cols))
  row_id <- 1L
  out <- vector("list", nrow(merged) * length(metric_cols))
  for (i in seq_len(nrow(merged))) {
    for (metric in metric_cols) {
      out[[row_id]] <- data.frame(
        opening_roll = merged$opening_roll[[i]],
        checkpoint = merged$checkpoint[[i]],
        metric = metric,
        ttts_minus_ts = merged[[paste0(metric, "_ttts")]][[i]] - merged[[paste0(metric, "_ts")]][[i]],
        stringsAsFactors = FALSE
      )
      row_id <- row_id + 1L
    }
  }

  out <- do.call(rbind, out[seq_len(row_id - 1L)])
  rownames(out) <- NULL
  out
}

#' Compare TS and TTTS over the opening-roll battery
#'
#' `bg_opening_compare_study()` builds or loads opening-roll proxy references in
#' equal mode, runs a small coherent method set over a budget grid, summarizes
#' over seeds within opening roll, and only then aggregates over openings.
#'
#' @param rolls Optional vector/list of opening rolls. When omitted, the full 21
#'   unordered opening rolls are used.
#' @param include_doubles Logical scalar controlling whether doubles are part of
#'   the default opening battery.
#' @param methods Character vector of methods. Keep this set small and coherent.
#' @param budgets Integer-like budget vector.
#' @param seeds Integer-like seed vector for repeated method runs.
#' @param proxy_truths Optional opening truth source. Supply either a
#'   `bg_truth_battery` object or a non-empty list of `bg_truth_state` objects.
#'   When omitted, proxy references are built automatically.
#' @param truth_budget Integer-like equal-allocation reference budget used when
#'   `proxy_truths` is not supplied.
#' @param n_cores Integer-like worker count used by the comparison engine.
#' @param parallel Logical scalar; if `TRUE`, parallelize repeated study tasks.
#' @param progress Logical scalar; if `TRUE`, display progress.
#' @param cache_dir Optional opening-truth cache directory.
#' @param overwrite Logical scalar controlling replacement of an existing save.
#' @param save_path Optional `.rds` path for the returned study object.
#' @param bootstrap_reps Integer-like number of opening-level bootstrap
#'   replicates.
#' @param high_conf_threshold Numeric threshold used for the high-confidence but
#'   wrong rate.
#' @param seed Optional master seed used for truth construction and bootstrap
#'   resampling.
#' @param ... Additional arguments passed to [bg_compare_algorithms()].
#'
#' @return A `bg_opening_compare_study` object.
#' @export
bg_opening_compare_study <- function(
    rolls = NULL,
    include_doubles = TRUE,
    methods = c("thompson", "top_two_thompson", "equal"),
    budgets = c(16L, 32L, 64L, 128L, 256L, 512L),
    seeds = 1:30,
    proxy_truths = NULL,
    truth_budget = 4096L,
    n_cores = 1L,
    parallel = FALSE,
    progress = interactive(),
    cache_dir = NULL,
    overwrite = FALSE,
    save_path = NULL,
    bootstrap_reps = 200L,
    high_conf_threshold = 0.8,
    seed = NULL,
    ...) {
  cached <- bg_maybe_load_saved_study(save_path, overwrite = overwrite)
  if (!is.null(cached)) {
    return(cached)
  }

  bg_assert_scalar_flag(include_doubles, "include_doubles")
  budgets <- bg_normalize_study_budgets(budgets)
  seeds <- sort(unique(bg_coerce_integerish(seeds, "seeds", length(seeds))))
  n_cores <- bg_coerce_integerish(n_cores, "n_cores", 1L)
  bg_assert_scalar_flag(parallel, "parallel")
  bg_assert_scalar_flag(progress, "progress")
  bg_assert_scalar_flag(overwrite, "overwrite")
  bootstrap_reps <- bg_coerce_integerish(bootstrap_reps, "bootstrap_reps", 1L)
  if (!is.numeric(high_conf_threshold) || length(high_conf_threshold) != 1L || is.na(high_conf_threshold) ||
      high_conf_threshold <= 0 || high_conf_threshold > 1) {
    stop("`high_conf_threshold` must be a numeric scalar in (0, 1].", call. = FALSE)
  }

  if (is.null(proxy_truths)) {
    proxy_truths <- bg_opening_truth_build_all(
      rolls = rolls,
      include_doubles = include_doubles,
      budget = truth_budget,
      n_cores = n_cores,
      parallel = FALSE,
      reference_mode = "equal",
      cache = TRUE,
      cache_dir = cache_dir,
      overwrite = overwrite,
      seed = bg_derive_seed(seed, "opening-study-truth"),
      verbose = progress
    )
  }
  if (!inherits(proxy_truths, "bg_truth_battery")) {
    proxy_truths <- bg_opening_truth_battery_from_truths(proxy_truths)
  }
  if (!all(c("opening_roll", "die1", "die2", "is_double", "roll_group") %in% names(proxy_truths$summary))) {
    stop("`proxy_truths` must be an opening-roll truth battery with opening metadata in `summary`.", call. = FALSE)
  }
  if (!all(proxy_truths$summary$reference_mode == "equal")) {
    warning(
      "The supplied opening truths were not all built in `reference_mode = 'equal'`. ",
      "Use equal-mode references for headline opening-study conclusions.",
      call. = FALSE
    )
  }

  problems <- lapply(proxy_truths$truths, `[[`, "problem")
  references <- lapply(proxy_truths$truths, `[[`, "reference")
  comparison <- bg_compare_algorithms(
    problems = problems,
    methods = methods,
    budgets = budgets,
    seeds = seeds,
    proxy_references = references,
    n_cores = n_cores,
    parallel = parallel,
    progress = progress,
    ...
  )

  panel <- bg_eval_reference_aware(comparison, truth = proxy_truths, top_k = 3L)
  runtime_panel <- bg_eval_runtime_panel(comparison)
  seed_panel <- Reduce(
    function(left, right) merge(left, right, by = c("problem_id", "allocation_policy", "checkpoint", "seed", "ts_mode"), all = TRUE, sort = FALSE),
    list(
      panel,
      runtime_panel
    )
  )

  opening_lookup <- proxy_truths$summary[, c("problem_id", "opening_roll", "die1", "die2", "is_double", "roll_group"), drop = FALSE]
  seed_panel <- merge(seed_panel, opening_lookup, by = "problem_id", all.x = TRUE, sort = FALSE)
  seed_panel$truth_top2_hit <- as.numeric(seed_panel$truth_top2_hit)
  seed_panel$truth_top_k_hit <- as.numeric(seed_panel$truth_top_k_hit)
  seed_panel$high_confidence_wrong <- !is.na(seed_panel$recommended_prob_best) &
    seed_panel$recommended_prob_best >= high_conf_threshold &
    !is.na(seed_panel$top1_match) &
    !seed_panel$top1_match

  split_key <- interaction(seed_panel$opening_roll, seed_panel$allocation_policy, seed_panel$checkpoint, seed_panel$ts_mode, drop = TRUE, lex.order = TRUE)
  opening_summary <- lapply(
    split(seed_panel, split_key),
    function(df) {
      recommendation_modal_share <- bg_truth_modal_share(df$recommended_move_label)

      data.frame(
        opening_roll = df$opening_roll[[1L]],
        die1 = df$die1[[1L]],
        die2 = df$die2[[1L]],
        is_double = df$is_double[[1L]],
        roll_group = df$roll_group[[1L]],
        allocation_policy = df$allocation_policy[[1L]],
        checkpoint = df$checkpoint[[1L]],
        ts_mode = df$ts_mode[[1L]],
        n_seeds = length(unique(df$seed[!is.na(df$seed)])),
        mean_top1_match = mean(df$top1_match, na.rm = TRUE),
        mean_simple_regret = mean(df$simple_regret, na.rm = TRUE),
        mean_selected_reference_rank = mean(df$selected_reference_rank, na.rm = TRUE),
        mean_share_top2_truth = mean(df$share_top2_truth, na.rm = TRUE),
        mean_share_mc_screened_suboptimal = mean(df$share_mc_screened_suboptimal, na.rm = TRUE),
        mean_gap_weighted_wasted_allocation = mean(df$gap_weighted_wasted_allocation, na.rm = TRUE),
        mean_truth_top2_hit = mean(df$truth_top2_hit, na.rm = TRUE),
        mean_truth_top_k_hit = mean(df$truth_top_k_hit, na.rm = TRUE),
        mean_restricted_pairwise_ordering_accuracy = mean(df$restricted_pairwise_ordering_accuracy, na.rm = TRUE),
        mean_runtime_seconds = mean(df$runtime_seconds, na.rm = TRUE),
        recommendation_instability = if (is.na(recommendation_modal_share)) NA_real_ else 1 - recommendation_modal_share,
        high_confidence_wrong_rate = mean(df$high_confidence_wrong, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }
  )
  opening_summary <- do.call(rbind, opening_summary)
  rownames(opening_summary) <- NULL

  aggregate_metrics <- c(
    "mean_top1_match",
    "mean_simple_regret",
    "mean_share_top2_truth",
    "mean_share_mc_screened_suboptimal",
    "mean_gap_weighted_wasted_allocation",
    "mean_truth_top2_hit",
    "mean_truth_top_k_hit",
    "recommendation_instability",
    "high_confidence_wrong_rate"
  )
  opening_aggregate <- bg_opening_bootstrap_summary(
    opening_summary = opening_summary,
    metric_cols = aggregate_metrics,
    bootstrap_reps = bootstrap_reps,
    seed = bg_derive_seed(seed, "opening-study-bootstrap")
  )

  out <- structure(
    list(
      truths = proxy_truths,
      comparison = comparison,
      seed_panel = seed_panel,
      opening_summary = opening_summary,
      opening_aggregate = opening_aggregate,
      contrasts = bg_opening_contrast_table(opening_summary),
      settings = list(
        methods = methods,
        budgets = budgets,
        seeds = seeds,
        truth_budget = truth_budget,
        n_cores = n_cores,
        parallel = isTRUE(parallel),
        bootstrap_reps = bootstrap_reps,
        high_conf_threshold = high_conf_threshold,
        seed = seed
      )
    ),
    class = "bg_opening_compare_study"
  )

  if (!is.null(save_path)) {
    bg_study_save(out, save_path, overwrite = overwrite)
  }
  out
}

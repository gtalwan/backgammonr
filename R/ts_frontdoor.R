# Core TS problem construction, proxy-reference building, and Thompson engine logic.
bg_ts_private <- new.env(parent = emptyenv())
bg_ts_private$problem_cache <- new.env(parent = emptyenv())

# Normalize public policy names once so every front door shares the same
# accepted spellings and error messages.
bg_match_allocation_policy_public <- function(policy) {
  if (length(policy) > 1L) {
    policy <- policy[[1L]]
  }
  match.arg(
    policy,
    choices = c(
      "thompson",
      "top_two_thompson",
      "multi_sample_thompson",
      "tempered_thompson",
      "budget_aware_thompson",
      "elimination_thompson",
      "ranking_aware_thompson",
      "equal",
      "greedy",
      "ucb",
      "ocba"
    )
  )
}

# Thompson-style methods are treated as one family even when their internal
# choice rules differ, so keep the family membership test centralized.
bg_is_thompson_policy_public <- function(policy) {
  policy <- bg_match_allocation_policy_public(policy)
  policy %in% c(
    "thompson",
    "top_two_thompson",
    "multi_sample_thompson",
    "tempered_thompson",
    "budget_aware_thompson",
    "elimination_thompson",
    "ranking_aware_thompson"
  )
}

# Keep the experimental TS list explicit so docs, warnings, and plots all
# demote the same policies consistently.
bg_experimental_ts_policies <- function() {
  c(
    "multi_sample_thompson",
    "tempered_thompson",
    "budget_aware_thompson",
    "elimination_thompson",
    "ranking_aware_thompson"
  )
}

# Map public policy names onto the smaller set of engine labels used by the
# fast scalar path and by shared summary code.
bg_allocation_policy_engine <- function(policy) {
  policy <- bg_match_allocation_policy_public(policy)
  switch(
    policy,
    thompson = "thompson",
    top_two_thompson = "ttts",
    multi_sample_thompson = "thompson",
    tempered_thompson = "thompson",
    budget_aware_thompson = "thompson",
    elimination_thompson = "thompson",
    ranking_aware_thompson = "thompson",
    equal = "equal",
    greedy = "greedy",
    ucb = "ucb",
    ocba = "ocba"
  )
}

bg_match_simulation_policy_public <- function(policy) {
  if (length(policy) > 1L) {
    policy <- policy[[1L]]
  }
  match.arg(
    policy,
    choices = c("random", "heuristic", "aggressive", "defensive", "ts_local")
  )
}

bg_resolve_simulation_policy <- function(
    simulation_policy = c("random", "heuristic", "aggressive", "defensive", "ts_local"),
    heuristic_policy = c("aggressive", "defensive")) {
  simulation_policy <- bg_match_simulation_policy_public(simulation_policy)
  heuristic_policy <- match.arg(heuristic_policy)

  if (simulation_policy == "heuristic") {
    return(list(
      simulation_policy = simulation_policy,
      simulation_policy_engine = heuristic_policy,
      note = sprintf(
        "Simulation policy 'heuristic' currently resolves to the '%s' heuristic rollout policy.",
        heuristic_policy
      ),
      experimental = FALSE
    ))
  }

  if (simulation_policy == "ts_local") {
    return(list(
      simulation_policy = simulation_policy,
      simulation_policy_engine = "thompson_rollout",
      note = paste(
        "Simulation policy 'ts_local' is experimental and changes the rollout-model estimand.",
        "It uses local Thompson-rollout move choice inside continuation playouts."
      ),
      experimental = TRUE
    ))
  }

  list(
    simulation_policy = simulation_policy,
    simulation_policy_engine = simulation_policy,
    note = NULL,
    experimental = FALSE
  )
}

# Checkpoints are reused by traces, comparisons, and evaluation panels. Build
# the default grid once so all workflows report the same budgets.
bg_checkpoint_grid <- function(budget, checkpoints = NULL) {
  budget <- bg_coerce_integerish(budget, "budget", 1L)
  if (budget < 1L) {
    stop("`budget` must be at least 1.", call. = FALSE)
  }

  if (!is.null(checkpoints)) {
    checkpoints <- sort(unique(bg_coerce_integerish(checkpoints, "checkpoints", length(checkpoints))))
    checkpoints <- checkpoints[checkpoints >= 1L & checkpoints <= budget]
    if (length(checkpoints) == 0L || checkpoints[[length(checkpoints)]] != budget) {
      checkpoints <- c(checkpoints, budget)
    }
    return(checkpoints)
  }

  small <- seq_len(min(8L, budget))
  powers <- 2L^(0:ceiling(log(budget, base = 2)))
  powers <- powers[powers <= budget]
  checkpoints <- sort(unique(c(small, powers, budget)))
  as.integer(checkpoints)
}

# Cache keys are based on the full rollout-model estimand, not just board + roll.
# Changing continuation policy or model stack should produce a distinct problem.
bg_problem_key <- function(
    board,
    roll,
    simulation_policy_engine,
    max_rollout_turns,
    unresolved_value,
    reward_model,
    posterior_model,
    model_signature = NULL) {
  paste(
    paste(unclass(board)$points, collapse = ","),
    paste(unclass(board)$bar, collapse = ","),
    paste(unclass(board)$off, collapse = ","),
    unclass(board)$turn,
    paste(bg_as_roll(roll)$dice, collapse = "-"),
    simulation_policy_engine,
    max_rollout_turns,
    format(as.numeric(unresolved_value), scientific = FALSE, trim = TRUE),
    reward_model,
    posterior_model,
    if (is.null(model_signature)) "" else as.character(model_signature),
    sep = "::"
  )
}

# Some workflows need deterministic posterior draws without permanently
# clobbering the session RNG state.
bg_ts_with_seed <- function(seed, expr) {
  if (is.null(seed)) {
    return(force(expr))
  }

  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
  } else {
    on.exit(rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
  }

  set.seed(bg_coerce_integerish(seed, "seed", 1L))
  force(expr)
}

# The fast scalar path still uses a lightweight probability-best diagnostic.
# Keep it separate from the richer explicit-posterior summary machinery.
bg_probability_best_summary <- function(alpha, beta, draws = 256L, seed = NULL) {
  alpha <- as.numeric(alpha)
  beta <- as.numeric(beta)
  draws <- bg_coerce_integerish(draws, "draws", 1L)

  if (length(alpha) == 0L) {
    return(list(prob_best = numeric(0L), expected_regret = numeric(0L)))
  }

  draw_mat <- bg_ts_with_seed(
    seed,
    vapply(
      seq_along(alpha),
      function(i) stats::rbeta(draws, alpha[[i]], beta[[i]]),
      numeric(draws)
    )
  )

  if (is.null(dim(draw_mat))) {
    draw_mat <- matrix(draw_mat, ncol = 1L)
  }

  winner <- max.col(draw_mat, ties.method = "first")
  best_draw <- draw_mat[cbind(seq_len(draws), winner)]
  prob_best <- tabulate(winner, nbins = ncol(draw_mat)) / draws
  expected_regret <- colMeans(best_draw - draw_mat)

  list(
    prob_best = as.numeric(prob_best),
    expected_regret = as.numeric(expected_regret)
  )
}

# Allocation concentration summaries feed run objects, study tables, and plots.
bg_allocation_concentration <- function(counts) {
  counts <- as.numeric(counts)
  total <- sum(counts)
  if (total <= 0) {
    return(list(entropy = NA_real_, hhi = NA_real_, max_share = NA_real_))
  }

  share <- counts / total
  positive <- share[share > 0]
  entropy <- if (length(positive) <= 1L) {
    0
  } else {
    -sum(positive * log(positive)) / log(length(share))
  }

  list(
    entropy = entropy,
    hhi = sum(share^2),
    max_share = max(share)
  )
}

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

bg_move_action_features <- function(move) {
  move <- bg_as_move_sequence(move)
  steps <- move$steps

  data.frame(
    n_steps = move$n_steps,
    n_hits = sum(vapply(steps, function(step) isTRUE(step$hit), logical(1L))),
    n_bar_entries = sum(vapply(steps, function(step) step$from == 0L, logical(1L))),
    n_bear_off = sum(vapply(steps, function(step) step$to == 25L, logical(1L))),
    total_step_distance = sum(vapply(steps, function(step) abs(step$to - step$from), numeric(1L))),
    stringsAsFactors = FALSE
  )
}

bg_problem_candidate_table <- function(board, legal_moves, simulation_policy_engine, max_rollout_turns, unresolved_value) {
  if (length(legal_moves) == 0L) {
    out <- data.frame(
      candidate_index = integer(0L),
      representative_index = integer(0L),
      n_equivalent_sequences = integer(0L),
      stringsAsFactors = FALSE
    )
    out$move <- I(vector("list", 0L))
    out$move_label <- character(0L)
    out$n_steps <- integer(0L)
    out$n_hits <- integer(0L)
    out$n_bar_entries <- integer(0L)
    out$n_bear_off <- integer(0L)
    out$total_step_distance <- numeric(0L)
    return(out)
  }

  legal_moves_unclass <- lapply(legal_moves, bg_unclass_move_sequence)
  collapsed <- bg_cpp_rollout_blocks(
    unclass(board),
    legal_moves_unclass,
    integer(0L),
    integer(0L),
    integer(0L),
    simulation_policy_engine,
    max_rollout_turns,
    unresolved_value,
    "iid",
    FALSE,
    1L,
    0L,
    FALSE
  )$candidate_map

  collapsed <- as.data.frame(collapsed, stringsAsFactors = FALSE)
  collapsed$move <- I(lapply(collapsed$candidate_index, function(i) legal_moves[[i]]))
  collapsed$move_label <- vapply(collapsed$move, bg_move_label, character(1L))
  collapsed$n_steps <- vapply(collapsed$move, function(move) move$n_steps, integer(1L))
  action_features <- do.call(
    rbind,
    lapply(collapsed$move, bg_move_action_features)
  )
  rownames(action_features) <- NULL
  cbind(
    collapsed,
    action_features[, setdiff(names(action_features), "n_steps"), drop = FALSE],
    stringsAsFactors = FALSE
  )
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

bg_checkpoint_match_summary <- function(results, method_col = "allocation_policy") {
  grouped <- aggregate(
    results[["proxy_reference_match"]],
    by = list(
      method = results[[method_col]],
      checkpoint = results[["checkpoint"]]
    ),
    FUN = mean,
    na.rm = TRUE
  )
  names(grouped)[names(grouped) == "x"] <- "proxy_reference_match_rate"
  grouped
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
  # Recompute the reference summary columns in C++ so all proxy-reference
  # builds share the same formulas and interval semantics.
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

  # The state-level summary is intentionally one row per decision problem.
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

  structure(
    list(
      problem = problem,
      action_table = tab,
      summary = summary,
      warnings = unique(warnings),
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

bg_empty_ts_action_table <- function(problem) {
  tab <- problem$candidate_table
  tab$empirical_value <- numeric(nrow(tab))
  stats <- bg_stats_table_template(tab$candidate_index)
  tab <- merge(tab, stats, by = "candidate_index", sort = FALSE, all.x = TRUE)
  tab <- tab[match(problem$candidate_table$candidate_index, tab$candidate_index), , drop = FALSE]
  tab$alpha <- numeric(nrow(tab))
  tab$beta <- numeric(nrow(tab))
  tab$estimate <- numeric(nrow(tab))
  tab$posterior_sd <- numeric(nrow(tab))
  tab$lower_95 <- numeric(nrow(tab))
  tab$upper_95 <- numeric(nrow(tab))
  tab$posterior_interval_type <- character(nrow(tab))
  tab$sample_variance <- numeric(nrow(tab))
  tab$model_relative_prob_best <- numeric(nrow(tab))
  tab$model_relative_expected_regret <- numeric(nrow(tab))
  tab$rank <- integer(nrow(tab))
  tab$recommended <- logical(nrow(tab))
  tab$proxy_reference_mean <- numeric(nrow(tab))
  tab$proxy_reference_rank <- integer(nrow(tab))
  tab$simple_regret <- numeric(nrow(tab))
  tab$proxy_reference_mc_lower_95 <- numeric(nrow(tab))
  tab$proxy_reference_mc_upper_95 <- numeric(nrow(tab))
  tab
}

bg_empty_checkpoint_table <- function(checkpoints, allocation_policy, ts_mode) {
  checkpoints <- as.integer(checkpoints)
  data.frame(
    checkpoint = checkpoints,
    allocation_policy = rep(allocation_policy, length(checkpoints)),
    ts_mode = rep(ts_mode, length(checkpoints)),
    recommended_index = rep(NA_integer_, length(checkpoints)),
    recommended_move_label = rep(NA_character_, length(checkpoints)),
    recommended_estimate = rep(NA_real_, length(checkpoints)),
    recommended_prob_best = rep(NA_real_, length(checkpoints)),
    recommended_expected_regret = rep(NA_real_, length(checkpoints)),
    recommended_allocation_count = rep(NA_integer_, length(checkpoints)),
    n_actions = rep(0L, length(checkpoints)),
    allocation_entropy = rep(NA_real_, length(checkpoints)),
    allocation_hhi = rep(NA_real_, length(checkpoints)),
    allocation_max_share = rep(NA_real_, length(checkpoints)),
    top_two_prob_best_mass = rep(NA_real_, length(checkpoints)),
    unresolved_fraction = rep(NA_real_, length(checkpoints)),
    runtime_seconds = rep(0, length(checkpoints)),
    rollout_throughput = rep(NA_real_, length(checkpoints)),
    proxy_reference_best_index = rep(NA_integer_, length(checkpoints)),
    proxy_reference_best_move_label = rep(NA_character_, length(checkpoints)),
    simple_regret = rep(NA_real_, length(checkpoints)),
    selected_reference_rank = rep(NA_integer_, length(checkpoints)),
    proxy_reference_match = rep(NA, length(checkpoints)),
    stringsAsFactors = FALSE
  )
}

bg_empty_ts_run <- function(problem, allocation_policy, budget, checkpoints, reference, legacy_evaluation, settings, ts_mode, warnings) {
  checkpoint_table <- bg_empty_checkpoint_table(checkpoints, allocation_policy, ts_mode)
  checkpoint_actions <- bg_empty_ts_action_table(problem)
  checkpoint_actions$checkpoint <- integer(0L)
  bg_make_ts_run(
    problem = problem,
    allocation_policy = allocation_policy,
    budget = budget,
    ts_mode = ts_mode,
    action_table = bg_empty_ts_action_table(problem),
    checkpoint_table = checkpoint_table,
    checkpoint_actions = checkpoint_actions,
    reference = reference,
    legacy_evaluation = legacy_evaluation,
    settings = settings,
    warnings = unique(c("No legal moves are available for this decision problem.", warnings))
  )
}

bg_rebuild_checkpoint_from_trace <- function(
    problem,
    trace_rows,
    checkpoint,
    total_budget,
    runtime_seconds,
    allocation_policy,
    reference = NULL,
    prob_best_draws = 256L,
    seed = NULL,
    ts_mode = "sequential") {
  tab <- trace_rows[trace_rows$checkpoint == checkpoint, , drop = FALSE]
  tab <- tab[order(tab$candidate_index), , drop = FALSE]
  tab <- merge(
    problem$candidate_table,
    tab,
    by = "candidate_index",
    sort = FALSE,
    all.y = TRUE
  )

  probs <- bg_probability_best_summary(
    alpha = tab$alpha,
    beta = tab$beta,
    draws = prob_best_draws,
    seed = bg_derive_seed(seed, "prob_best", allocation_policy, checkpoint)
  )
  tab$model_relative_prob_best <- probs$prob_best
  tab$model_relative_expected_regret <- probs$expected_regret
  tab$sample_variance <- ifelse(
    tab$allocation_count > 1L,
    pmax(
      (
        (tab$wins + problem$settings$unresolved_value^2 * tab$unresolved) -
          ((tab$wins + problem$settings$unresolved_value * tab$unresolved)^2 / tab$allocation_count)
      ) / (tab$allocation_count - 1L),
      0
    ),
    NA_real_
  )

  concentration <- bg_allocation_concentration(tab$allocation_count)
  selected_index <- tab$leader_index[[1L]]
  selected_row <- tab[tab$candidate_index == selected_index, , drop = FALSE]
  if (nrow(selected_row) == 0L) {
    selected_row <- tab[which.max(tab$estimate), , drop = FALSE]
    selected_index <- selected_row$candidate_index[[1L]]
  }
  throughput <- if (isTRUE(runtime_seconds > 0)) total_budget / runtime_seconds else NA_real_
  approx_runtime <- if (isTRUE(throughput > 0)) checkpoint / throughput else NA_real_

  summary <- data.frame(
    checkpoint = checkpoint,
    allocation_policy = allocation_policy,
    ts_mode = ts_mode,
    recommended_index = selected_index,
    recommended_move_label = selected_row$move_label[[1L]],
    recommended_estimate = selected_row$estimate[[1L]],
    recommended_prob_best = selected_row$model_relative_prob_best[[1L]],
    recommended_expected_regret = selected_row$model_relative_expected_regret[[1L]],
    recommended_allocation_count = selected_row$allocation_count[[1L]],
    n_actions = nrow(tab),
    allocation_entropy = concentration$entropy,
    allocation_hhi = concentration$hhi,
    allocation_max_share = concentration$max_share,
    top_two_prob_best_mass = sum(sort(tab$model_relative_prob_best, decreasing = TRUE)[seq_len(min(2L, nrow(tab)))]),
    unresolved_fraction = if (sum(tab$allocation_count) > 0L) sum(tab$unresolved) / sum(tab$allocation_count) else NA_real_,
    runtime_seconds = approx_runtime,
    rollout_throughput = throughput,
    stringsAsFactors = FALSE
  )

  if (!is.null(reference)) {
    ref <- bg_reference_snapshot_public(reference)
    summary$proxy_reference_best_index <- ref$best_index
    summary$proxy_reference_best_move_label <- ref$label_lookup[[as.character(ref$best_index)]]
    chosen_ref <- ref$value_lookup[[as.character(selected_index)]]
    summary$simple_regret <- ref$best_value - chosen_ref
    summary$selected_reference_rank <- ref$rank_lookup[[as.character(selected_index)]]
    summary$proxy_reference_match <- selected_index == ref$best_index
  } else {
    summary$proxy_reference_best_index <- NA_integer_
    summary$proxy_reference_best_move_label <- NA_character_
    summary$simple_regret <- NA_real_
    summary$selected_reference_rank <- NA_integer_
    summary$proxy_reference_match <- NA
  }

  list(summary = summary, actions = tab)
}

bg_action_table_from_stats <- function(
    problem,
    stats_table,
    allocation_policy,
    reference = NULL,
    prob_best_draws = 256L,
    seed = NULL) {
  if (nrow(problem$candidate_table) == 0L) {
    return(bg_empty_ts_action_table(problem))
  }

  tab <- stats_table[order(stats_table$candidate_index), , drop = FALSE]
  tab <- merge(problem$candidate_table, tab, by = "candidate_index", sort = FALSE, all.y = TRUE)
  tab$empirical_value <- ifelse(tab$allocation_count > 0L, tab$reward_sum / tab$allocation_count, NA_real_)
  tab$alpha <- problem$settings$prior_alpha + tab$reward_sum
  tab$beta <- problem$settings$prior_beta + (tab$allocation_count - tab$reward_sum)
  tab$estimate <- tab$alpha / (tab$alpha + tab$beta)
  tab$posterior_sd <- sqrt((tab$alpha * tab$beta) / ((tab$alpha + tab$beta)^2 * (tab$alpha + tab$beta + 1)))
  tab$lower_95 <- stats::qbeta(0.025, tab$alpha, tab$beta)
  tab$upper_95 <- stats::qbeta(0.975, tab$alpha, tab$beta)
  tab$posterior_interval_type <- "pseudo_beta_quantile"
  tab$sample_variance <- ifelse(
    tab$allocation_count > 1L,
    pmax((tab$reward_sum_sq - (tab$reward_sum^2 / tab$allocation_count)) / (tab$allocation_count - 1L), 0),
    NA_real_
  )
  probs <- bg_probability_best_summary(
    alpha = tab$alpha,
    beta = tab$beta,
    draws = prob_best_draws,
    seed = bg_derive_seed(seed, "final-prob-best", allocation_policy)
  )
  tab$model_relative_prob_best <- probs$prob_best
  tab$model_relative_expected_regret <- probs$expected_regret
  tab <- tab[order(-tab$estimate, tab$candidate_index), , drop = FALSE]
  tab$rank <- seq_len(nrow(tab))
  tab$recommended <- FALSE
  tab$recommended[[1L]] <- TRUE

  if (!is.null(reference)) {
    ref <- bg_reference_snapshot_public(reference)
    tab$proxy_reference_mean <- ref$value_lookup[as.character(tab$candidate_index)]
    tab$proxy_reference_rank <- ref$rank_lookup[as.character(tab$candidate_index)]
    tab$simple_regret <- ref$best_value - tab$proxy_reference_mean
    if ("reference_mc_lower_95" %in% names(ref$table)) {
      ref_lower_lookup <- stats::setNames(ref$table$reference_mc_lower_95, ref$table$candidate_index)
      ref_upper_lookup <- stats::setNames(ref$table$reference_mc_upper_95, ref$table$candidate_index)
      tab$proxy_reference_mc_lower_95 <- ref_lower_lookup[as.character(tab$candidate_index)]
      tab$proxy_reference_mc_upper_95 <- ref_upper_lookup[as.character(tab$candidate_index)]
    } else {
      tab$proxy_reference_mc_lower_95 <- NA_real_
      tab$proxy_reference_mc_upper_95 <- NA_real_
    }
  } else {
    tab$proxy_reference_mean <- NA_real_
    tab$proxy_reference_rank <- NA_integer_
    tab$simple_regret <- NA_real_
    tab$proxy_reference_mc_lower_95 <- NA_real_
    tab$proxy_reference_mc_upper_95 <- NA_real_
  }

  rownames(tab) <- NULL
  tab
}

bg_collect_run_warnings <- function(problem, action_table, checkpoint_table, reference = NULL, ts_mode = "sequential") {
  warnings <- character(0L)
  if (nrow(action_table) == 0L) {
    return("No legal moves are available for this decision problem.")
  }

  if (problem$settings$simulation_policy != "random") {
    warnings <- c(
      warnings,
      sprintf(
        "Simulation policy '%s' differs from the benchmark default 'random'; the rollout-model value is changed.",
        problem$settings$simulation_policy
      )
    )
  }

  if (isTRUE(problem$settings$simulation_policy == "ts_local")) {
    warnings <- c(
      warnings,
      "Simulation policy 'ts_local' is experimental and changes the continuation policy inside rollouts."
    )
  }

  if (ts_mode == "batched") {
    warnings <- c(
      warnings,
      "Batched TS mode is experimental and differs from the canonical sequential Thompson semantics."
    )
  }

  if (!is.null(checkpoint_table$allocation_policy) &&
      any(checkpoint_table$allocation_policy %in% bg_experimental_ts_policies(), na.rm = TRUE)) {
    warnings <- c(
      warnings,
      "This run uses an experimental Thompson-inspired policy rather than canonical TS or top-two TS."
    )
  }

  if (mean(checkpoint_table$unresolved_fraction, na.rm = TRUE) > 0.1) {
    warnings <- c(
      warnings,
      "Unresolved rollout fraction is high; truncation may materially affect the model-relative recommendation."
    )
  }
  if (sum(action_table$unresolved, na.rm = TRUE) > 0L &&
      !problem$settings$unresolved_value %in% c(0, 1) &&
      identical(problem$settings$posterior_model_canonical, "beta_pseudo")) {
    warnings <- c(
      warnings,
      sprintf(
        "Some rollouts are unresolved and receive payoff %.3f; `%s + %s` is therefore an approximate pseudo-conjugate summary rather than exact Bernoulli conjugacy.",
        problem$settings$unresolved_value
        ,
        problem$settings$reward_model,
        problem$settings$posterior_model
      )
    )
  }

  collapse_reduction <- length(problem$legal_moves) - nrow(problem$candidate_table)
  if (collapse_reduction > 0L) {
    warnings <- c(
      warnings,
      sprintf(
        "Identical post-move collapse reduced %d raw legal moves to %d unique candidate states.",
        length(problem$legal_moves),
        nrow(problem$candidate_table)
      )
    )
  }

  if (!is.null(reference) && length(reference$warnings) > 0L) {
    warnings <- c(warnings, reference$warnings)
  }

  unique(warnings)
}

bg_make_ts_run <- function(
    problem,
    allocation_policy,
    budget,
    ts_mode,
    action_table,
    checkpoint_table,
    checkpoint_actions,
    reference,
    legacy_evaluation,
    settings,
    warnings) {
  recommended <- if (nrow(action_table) == 0L) action_table else action_table[action_table$recommended, , drop = FALSE]
  structure(
    list(
      problem = problem,
      action_table = action_table,
      checkpoint_table = checkpoint_table,
      checkpoint_actions = checkpoint_actions,
      reference = reference,
      evaluation = legacy_evaluation,
      settings = settings,
      warnings = warnings,
      budget = budget,
      allocation_policy = allocation_policy,
      ts_mode = ts_mode,
      recommended_index = if (nrow(recommended) == 0L) NA_integer_ else recommended$candidate_index[[1L]],
      recommended_move = if (nrow(recommended) == 0L) NULL else recommended$move[[1L]],
      recommended_move_label = if (nrow(recommended) == 0L) NA_character_ else recommended$move_label[[1L]]
    ),
    class = "bg_ts_run"
  )
}

bg_run_method_path <- function(
    problem,
    allocation_policy,
    budget,
    checkpoints,
    reference = NULL,
    seed = NULL,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    fast_diagnostics = FALSE,
    ttts_beta = 0.5,
    ucb_exploration = 1,
    ...) {
  allocation_policy <- bg_match_allocation_policy_public(allocation_policy)
  if (!bg_problem_supports_legacy_scalar_engine(problem)) {
    stop(
      sprintf(
        "`allocation_policy = '%s'` currently uses the legacy scalar engine and is only available for ",
        allocation_policy
      ),
      "`reward_model = 'scalar_payoff'` with `posterior_model = 'beta_pseudo'`. ",
      "Use Thompson-family policies on the explicit posterior path, or rebuild the problem ",
      "on the legacy scalar stack before calling scalar-engine comparators.",
      call. = FALSE
    )
  }
  engine_method <- bg_allocation_policy_engine(allocation_policy)
  dice_mode <- match.arg(dice_mode)
  checkpoints <- bg_checkpoint_grid(budget, checkpoints)

  evaluation <- bg_evaluate_actions_method(
    board = problem$board,
    method = engine_method,
    legal_moves = problem$legal_moves,
    total_budget = budget,
    rollout_policy = problem$settings$simulation_policy_engine,
    max_rollout_turns = problem$settings$max_rollout_turns,
    unresolved_value = problem$settings$unresolved_value,
    initial_allocations = 1L,
    ucb_exploration = if (allocation_policy == "top_two_thompson") {
      as.numeric(ttts_beta)
    } else if (allocation_policy == "ucb") {
      as.numeric(ucb_exploration)
    } else {
      1
    },
    prior_alpha = problem$settings$prior_alpha,
    prior_beta = problem$settings$prior_beta,
    dice_mode = dice_mode,
    crn = crn,
    fast_diagnostics = fast_diagnostics,
    trace = TRUE,
    trace_every = 1L,
    seed = seed
  )

  if (nrow(evaluation$results) == 0L) {
    return(bg_empty_ts_run(
      problem = problem,
      allocation_policy = allocation_policy,
      budget = budget,
      checkpoints = checkpoints,
      reference = reference,
      legacy_evaluation = evaluation,
      settings = list(
        budget = budget,
        checkpoints = checkpoints,
        seed = seed,
        dice_mode = dice_mode,
        crn = isTRUE(crn),
        reward_model = problem$settings$reward_model,
        posterior_model = problem$settings$posterior_model,
        engine_path = "legacy_scalar_engine",
        ucb_exploration = if (allocation_policy == "ucb") as.numeric(ucb_exploration) else NA_real_
      ),
      ts_mode = "sequential",
      warnings = character(0L)
    ))
  }

  trace_rows <- evaluation$trace[evaluation$trace$checkpoint %in% checkpoints, , drop = FALSE]
  pieces <- lapply(
    checkpoints,
    function(ck) bg_rebuild_checkpoint_from_trace(
      problem = problem,
      trace_rows = trace_rows,
      checkpoint = ck,
      total_budget = budget,
      runtime_seconds = evaluation$runtime_seconds,
      allocation_policy = allocation_policy,
      reference = reference,
      seed = seed,
      ts_mode = "sequential"
    )
  )

  checkpoint_table <- do.call(rbind, lapply(pieces, `[[`, "summary"))
  checkpoint_actions <- do.call(rbind, Map(
    function(piece, ck) {
      action_df <- piece$actions
      action_df$checkpoint <- ck
      action_df
    },
    pieces,
    checkpoints
  ))

  action_table <- evaluation$results
  action_table$model_relative_prob_best <- action_table$prob_best
  action_table$model_relative_expected_regret <- action_table$posterior_expected_regret
  if (!is.null(reference)) {
    ref <- bg_reference_snapshot_public(reference)
    action_table$proxy_reference_mean <- ref$value_lookup[as.character(action_table$candidate_index)]
    action_table$proxy_reference_rank <- ref$rank_lookup[as.character(action_table$candidate_index)]
    action_table$simple_regret <- ref$best_value - action_table$proxy_reference_mean
  } else {
    action_table$proxy_reference_mean <- NA_real_
    action_table$proxy_reference_rank <- NA_integer_
    action_table$simple_regret <- NA_real_
  }

  warnings <- bg_collect_run_warnings(problem, action_table, checkpoint_table, reference = reference, ts_mode = "sequential")

  bg_make_ts_run(
    problem = problem,
    allocation_policy = allocation_policy,
    budget = budget,
    ts_mode = "sequential",
    action_table = action_table,
    checkpoint_table = checkpoint_table,
    checkpoint_actions = checkpoint_actions,
    reference = reference,
    legacy_evaluation = evaluation,
    settings = list(
      budget = budget,
      checkpoints = checkpoints,
      seed = seed,
      dice_mode = dice_mode,
      crn = isTRUE(crn),
      reward_model = problem$settings$reward_model,
      posterior_model = problem$settings$posterior_model,
      engine_path = "legacy_scalar_engine",
      ucb_exploration = if (allocation_policy == "ucb") as.numeric(ucb_exploration) else NA_real_
    ),
    warnings = warnings
  )
}

bg_batch_allocation_counts <- function(alpha, beta, batch_size, allocation_policy, seed = NULL, ttts_beta = 0.5) {
  n_actions <- length(alpha)
  allocation_policy <- bg_match_allocation_policy_public(allocation_policy)
  counts <- integer(n_actions)
  if (n_actions == 0L || batch_size <= 0L) {
    return(counts)
  }

  bg_ts_with_seed(seed, {
    for (i in seq_len(batch_size)) {
      if (allocation_policy == "thompson") {
        winner <- which.max(stats::rbeta(n_actions, alpha, beta))
        counts[[winner]] <- counts[[winner]] + 1L
        next
      }

      if (allocation_policy == "top_two_thompson") {
        draw_winner <- function() which.max(stats::rbeta(n_actions, alpha, beta))
        top1 <- draw_winner()
        if (n_actions == 1L || stats::runif(1L) <= ttts_beta) {
          counts[[top1]] <- counts[[top1]] + 1L
          next
        }

        for (attempt in seq_len(64L)) {
          top2 <- draw_winner()
          if (top2 != top1) {
            counts[[top2]] <- counts[[top2]] + 1L
            break
          }
          if (attempt == 64L) {
            counts[[top1]] <- counts[[top1]] + 1L
          }
        }
      }
    }
  })

  counts
}

bg_temper_beta_parameters <- function(alpha, beta, temperature = 1) {
  if (!is.numeric(temperature) || length(temperature) != 1L || is.na(temperature) || temperature <= 0) {
    stop("`temperature` must be a positive numeric scalar.", call. = FALSE)
  }

  total <- pmax(alpha + beta, 2 + 1e-8)
  mean <- alpha / total
  adjusted_total <- pmax(2 + 1e-8, 2 + (total - 2) / temperature)

  list(
    alpha = pmax(mean * adjusted_total, 1e-6),
    beta = pmax((1 - mean) * adjusted_total, 1e-6)
  )
}

bg_variant_draw_matrix <- function(alpha, beta, draws, temperature = 1) {
  draws <- bg_coerce_integerish(draws, "draws", 1L)
  tempered <- bg_temper_beta_parameters(alpha, beta, temperature = temperature)
  mat <- vapply(
    seq_along(alpha),
    function(i) stats::rbeta(draws, tempered$alpha[[i]], tempered$beta[[i]]),
    numeric(draws)
  )
  if (is.null(dim(mat))) {
    mat <- matrix(mat, ncol = 1L)
  }
  mat
}

bg_variant_pick_index <- function(scores, allocation_count, tie_break = NULL) {
  scores <- as.numeric(scores)
  allocation_count <- as.numeric(allocation_count)
  if (length(scores) < 1L) {
    stop("`scores` must be non-empty.", call. = FALSE)
  }

  best <- which(scores >= max(scores, na.rm = TRUE) - 1e-12)
  if (length(best) == 1L) {
    return(best[[1L]])
  }

  least_allocated <- allocation_count[best]
  best <- best[least_allocated == min(least_allocated, na.rm = TRUE)]
  if (length(best) == 1L || is.null(tie_break)) {
    return(best[[1L]])
  }

  tie_break <- as.numeric(tie_break)
  best[[which.max(tie_break[best])]]
}

bg_variant_top_two_choice <- function(draw_mat, allocation_count, ttts_beta = 0.5) {
  winner_once <- function() {
    bg_variant_pick_index(
      scores = draw_mat[sample.int(nrow(draw_mat), 1L), ],
      allocation_count = allocation_count
    )
  }

  top1 <- winner_once()
  if (ncol(draw_mat) == 1L) {
    return(top1)
  }

  if (!is.numeric(ttts_beta) || length(ttts_beta) != 1L || is.na(ttts_beta) || ttts_beta <= 0 || ttts_beta > 1) {
    ttts_beta <- 0.5
  }

  if (stats::runif(1L) <= ttts_beta) {
    return(top1)
  }

  for (attempt in seq_len(64L)) {
    top2 <- winner_once()
    if (!identical(top2, top1)) {
      return(top2)
    }
  }

  posterior_mean <- colMeans(draw_mat)
  posterior_mean[top1] <- -Inf
  bg_variant_pick_index(
    scores = posterior_mean,
    allocation_count = allocation_count,
    tie_break = posterior_mean
  )
}

bg_variant_ranking_choice <- function(draw_mat, posterior_mean, allocation_count, focus_top_k = 3L) {
  focus_top_k <- bg_coerce_integerish(focus_top_k, "focus_top_k", 1L)
  if (ncol(draw_mat) <= 1L) {
    return(1L)
  }

  focus_local <- order(posterior_mean, decreasing = TRUE)[seq_len(min(focus_top_k, length(posterior_mean)))]
  if (length(focus_local) <= 1L) {
    return(focus_local[[1L]])
  }

  uncertainty <- numeric(length(posterior_mean))
  for (i in focus_local) {
    pair_uncertainty <- numeric(0L)
    for (j in focus_local) {
      if (identical(i, j)) {
        next
      }
      p_ij <- mean(draw_mat[, i] > draw_mat[, j])
      pair_uncertainty <- c(pair_uncertainty, p_ij * (1 - p_ij))
    }
    uncertainty[[i]] <- sum(pair_uncertainty)
  }

  bg_variant_pick_index(
    scores = uncertainty,
    allocation_count = allocation_count,
    tie_break = posterior_mean
  )
}

bg_variant_update_active_set <- function(
    active,
    alpha,
    beta,
    allocation_count,
    min_allocations = 4L,
    keep_top = 2L,
    margin = 0) {
  min_allocations <- bg_coerce_integerish(min_allocations, "min_allocations", 1L)
  keep_top <- bg_coerce_integerish(keep_top, "keep_top", 1L)
  if (!is.numeric(margin) || length(margin) != 1L || is.na(margin) || margin < 0) {
    stop("`margin` must be a nonnegative numeric scalar.", call. = FALSE)
  }

  active_idx <- which(active)
  if (length(active_idx) <= keep_top) {
    return(active)
  }
  if (any(allocation_count[active_idx] < min_allocations)) {
    return(active)
  }

  posterior_mean <- alpha / (alpha + beta)
  posterior_sd <- sqrt((alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1)))
  lower_95 <- pmax(posterior_mean - 1.96 * posterior_sd, 0)
  upper_95 <- pmin(posterior_mean + 1.96 * posterior_sd, 1)
  leader_lower <- max(lower_95[active_idx], na.rm = TRUE)
  keep_idx <- active_idx[order(posterior_mean[active_idx], decreasing = TRUE)][seq_len(min(keep_top, length(active_idx)))]

  eliminated <- setdiff(
    active_idx[upper_95[active_idx] + margin < leader_lower],
    keep_idx
  )
  if (length(eliminated) > 0L) {
    active[eliminated] <- FALSE
  }
  active
}

bg_mark_recommended_from_eligibility <- function(action_table) {
  if (!"eligible" %in% names(action_table) || nrow(action_table) < 1L) {
    return(action_table)
  }

  action_table$recommended <- FALSE
  keep <- which(action_table$eligible)
  if (length(keep) < 1L) {
    keep <- 1L
  }
  action_table$recommended[[keep[[1L]]]] <- TRUE
  action_table
}

bg_variant_select_candidate <- function(
    allocation_policy,
    alpha,
    beta,
    allocation_count,
    spent,
    budget,
    active,
    multi_sample_draws = 5L,
    temperature = 1.25,
    ttts_beta = 0.5,
    ranking_top_k = 3L,
    ranking_draws = 128L) {
  allocation_policy <- bg_match_allocation_policy_public(allocation_policy)
  active_idx <- which(active)
  if (length(active_idx) < 1L) {
    stop("No active actions remain.", call. = FALSE)
  }
  if (length(active_idx) == 1L) {
    return(active_idx[[1L]])
  }

  local_alpha <- alpha[active_idx]
  local_beta <- beta[active_idx]
  local_count <- allocation_count[active_idx]
  posterior_mean <- local_alpha / (local_alpha + local_beta)

  local_choice <- switch(
    allocation_policy,
    thompson = {
      draw <- bg_variant_draw_matrix(local_alpha, local_beta, draws = 1L, temperature = 1)
      bg_variant_pick_index(draw[1L, ], local_count, tie_break = posterior_mean)
    },
    top_two_thompson = {
      draw_mat <- bg_variant_draw_matrix(local_alpha, local_beta, draws = max(64L, ranking_draws), temperature = 1)
      bg_variant_top_two_choice(draw_mat, local_count, ttts_beta = ttts_beta)
    },
    multi_sample_thompson = {
      draw_mat <- bg_variant_draw_matrix(local_alpha, local_beta, draws = max(2L, multi_sample_draws), temperature = 1)
      winners <- apply(draw_mat, 1L, function(x) bg_variant_pick_index(x, local_count, tie_break = posterior_mean))
      winner_freq <- tabulate(winners, nbins = ncol(draw_mat))
      bg_variant_pick_index(winner_freq, local_count, tie_break = posterior_mean)
    },
    tempered_thompson = {
      draw <- bg_variant_draw_matrix(local_alpha, local_beta, draws = 1L, temperature = temperature)
      bg_variant_pick_index(draw[1L, ], local_count, tie_break = posterior_mean)
    },
    ranking_aware_thompson = {
      draw_mat <- bg_variant_draw_matrix(local_alpha, local_beta, draws = max(64L, ranking_draws), temperature = 1)
      bg_variant_ranking_choice(
        draw_mat = draw_mat,
        posterior_mean = posterior_mean,
        allocation_count = local_count,
        focus_top_k = ranking_top_k
      )
    },
    budget_aware_thompson = {
      progress <- if (budget <= 0L) 1 else spent / budget
      if (progress < 0.3) {
        draw_mat <- bg_variant_draw_matrix(
          local_alpha,
          local_beta,
          draws = max(3L, multi_sample_draws),
          temperature = max(temperature, 1.35)
        )
        winners <- apply(draw_mat, 1L, function(x) bg_variant_pick_index(x, local_count, tie_break = posterior_mean))
        winner_freq <- tabulate(winners, nbins = ncol(draw_mat))
        bg_variant_pick_index(winner_freq, local_count, tie_break = posterior_mean)
      } else {
        sorted_mean <- sort(posterior_mean, decreasing = TRUE)
        top_gap <- if (length(sorted_mean) >= 2L) sorted_mean[[1L]] - sorted_mean[[2L]] else Inf
        posterior_sd <- sqrt((local_alpha * local_beta) / ((local_alpha + local_beta)^2 * (local_alpha + local_beta + 1)))
        ordered_sd <- posterior_sd[order(posterior_mean, decreasing = TRUE)]
        gap_sd <- if (length(ordered_sd) >= 2L) sqrt(sum(ordered_sd[1:2]^2)) else 0

        if (progress >= 0.75 && is.finite(top_gap) && is.finite(gap_sd) && top_gap <= 1.25 * gap_sd) {
          draw_mat <- bg_variant_draw_matrix(local_alpha, local_beta, draws = max(64L, ranking_draws), temperature = 1)
          bg_variant_top_two_choice(draw_mat, local_count, ttts_beta = ttts_beta)
        } else if (progress >= 0.75) {
          draw_mat <- bg_variant_draw_matrix(local_alpha, local_beta, draws = max(64L, ranking_draws), temperature = 1)
          bg_variant_ranking_choice(
            draw_mat = draw_mat,
            posterior_mean = posterior_mean,
            allocation_count = local_count,
            focus_top_k = ranking_top_k
          )
        } else {
          draw <- bg_variant_draw_matrix(local_alpha, local_beta, draws = 1L, temperature = 1)
          bg_variant_pick_index(draw[1L, ], local_count, tie_break = posterior_mean)
        }
      }
    },
    elimination_thompson = {
      draw <- bg_variant_draw_matrix(local_alpha, local_beta, draws = 1L, temperature = 1)
      bg_variant_pick_index(draw[1L, ], local_count, tie_break = posterior_mean)
    },
    stop("Unsupported Thompson-family variant.", call. = FALSE)
  )

  active_idx[[local_choice]]
}

bg_checkpoint_summary_from_action_table <- function(
    action_table,
    allocation_policy,
    checkpoint,
    runtime_seconds,
    reference = NULL,
    ts_mode = "sequential") {
  concentration <- bg_allocation_concentration(action_table$allocation_count)
  recommended <- action_table[action_table$recommended, , drop = FALSE]

  out <- data.frame(
    checkpoint = checkpoint,
    allocation_policy = allocation_policy,
    ts_mode = ts_mode,
    recommended_index = recommended$candidate_index[[1L]],
    recommended_move_label = recommended$move_label[[1L]],
    recommended_estimate = recommended$estimate[[1L]],
    recommended_prob_best = recommended$model_relative_prob_best[[1L]],
    recommended_expected_regret = recommended$model_relative_expected_regret[[1L]],
    recommended_allocation_count = recommended$allocation_count[[1L]],
    n_actions = nrow(action_table),
    allocation_entropy = concentration$entropy,
    allocation_hhi = concentration$hhi,
    allocation_max_share = concentration$max_share,
    top_two_prob_best_mass = sum(sort(action_table$model_relative_prob_best, decreasing = TRUE)[seq_len(min(2L, nrow(action_table)))]),
    unresolved_fraction = if (sum(action_table$allocation_count) > 0L) sum(action_table$unresolved) / sum(action_table$allocation_count) else NA_real_,
    runtime_seconds = runtime_seconds,
    rollout_throughput = if (isTRUE(runtime_seconds > 0)) checkpoint / runtime_seconds else NA_real_,
    stringsAsFactors = FALSE
  )

  if (!is.null(reference)) {
    ref <- bg_reference_snapshot_public(reference)
    out$proxy_reference_best_index <- ref$best_index
    out$proxy_reference_best_move_label <- ref$label_lookup[[as.character(ref$best_index)]]
    out$simple_regret <- ref$best_value - ref$value_lookup[[as.character(recommended$candidate_index[[1L]])]]
    out$selected_reference_rank <- ref$rank_lookup[[as.character(recommended$candidate_index[[1L]])]]
    out$proxy_reference_match <- identical(recommended$candidate_index[[1L]], ref$best_index)
  } else {
    out$proxy_reference_best_index <- NA_integer_
    out$proxy_reference_best_move_label <- NA_character_
    out$simple_regret <- NA_real_
    out$selected_reference_rank <- NA_integer_
    out$proxy_reference_match <- NA
  }

  out
}

bg_run_custom_ts_variant <- function(
    problem,
    allocation_policy,
    budget,
    checkpoints,
    reference = NULL,
    seed = NULL,
    task_block_size = 1L,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    multi_sample_draws = 5L,
    temperature = 1.25,
    ranking_top_k = 3L,
    ranking_draws = 128L,
    elimination_min_allocations = 4L,
    elimination_keep_top = 2L,
    elimination_margin = 0,
    ttts_beta = 0.5) {
  allocation_policy <- bg_match_allocation_policy_public(allocation_policy)
  dice_mode <- match.arg(dice_mode)
  task_block_size <- bg_coerce_integerish(task_block_size, "task_block_size", 1L)
  multi_sample_draws <- bg_coerce_integerish(multi_sample_draws, "multi_sample_draws", 1L)
  ranking_top_k <- bg_coerce_integerish(ranking_top_k, "ranking_top_k", 1L)
  ranking_draws <- bg_coerce_integerish(ranking_draws, "ranking_draws", 1L)

  candidate_table <- problem$candidate_table[order(problem$candidate_table$candidate_index), , drop = FALSE]
  if (nrow(candidate_table) == 0L) {
    return(bg_empty_ts_run(
      problem = problem,
      allocation_policy = allocation_policy,
      budget = budget,
      checkpoints = checkpoints,
      reference = reference,
      legacy_evaluation = NULL,
      settings = list(
        budget = budget,
        checkpoints = checkpoints,
        seed = seed,
        task_block_size = task_block_size,
        dice_mode = dice_mode,
        crn = isTRUE(crn),
        multi_sample_draws = multi_sample_draws,
        temperature = temperature,
        ranking_top_k = ranking_top_k,
        ranking_draws = ranking_draws,
        elimination_min_allocations = elimination_min_allocations,
        elimination_keep_top = elimination_keep_top,
        elimination_margin = elimination_margin,
        reward_model = problem$settings$reward_model,
        posterior_model = problem$settings$posterior_model
      ),
      ts_mode = "sequential",
      warnings = character(0L)
    ))
  }

  stats_table <- bg_stats_table_template(candidate_table$candidate_index)
  active <- rep(TRUE, nrow(stats_table))
  eliminated_at <- rep(NA_integer_, nrow(stats_table))
  checkpoint_rows <- vector("list", length(checkpoints))
  checkpoint_actions <- vector("list", length(checkpoints))
  spent <- 0L
  runtime_start <- proc.time()[[3L]]

  bg_ts_with_seed(seed, {
    for (ck_idx in seq_along(checkpoints)) {
      checkpoint_target <- checkpoints[[ck_idx]]

      while (spent < checkpoint_target) {
        alpha <- problem$settings$prior_alpha + stats_table$reward_sum
        beta <- problem$settings$prior_beta + (stats_table$allocation_count - stats_table$reward_sum)

        if (identical(allocation_policy, "elimination_thompson")) {
          updated <- bg_variant_update_active_set(
            active = active,
            alpha = alpha,
            beta = beta,
            allocation_count = stats_table$allocation_count,
            min_allocations = elimination_min_allocations,
            keep_top = elimination_keep_top,
            margin = elimination_margin
          )
          newly_eliminated <- which(active & !updated)
          if (length(newly_eliminated) > 0L) {
            eliminated_at[newly_eliminated] <- spent
          }
          active <- updated
        }

        chosen_pos <- bg_variant_select_candidate(
          allocation_policy = allocation_policy,
          alpha = alpha,
          beta = beta,
          allocation_count = stats_table$allocation_count,
          spent = spent,
          budget = budget,
          active = active,
          multi_sample_draws = multi_sample_draws,
          temperature = temperature,
          ttts_beta = ttts_beta,
          ranking_top_k = ranking_top_k,
          ranking_draws = ranking_draws
        )

        block_results <- bg_call_rollout_blocks(
          problem = problem,
          candidate_index = candidate_table$candidate_index[chosen_pos],
          block_rollouts = 1L,
          start_counts = stats_table$allocation_count[chosen_pos],
          task_block_size = task_block_size,
          dice_mode = dice_mode,
          crn = crn,
          seed = bg_derive_seed(seed, "variant-rollout", allocation_policy, spent + 1L)
        )

        stats_table$allocation_count[chosen_pos] <- stats_table$allocation_count[chosen_pos] + block_results$added_allocation_count[[1L]]
        stats_table$wins[chosen_pos] <- stats_table$wins[chosen_pos] + block_results$wins[[1L]]
        stats_table$losses[chosen_pos] <- stats_table$losses[chosen_pos] + block_results$losses[[1L]]
        for (nm in bg_scored_outcome_columns()) {
          stats_table[[nm]][chosen_pos] <- stats_table[[nm]][chosen_pos] + block_results[[nm]][[1L]]
        }
        stats_table$reward_sum[chosen_pos] <- stats_table$reward_sum[chosen_pos] + block_results$reward_sum[[1L]]
        stats_table$reward_sum_sq[chosen_pos] <- stats_table$reward_sum_sq[chosen_pos] + block_results$reward_sum_sq[[1L]]
        spent <- spent + 1L
      }

      action_table_ck <- bg_action_table_from_stats(
        problem = problem,
        stats_table = stats_table,
        allocation_policy = allocation_policy,
        reference = reference,
        seed = bg_derive_seed(seed, "variant-final", allocation_policy, checkpoint_target)
      )
      if (identical(allocation_policy, "elimination_thompson")) {
        alpha <- problem$settings$prior_alpha + stats_table$reward_sum
        beta <- problem$settings$prior_beta + (stats_table$allocation_count - stats_table$reward_sum)
        updated <- bg_variant_update_active_set(
          active = active,
          alpha = alpha,
          beta = beta,
          allocation_count = stats_table$allocation_count,
          min_allocations = elimination_min_allocations,
          keep_top = elimination_keep_top,
          margin = elimination_margin
        )
        newly_eliminated <- which(active & !updated)
        if (length(newly_eliminated) > 0L) {
          eliminated_at[newly_eliminated] <- spent
        }
        active <- updated
      }
      action_match <- match(action_table_ck$candidate_index, candidate_table$candidate_index)
      action_table_ck$eligible <- active[action_match]
      action_table_ck$eliminated_at <- eliminated_at[action_match]
      if (identical(allocation_policy, "elimination_thompson")) {
        action_table_ck <- bg_mark_recommended_from_eligibility(action_table_ck)
      }
      checkpoint_rows[[ck_idx]] <- bg_checkpoint_summary_from_action_table(
        action_table = action_table_ck,
        allocation_policy = allocation_policy,
        checkpoint = checkpoint_target,
        runtime_seconds = proc.time()[[3L]] - runtime_start,
        reference = reference,
        ts_mode = "sequential"
      )
      action_table_ck$checkpoint <- checkpoint_target
      checkpoint_actions[[ck_idx]] <- action_table_ck
    }
  })

  checkpoint_table <- do.call(rbind, checkpoint_rows)
  action_table <- bg_action_table_from_stats(
    problem = problem,
    stats_table = stats_table,
    allocation_policy = allocation_policy,
    reference = reference,
    seed = bg_derive_seed(seed, "variant-final-table", allocation_policy)
  )
  action_match <- match(action_table$candidate_index, candidate_table$candidate_index)
  action_table$eligible <- active[action_match]
  action_table$eliminated_at <- eliminated_at[action_match]
  if (identical(allocation_policy, "elimination_thompson")) {
    action_table <- bg_mark_recommended_from_eligibility(action_table)
  }
  warnings <- bg_collect_run_warnings(problem, action_table, checkpoint_table, reference = reference, ts_mode = "sequential")
  warnings <- unique(c(
    warnings,
    sprintf(
      "%s is an experimental Thompson-family variant implemented on the current scalar `%s + %s` rollout engine.",
      bg_allocation_policy_label(allocation_policy),
      problem$settings$reward_model,
      problem$settings$posterior_model
    )
  ))

  bg_make_ts_run(
    problem = problem,
    allocation_policy = allocation_policy,
    budget = budget,
    ts_mode = "sequential",
    action_table = action_table,
    checkpoint_table = checkpoint_table,
    checkpoint_actions = do.call(rbind, checkpoint_actions),
    reference = reference,
    legacy_evaluation = NULL,
    settings = list(
      budget = budget,
      checkpoints = checkpoints,
      seed = seed,
      task_block_size = task_block_size,
      dice_mode = dice_mode,
      crn = isTRUE(crn),
      multi_sample_draws = multi_sample_draws,
      temperature = temperature,
      ranking_top_k = ranking_top_k,
      ranking_draws = ranking_draws,
      elimination_min_allocations = elimination_min_allocations,
      elimination_keep_top = elimination_keep_top,
      elimination_margin = elimination_margin,
      reward_model = problem$settings$reward_model,
      posterior_model = problem$settings$posterior_model
    ),
    warnings = warnings
  )
}

bg_run_batched_ts <- function(
    problem,
    allocation_policy,
    budget,
    checkpoints,
    reference = NULL,
    seed = NULL,
    batch_size = 16L,
    dynamic_batching = FALSE,
    task_block_size = 64L,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    ttts_beta = 0.5,
    ...) {
  allocation_policy <- bg_match_allocation_policy_public(allocation_policy)
  if (!allocation_policy %in% c("thompson", "top_two_thompson")) {
    stop("Batched mode is only implemented for Thompson-family allocation policies.", call. = FALSE)
  }
  dice_mode <- match.arg(dice_mode)
  checkpoints <- bg_checkpoint_grid(budget, checkpoints)
  batch_size <- bg_coerce_integerish(batch_size, "batch_size", 1L)
  if (batch_size < 1L) {
    stop("`batch_size` must be at least 1.", call. = FALSE)
  }
  task_block_size <- bg_coerce_integerish(task_block_size, "task_block_size", 1L)

  candidate_table <- problem$candidate_table[order(problem$candidate_table$candidate_index), , drop = FALSE]
  if (nrow(candidate_table) == 0L) {
    return(bg_empty_ts_run(
      problem = problem,
      allocation_policy = allocation_policy,
      budget = budget,
      checkpoints = checkpoints,
      reference = reference,
      legacy_evaluation = NULL,
      settings = list(
        budget = budget,
        checkpoints = checkpoints,
        seed = seed,
        batch_size = batch_size,
        dynamic_batching = isTRUE(dynamic_batching),
        task_block_size = task_block_size,
        dice_mode = dice_mode,
        crn = isTRUE(crn),
        reward_model = problem$settings$reward_model,
        posterior_model = problem$settings$posterior_model
      ),
      ts_mode = "batched",
      warnings = character(0L)
    ))
  }

  stats_table <- bg_stats_table_template(candidate_table$candidate_index)

  checkpoint_rows <- vector("list", length(checkpoints))
  checkpoint_actions <- vector("list", length(checkpoints))
  spent <- 0L
  runtime_start <- proc.time()[[3L]]

  for (ck_idx in seq_along(checkpoints)) {
    checkpoint_target <- checkpoints[[ck_idx]]
    while (spent < checkpoint_target) {
      remaining <- checkpoint_target - spent
      batch_now <- if (isTRUE(dynamic_batching)) {
        min(remaining, max(1L, min(batch_size, as.integer(2^floor(log(max(spent, 1), 2))))))
      } else {
        min(remaining, batch_size)
      }

      alpha <- problem$settings$prior_alpha + stats_table$reward_sum
      beta <- problem$settings$prior_beta + (stats_table$allocation_count - stats_table$reward_sum)
      allocations <- bg_batch_allocation_counts(
        alpha = alpha,
        beta = beta,
        batch_size = batch_now,
        allocation_policy = allocation_policy,
        seed = bg_derive_seed(seed, "batched-allocation", allocation_policy, spent, checkpoint_target),
        ttts_beta = ttts_beta
      )

      chosen <- which(allocations > 0L)
      block_results <- bg_call_rollout_blocks(
        problem = problem,
        candidate_index = candidate_table$candidate_index[chosen],
        block_rollouts = allocations[chosen],
        start_counts = stats_table$allocation_count[chosen],
        task_block_size = task_block_size,
        dice_mode = dice_mode,
        crn = crn,
        seed = bg_derive_seed(seed, "batched-rollouts", allocation_policy, spent, checkpoint_target)
      )

      stats_table$allocation_count[chosen] <- stats_table$allocation_count[chosen] + block_results$added_allocation_count
      stats_table$wins[chosen] <- stats_table$wins[chosen] + block_results$wins
      stats_table$losses[chosen] <- stats_table$losses[chosen] + block_results$losses
      for (nm in bg_scored_outcome_columns()) {
        stats_table[[nm]][chosen] <- stats_table[[nm]][chosen] + block_results[[nm]]
      }
      stats_table$reward_sum[chosen] <- stats_table$reward_sum[chosen] + block_results$reward_sum
      stats_table$reward_sum_sq[chosen] <- stats_table$reward_sum_sq[chosen] + block_results$reward_sum_sq
      spent <- spent + batch_now
    }

    action_table_ck <- bg_action_table_from_stats(
      problem = problem,
      stats_table = stats_table,
      allocation_policy = allocation_policy,
      reference = reference,
      seed = bg_derive_seed(seed, "batched-final", checkpoints[[ck_idx]])
    )
    concentration <- bg_allocation_concentration(action_table_ck$allocation_count)
    recommended <- action_table_ck[action_table_ck$recommended, , drop = FALSE]
    checkpoint_rows[[ck_idx]] <- data.frame(
      checkpoint = checkpoint_target,
      allocation_policy = allocation_policy,
      ts_mode = "batched",
      recommended_index = recommended$candidate_index[[1L]],
      recommended_move_label = recommended$move_label[[1L]],
      recommended_estimate = recommended$estimate[[1L]],
      recommended_prob_best = recommended$model_relative_prob_best[[1L]],
      recommended_expected_regret = recommended$model_relative_expected_regret[[1L]],
      recommended_allocation_count = recommended$allocation_count[[1L]],
      n_actions = nrow(action_table_ck),
      allocation_entropy = concentration$entropy,
      allocation_hhi = concentration$hhi,
      allocation_max_share = concentration$max_share,
      top_two_prob_best_mass = sum(sort(action_table_ck$model_relative_prob_best, decreasing = TRUE)[seq_len(min(2L, nrow(action_table_ck)))]),
      unresolved_fraction = if (sum(action_table_ck$allocation_count) > 0L) sum(action_table_ck$unresolved) / sum(action_table_ck$allocation_count) else NA_real_,
      runtime_seconds = proc.time()[[3L]] - runtime_start,
      rollout_throughput = if ((proc.time()[[3L]] - runtime_start) > 0) checkpoint_target / (proc.time()[[3L]] - runtime_start) else NA_real_,
      stringsAsFactors = FALSE
    )
    if (!is.null(reference)) {
      ref <- bg_reference_snapshot_public(reference)
      checkpoint_rows[[ck_idx]]$proxy_reference_best_index <- ref$best_index
      checkpoint_rows[[ck_idx]]$proxy_reference_best_move_label <- ref$label_lookup[[as.character(ref$best_index)]]
      checkpoint_rows[[ck_idx]]$simple_regret <- ref$best_value - ref$value_lookup[[as.character(recommended$candidate_index[[1L]])]]
      checkpoint_rows[[ck_idx]]$selected_reference_rank <- ref$rank_lookup[[as.character(recommended$candidate_index[[1L]])]]
      checkpoint_rows[[ck_idx]]$proxy_reference_match <- recommended$candidate_index[[1L]] == ref$best_index
    } else {
      checkpoint_rows[[ck_idx]]$proxy_reference_best_index <- NA_integer_
      checkpoint_rows[[ck_idx]]$proxy_reference_best_move_label <- NA_character_
      checkpoint_rows[[ck_idx]]$simple_regret <- NA_real_
      checkpoint_rows[[ck_idx]]$selected_reference_rank <- NA_integer_
      checkpoint_rows[[ck_idx]]$proxy_reference_match <- NA
    }
    action_table_ck$checkpoint <- checkpoint_target
    checkpoint_actions[[ck_idx]] <- action_table_ck
  }

  checkpoint_table <- do.call(rbind, checkpoint_rows)
  action_table <- bg_action_table_from_stats(
    problem = problem,
    stats_table = stats_table,
    allocation_policy = allocation_policy,
    reference = reference,
    seed = bg_derive_seed(seed, "batched-final-table")
  )
  warnings <- bg_collect_run_warnings(problem, action_table, checkpoint_table, reference = reference, ts_mode = "batched")

  bg_make_ts_run(
    problem = problem,
    allocation_policy = allocation_policy,
    budget = budget,
    ts_mode = "batched",
    action_table = action_table,
    checkpoint_table = checkpoint_table,
    checkpoint_actions = do.call(rbind, checkpoint_actions),
    reference = reference,
    legacy_evaluation = NULL,
    settings = list(
      budget = budget,
      checkpoints = checkpoints,
      seed = seed,
      batch_size = batch_size,
      dynamic_batching = isTRUE(dynamic_batching),
      task_block_size = task_block_size,
      dice_mode = dice_mode,
      crn = isTRUE(crn),
      reward_model = problem$settings$reward_model,
      posterior_model = problem$settings$posterior_model
    ),
    warnings = warnings
  )
}

#' Build a one-state, one-roll Thompson-sampling decision problem
#'
#' `bg_problem()` creates the canonical decision object used by the new
#' Thompson-first public API. It freezes:
#'
#' - one board state;
#' - one realized dice roll;
#' - one legal action set;
#' - one rollout continuation policy;
#' - one truncation and payoff mapping;
#' - one explicit reward model and posterior model.
#'
#' This makes the package's scientific object explicit: a fixed-budget
#' best-action-identification problem under Monte Carlo noise, not a claim about
#' exact backgammon truth.
#'
#' @param state A `bg_board` object.
#' @param roll A `bg_roll` object.
#' @param simulation_policy Continuation policy used after the root action
#'   inside rollouts. `"random"` is the benchmark default. `"heuristic"`
#'   resolves to a fixed heuristic continuation policy. `"ts_local"` is
#'   experimental because it changes the rollout-model estimand.
#' @param heuristic_policy Heuristic continuation policy used when
#'   `simulation_policy = "heuristic"`.
#' @param max_rollout_turns Integer-like rollout truncation horizon.
#' @param unresolved_value Numeric payoff assigned to unresolved rollouts.
#' @param prior_alpha Positive pseudo-count used by Thompson-family summaries.
#' @param prior_beta Positive pseudo-count used by Thompson-family summaries.
#' @param reward_model Reward definition used by the rollout engine.
#' @param posterior_model Posterior family used by Thompson-style summaries.
#' @param posterior_prior Optional named list overriding the default prior for
#'   the chosen `reward_model` / `posterior_model` pair.
#' @param legal_moves Optional precomputed legal move set.
#' @param cache Logical scalar; if `TRUE`, reuse a cached problem object when
#'   the board, roll, and rollout-model settings match.
#' @param problem_id Optional string identifier used in repeated studies.
#'
#' @return A `bg_problem` object.
#' @export
bg_problem <- function(
    state,
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
    legal_moves = NULL,
    cache = TRUE,
    problem_id = NULL) {
  if (!is_bg_board(state)) {
    stop("`state` must inherit from class 'bg_board'.", call. = FALSE)
  }
  bg_validate_board(state)
  roll <- bg_as_roll(roll)
  bg_assert_scalar_flag(cache, "cache")

  resolved_policy <- bg_resolve_simulation_policy(
    simulation_policy = simulation_policy,
    heuristic_policy = heuristic_policy
  )
  max_rollout_turns <- bg_coerce_integerish(max_rollout_turns, "max_rollout_turns", 1L)

  if (!is.numeric(unresolved_value) || length(unresolved_value) != 1L || is.na(unresolved_value)) {
    stop("`unresolved_value` must be a numeric scalar.", call. = FALSE)
  }
  if (unresolved_value < 0 || unresolved_value > 1) {
    stop("`unresolved_value` must lie in [0, 1].", call. = FALSE)
  }
  if (!is.numeric(prior_alpha) || length(prior_alpha) != 1L || is.na(prior_alpha) || prior_alpha <= 0) {
    stop("`prior_alpha` must be a positive numeric scalar.", call. = FALSE)
  }
  if (!is.numeric(prior_beta) || length(prior_beta) != 1L || is.na(prior_beta) || prior_beta <= 0) {
    stop("`prior_beta` must be a positive numeric scalar.", call. = FALSE)
  }
  model_spec <- bg_resolve_model_spec(
    reward_model = reward_model,
    posterior_model = posterior_model,
    unresolved_value = unresolved_value,
    prior_alpha = prior_alpha,
    prior_beta = prior_beta,
    posterior_prior = posterior_prior
  )

  key <- bg_problem_key(
    board = state,
    roll = roll,
    simulation_policy_engine = resolved_policy$simulation_policy_engine,
    max_rollout_turns = max_rollout_turns,
    unresolved_value = unresolved_value,
    reward_model = model_spec$reward_model_canonical,
    posterior_model = model_spec$posterior_model_canonical,
    model_signature = model_spec$model_signature
  )
  if (isTRUE(cache) && exists(key, envir = bg_ts_private$problem_cache, inherits = FALSE)) {
    cached <- get(key, envir = bg_ts_private$problem_cache, inherits = FALSE)
    if (!is.null(problem_id)) {
      cached$problem_id <- problem_id
    }
    return(cached)
  }

  legal_moves <- if (is.null(legal_moves)) {
    bg_legal_moves(state, roll)
  } else {
    lapply(bg_normalize_move_sequence_list(legal_moves), bg_new_move_sequence)
  }
  # Downstream TS code reasons over unique root-action candidates, not raw legal
  # move sequences that may collapse to the same successor state.
  candidate_table <- bg_problem_candidate_table(
    board = state,
    legal_moves = legal_moves,
    simulation_policy_engine = resolved_policy$simulation_policy_engine,
    max_rollout_turns = max_rollout_turns,
    unresolved_value = unresolved_value
  )

  out <- structure(
    list(
      board = state,
      roll = roll,
      legal_moves = legal_moves,
      candidate_table = candidate_table,
      problem_id = if (is.null(problem_id)) paste0("problem_", substr(key, 1L, 12L)) else as.character(problem_id),
      settings = list(
        simulation_policy = resolved_policy$simulation_policy,
        simulation_policy_engine = resolved_policy$simulation_policy_engine,
        simulation_policy_note = resolved_policy$note,
        simulation_policy_experimental = resolved_policy$experimental,
        max_rollout_turns = max_rollout_turns,
        unresolved_value = as.numeric(unresolved_value),
        prior_alpha = as.numeric(prior_alpha),
        prior_beta = as.numeric(prior_beta),
        reward_model = model_spec$reward_model,
        posterior_model = model_spec$posterior_model,
        reward_model_canonical = model_spec$reward_model_canonical,
        posterior_model_canonical = model_spec$posterior_model_canonical,
        posterior_family = model_spec$posterior_family,
        posterior_prior = model_spec$prior,
        model_signature = model_spec$model_signature,
        model_exact = isTRUE(model_spec$exact),
        model_note = model_spec$note,
        legacy_alias = isTRUE(model_spec$legacy_alias)
      )
    ),
    class = "bg_problem"
  )

  if (isTRUE(cache)) {
    assign(key, out, envir = bg_ts_private$problem_cache)
  }
  out
}

#' Solve one decision problem with Thompson sampling
#'
#' `bg_ts_decide()` is the main beginner entry point. It solves one
#' `bg_problem` under a fixed simulation budget and returns a coherent
#' Thompson-first object with:
#'
#' - final recommended action;
#' - candidate-level estimates and uncertainty;
#' - budget-path diagnostics at checkpoints;
#' - optional comparison to a high-budget proxy reference.
#'
#' By default the function uses canonical sequential Thompson sampling. Several
#' experimental Thompson-family variants are also available on the same scalar
#' rollout engine so they can be compared directly. Batched mode remains
#' available only for canonical TS and top-two TS.
#'
#' @param problem A `bg_problem` object.
#' @param budget Integer-like finite simulation budget.
#' @param allocation_policy Thompson-family allocation policy. `"thompson"` is
#'   the canonical default. `"top_two_thompson"` is the TTTS comparator. The
#'   current front door also supports `"multi_sample_thompson"`,
#'   `"tempered_thompson"`, `"budget_aware_thompson"`,
#'   `"elimination_thompson"`, and `"ranking_aware_thompson"` as experimental
#'   compare-ready variants on the same reward model.
#' @param proxy_reference Optional `bg_reference` object.
#' @param checkpoints Optional integer checkpoint vector. Defaults to a grid of
#'   very small budgets, powers of two, and the final budget.
#' @param ts_mode Either `"sequential"` or `"batched"`.
#' @param multi_sample_draws Integer-like number of posterior winner draws used
#'   by `"multi_sample_thompson"` and the exploratory phase of
#'   `"budget_aware_thompson"`.
#' @param temperature Positive scalar controlling posterior dispersion for
#'   `"tempered_thompson"` and the exploratory phase of
#'   `"budget_aware_thompson"`. Values above `1` make the posterior samples
#'   noisier; values below `1` make them more concentrated.
#' @param ranking_top_k Integer-like size of the near-top set used by
#'   `"ranking_aware_thompson"` and the late phase of
#'   `"budget_aware_thompson"`.
#' @param ranking_draws Integer-like number of posterior draws used by
#'   ranking-aware and top-two style decision rules inside the experimental
#'   variants.
#' @param elimination_min_allocations Integer-like minimum allocations per move
#'   before `"elimination_thompson"` starts screening clearly dominated moves.
#' @param elimination_keep_top Integer-like number of moves always protected
#'   from elimination.
#' @param elimination_margin Nonnegative screening margin for
#'   `"elimination_thompson"`. Larger values make elimination more conservative.
#' @param batch_size Integer-like batch size for experimental batched mode.
#' @param dynamic_batching Logical scalar; if `TRUE`, allows smaller early
#'   batches before growing toward `batch_size`.
#' @param task_block_size Integer-like simulation block size used by the
#'   batched block simulator.
#' @param dice_mode Variance mode for rollout dice generation. Non-`"iid"`
#'   settings are optional variance-reduced evaluation modes.
#' @param crn Logical scalar indicating common-random-number use.
#' @param seed Optional integer-like seed.
#'
#' @return A `bg_ts_run` object.
#' @keywords internal
#' @noRd
bg_ts_decide <- function(
    problem,
    budget = 256L,
    allocation_policy = c(
      "thompson",
      "top_two_thompson",
      "multi_sample_thompson",
      "tempered_thompson",
      "budget_aware_thompson",
      "elimination_thompson",
      "ranking_aware_thompson"
    ),
    proxy_reference = NULL,
    checkpoints = NULL,
    ts_mode = c("sequential", "batched"),
    ttts_beta = 0.5,
    multi_sample_draws = 5L,
    temperature = 1.25,
    ranking_top_k = 3L,
    ranking_draws = 128L,
    elimination_min_allocations = 4L,
    elimination_keep_top = 2L,
    elimination_margin = 0,
    batch_size = 16L,
    dynamic_batching = FALSE,
    task_block_size = 64L,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    seed = NULL) {
  if (!inherits(problem, "bg_problem")) {
    stop("`problem` must inherit from class 'bg_problem'.", call. = FALSE)
  }
  if (!is.null(proxy_reference) && !inherits(proxy_reference, "bg_reference")) {
    stop("`proxy_reference` must inherit from class 'bg_reference' when supplied.", call. = FALSE)
  }

  budget <- bg_coerce_integerish(budget, "budget", 1L)
  allocation_policy <- bg_match_allocation_policy_public(allocation_policy)
  ts_mode <- match.arg(ts_mode)
  checkpoints <- bg_checkpoint_grid(budget, checkpoints)
  dice_mode <- match.arg(dice_mode)
  bg_assert_scalar_flag(crn, "crn")
  bg_assert_scalar_flag(dynamic_batching, "dynamic_batching")

  if (!bg_is_thompson_policy_public(allocation_policy)) {
    stop("`bg_ts_decide()` is reserved for Thompson-family allocation policies; use `bg_compare_methods()` for baseline comparisons.", call. = FALSE)
  }

  if (ts_mode == "sequential" &&
      allocation_policy %in% c("thompson", "top_two_thompson") &&
      bg_problem_uses_fast_ts_path(problem)) {
    # Use the legacy scalar engine only when the problem really matches that
    # model stack; otherwise keep everything on the explicit-posterior path.
    return(bg_run_method_path(
      problem = problem,
      allocation_policy = allocation_policy,
      budget = budget,
      checkpoints = checkpoints,
      reference = proxy_reference,
      seed = seed,
      dice_mode = dice_mode,
      crn = crn,
      fast_diagnostics = FALSE,
      ttts_beta = ttts_beta
    ))
  }

  if (ts_mode == "sequential") {
    # Richer reward/posterior stacks and experimental TS variants all come
    # through the explicit-posterior implementation.
    return(bg_run_posterior_ts(
      problem = problem,
      allocation_policy = allocation_policy,
      budget = budget,
      checkpoints = checkpoints,
      reference = proxy_reference,
      seed = seed,
      task_block_size = task_block_size,
      dice_mode = dice_mode,
      crn = crn,
      multi_sample_draws = multi_sample_draws,
      temperature = temperature,
      ranking_top_k = ranking_top_k,
      ranking_draws = ranking_draws,
      elimination_min_allocations = elimination_min_allocations,
      elimination_keep_top = elimination_keep_top,
      elimination_margin = elimination_margin,
      ttts_beta = ttts_beta
    ))
  }

  if (!bg_problem_uses_fast_ts_path(problem)) {
    stop(
      "Batched mode is currently available only for the legacy/default fast path ",
      "(`scalar_payoff + beta_pseudo`).",
      call. = FALSE
    )
  }

  if (!allocation_policy %in% c("thompson", "top_two_thompson")) {
    stop(
      "Experimental Thompson variants currently support only `ts_mode = 'sequential'`; ",
      "batched mode remains reserved for canonical TS and top-two TS.",
      call. = FALSE
    )
  }

  bg_run_batched_ts(
    problem = problem,
    allocation_policy = allocation_policy,
    budget = budget,
    checkpoints = checkpoints,
    reference = proxy_reference,
    seed = seed,
    batch_size = batch_size,
    dynamic_batching = dynamic_batching,
    task_block_size = task_block_size,
    dice_mode = dice_mode,
    crn = crn,
    ttts_beta = ttts_beta
  )
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
    # Carry full sufficient statistics forward so extending a reference never
    # drops scored outcome counts or reward totals.
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
  add_budget <- budget - previous_budget

  if (reference_mode == "equal") {
    # Equal mode is the package's default proxy-reference construction because
    # its Monte Carlo interpretation is the cleanest.
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
    # Focused mode is explicitly approximate: screen broadly with an equal pilot,
    # then spend the remaining budget on a small plausible top set.
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

#' Compute interpretable board and action features
#'
#' `bg_board_features()` exposes a compact, interpretable feature view of a
#' board or `bg_problem`. These features are intended for structured
#' experiments, difficulty analysis, and move-by-move studies, not as claims of
#' exact backgammon truth.
#'
#' @param x A `bg_board` or `bg_problem` object.
#' @param player Optional player viewpoint. Defaults to the player to move.
#'
#' @return A list with `board_features` and, when available, `action_features`.
#' @export
bg_board_features <- function(x, player = NULL) {
  if (inherits(x, "bg_problem")) {
    board <- x$board
    if (is.null(player)) {
      player <- board$turn
    }
    board_features <- as.data.frame(bg_cpp_heuristic_board_features(unclass(board), player), stringsAsFactors = FALSE)
    action_features <- x$candidate_table[, c(
      "candidate_index",
      "move_label",
      "n_steps",
      "n_hits",
      "n_bar_entries",
      "n_bear_off",
      "total_step_distance",
      "n_equivalent_sequences"
    ), drop = FALSE]
    return(list(
      board_features = board_features,
      action_features = action_features
    ))
  }

  if (!is_bg_board(x)) {
    stop("`x` must inherit from class 'bg_board' or 'bg_problem'.", call. = FALSE)
  }

  if (is.null(player)) {
    player <- x$turn
  }

  list(
    board_features = as.data.frame(bg_cpp_heuristic_board_features(unclass(x), player), stringsAsFactors = FALSE),
    action_features = NULL
  )
}

# Thompson-family algorithms, posterior models, and shared run infrastructure.
#
# This file keeps the statistical control layer together: model resolution,
# posterior updates/draws, policy selection rules, and the public algorithm
# entry points.

# -----------------------------------------------------------------------------
# Source: bg_posteriors.R
# -----------------------------------------------------------------------------
# Posterior-model helpers and explicit-posterior kernels.
#
# This file does two related jobs:
# - route explicit-posterior TS workflows through the C++ posterior kernels; and
# - validate reward-model / posterior-model combinations so the package keeps a
#   coherent estimand story.
#
# The legacy scalar shortcut is handled as one explicit special case. All other
# research-facing posterior logic routes through the explicit kernels below.
bg_problem_uses_fast_ts_path <- function(problem) {
  inherits(problem, "bg_problem") &&
    identical(problem$settings$reward_model_canonical, "scalar_payoff") &&
    identical(problem$settings$posterior_model_canonical, "beta_pseudo")
}

# Comparator restrictions and front-door routing both depend on the same fast
# path eligibility rule.
bg_problem_supports_legacy_scalar_engine <- function(problem) {
  bg_problem_uses_fast_ts_path(problem)
}

bg_reference_interval_type <- function() {
  "mc_normal_approx"
}

bg_gap_interval_type <- function() {
  "mc_gap_normal_approx"
}

bg_posterior_interval_type <- function(problem) {
  posterior_model <- problem$settings$posterior_model_canonical
  switch(
    posterior_model,
    beta_bernoulli = "beta_quantile",
    beta_pseudo = "pseudo_beta_quantile",
    dirichlet_multinomial = "posterior_draw_quantile",
    gaussian_approx = "posterior_draw_quantile",
    normal_inverse_gamma = "posterior_draw_quantile",
    student_t_marginal = "posterior_draw_quantile",
    bootstrap = "posterior_draw_quantile",
    "posterior_draw_quantile"
  )
}

bg_problem_clone_models <- function(
    problem,
    reward_model = problem$settings$reward_model,
    posterior_model = problem$settings$posterior_model,
    posterior_prior = NULL,
    unresolved_value = problem$settings$unresolved_value,
    problem_id = problem$problem_id,
    cache = FALSE) {
  if (!inherits(problem, "bg_problem")) {
    stop("`problem` must inherit from class 'bg_problem'.", call. = FALSE)
  }

  bg_problem(
    state = problem$board,
    roll = problem$roll,
    simulation_policy = problem$settings$simulation_policy,
    heuristic_policy = if (identical(problem$settings$simulation_policy_engine, "defensive")) "defensive" else "aggressive",
    max_rollout_turns = problem$settings$max_rollout_turns,
    unresolved_value = unresolved_value,
    prior_alpha = problem$settings$prior_alpha,
    prior_beta = problem$settings$prior_beta,
    reward_model = reward_model,
    posterior_model = posterior_model,
    posterior_prior = posterior_prior,
    legal_moves = problem$legal_moves,
    cache = cache,
    problem_id = problem_id
  )
}

bg_posterior_kernel_inputs <- function(stats_table) {
  # Older and newer stats tables do not always carry exactly the same columns,
  # so pad missing sufficient statistics with zeros before calling C++.
  int_col <- function(name) {
    if (name %in% names(stats_table)) {
      return(as.integer(stats_table[[name]]))
    }
    rep.int(0L, nrow(stats_table))
  }

  list(
    allocation_count = as.integer(stats_table$allocation_count),
    wins = as.integer(stats_table$wins),
    losses = as.integer(stats_table$losses),
    single_loss = int_col("single_loss"),
    gammon_loss = int_col("gammon_loss"),
    backgammon_loss = int_col("backgammon_loss"),
    unresolved = int_col("unresolved"),
    single_win = int_col("single_win"),
    gammon_win = int_col("gammon_win"),
    backgammon_win = int_col("backgammon_win"),
    reward_sum = as.numeric(stats_table$reward_sum),
    reward_sum_sq = as.numeric(stats_table$reward_sum_sq)
  )
}

# Draw a posterior sample matrix once, then let the policy layer decide how to
# turn that matrix into the next allocation.
bg_posterior_draw_matrix <- function(problem, stats_table, draws = 1L, seed = NULL) {
  if (!inherits(problem, "bg_problem")) {
    stop("`problem` must inherit from class 'bg_problem'.", call. = FALSE)
  }
  draws <- bg_coerce_integerish(draws, "draws", 1L)
  inputs <- bg_posterior_kernel_inputs(stats_table)
  bg_ts_with_seed(
    seed,
    bg_cpp_posterior_sample_values(
      allocation_count = inputs$allocation_count,
      wins = inputs$wins,
      losses = inputs$losses,
      single_loss = inputs$single_loss,
      gammon_loss = inputs$gammon_loss,
      backgammon_loss = inputs$backgammon_loss,
      unresolved = inputs$unresolved,
      single_win = inputs$single_win,
      gammon_win = inputs$gammon_win,
      backgammon_win = inputs$backgammon_win,
      reward_sum = inputs$reward_sum,
      reward_sum_sq = inputs$reward_sum_sq,
      reward_model = problem$settings$reward_model_canonical,
      posterior_model = problem$settings$posterior_model_canonical,
      unresolved_value = problem$settings$unresolved_value,
      posterior_prior = problem$settings$posterior_prior,
      draws = draws
    )
  )
}

# Posterior summaries are computed in C++ and returned in one canonical table
# regardless of the posterior family underneath.
bg_posterior_summary_from_stats <- function(problem, stats_table, draws = 256L, seed = NULL) {
  if (!inherits(problem, "bg_problem")) {
    stop("`problem` must inherit from class 'bg_problem'.", call. = FALSE)
  }
  draws <- bg_coerce_integerish(draws, "draws", 1L)
  inputs <- bg_posterior_kernel_inputs(stats_table)
  out <- bg_ts_with_seed(
    seed,
    bg_cpp_posterior_summary(
      allocation_count = inputs$allocation_count,
      wins = inputs$wins,
      losses = inputs$losses,
      single_loss = inputs$single_loss,
      gammon_loss = inputs$gammon_loss,
      backgammon_loss = inputs$backgammon_loss,
      unresolved = inputs$unresolved,
      single_win = inputs$single_win,
      gammon_win = inputs$gammon_win,
      backgammon_win = inputs$backgammon_win,
      reward_sum = inputs$reward_sum,
      reward_sum_sq = inputs$reward_sum_sq,
      reward_model = problem$settings$reward_model_canonical,
      posterior_model = problem$settings$posterior_model_canonical,
      unresolved_value = problem$settings$unresolved_value,
      posterior_prior = problem$settings$posterior_prior,
      draws = draws
    )
  )
  as.data.frame(out, stringsAsFactors = FALSE)
}

bg_posterior_action_table_from_stats <- function(
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

  # Attach one coherent posterior summary block so every run object exposes the
  # same estimate / interval / regret / probability-best columns.
  posterior <- bg_posterior_summary_from_stats(
    problem = problem,
    stats_table = tab,
    draws = prob_best_draws,
    seed = seed
  )
  tab$alpha <- posterior$alpha
  tab$beta <- posterior$beta
  tab$estimate <- posterior$estimate
  tab$posterior_sd <- posterior$posterior_sd
  tab$lower_95 <- posterior$lower_95
  tab$upper_95 <- posterior$upper_95
  tab$posterior_interval_type <- bg_posterior_interval_type(problem)
  tab$model_relative_prob_best <- posterior$model_relative_prob_best
  tab$model_relative_expected_regret <- posterior$model_relative_expected_regret
  tab$sample_variance <- ifelse(
    tab$allocation_count > 1L,
    pmax((tab$reward_sum_sq - (tab$reward_sum^2 / tab$allocation_count)) / (tab$allocation_count - 1L), 0),
    NA_real_
  )
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

bg_draw_matrix_summary <- function(draw_mat) {
  if (is.null(dim(draw_mat))) {
    draw_mat <- matrix(draw_mat, ncol = 1L)
  }
  means <- colMeans(draw_mat)
  sds <- if (nrow(draw_mat) > 1L) {
    apply(draw_mat, 2L, stats::sd)
  } else {
    rep(0, ncol(draw_mat))
  }
  list(mean = as.numeric(means), sd = as.numeric(sds))
}

bg_posterior_pick_index <- function(scores, allocation_count, tie_break = NULL) {
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

# Tempered Thompson changes dispersion around the posterior mean rather than the
# mean itself.
bg_temper_draw_matrix <- function(draw_mat, posterior_mean, temperature = 1) {
  if (!is.numeric(temperature) || length(temperature) != 1L || is.na(temperature) || temperature <= 0) {
    stop("`temperature` must be a positive numeric scalar.", call. = FALSE)
  }
  if (identical(temperature, 1)) {
    return(draw_mat)
  }

  scale <- sqrt(temperature)
  centered <- sweep(draw_mat, 2L, posterior_mean, "-")
  out <- sweep(centered * scale, 2L, posterior_mean, "+")
  pmin(pmax(out, 0), 1)
}

# TTTS is implemented as repeated posterior-winner draws until a distinct
# challenger appears, with a deterministic fallback if repeated draws tie.
bg_posterior_top_two_choice <- function(draw_mat, allocation_count, ttts_beta = 0.5) {
  winner_once <- function() {
    bg_posterior_pick_index(
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
  bg_posterior_pick_index(
    scores = posterior_mean,
    allocation_count = allocation_count,
    tie_break = posterior_mean
  )
}

# Ranking-aware TS focuses on uncertainty inside the near-top set instead of
# only the current leader.
bg_posterior_ranking_choice <- function(draw_mat, posterior_mean, allocation_count, focus_top_k = 3L) {
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

  bg_posterior_pick_index(
    scores = uncertainty,
    allocation_count = allocation_count,
    tie_break = posterior_mean
  )
}

# Elimination only screens once every action has at least a minimum amount of
# evidence, and it always protects a small leader set from removal.
bg_posterior_update_active_set <- function(
    active,
    posterior_mean,
    posterior_sd,
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
  if (length(active_idx) <= keep_top || any(allocation_count[active_idx] < min_allocations)) {
    return(active)
  }

  lower_95 <- pmax(posterior_mean - 1.96 * posterior_sd, 0)
  upper_95 <- pmin(posterior_mean + 1.96 * posterior_sd, 1)
  leader_lower <- max(lower_95[active_idx], na.rm = TRUE)
  keep_idx <- active_idx[order(posterior_mean[active_idx], decreasing = TRUE)][seq_len(min(keep_top, length(active_idx)))]
  eliminated <- setdiff(active_idx[upper_95[active_idx] + margin < leader_lower], keep_idx)
  if (length(eliminated) > 0L) {
    active[eliminated] <- FALSE
  }
  active
}

# Different TS variants need different draw budgets. Keep that rule centralized
# so the sequential TS loop can stay simple.
bg_posterior_selection_draws <- function(
    allocation_policy,
    spent,
    budget,
    multi_sample_draws,
    ranking_draws) {
  switch(
    allocation_policy,
    thompson = 1L,
    multi_sample_thompson = max(2L, multi_sample_draws),
    soft_elimination_thompson = max(64L, ranking_draws),
    top_two_thompson = max(64L, ranking_draws),
    forced_exploration_thompson = 1L,
    top_k_thompson = max(64L, ranking_draws),
    max(64L, ranking_draws)
  )
}

# Convert one posterior draw matrix into one selected candidate under the chosen
# TS-family policy. This is the main policy switch for the explicit engine.
bg_posterior_select_candidate <- function(
    allocation_policy,
    draw_mat,
    posterior_mean,
    posterior_sd,
    allocation_count,
    spent,
    budget,
    active,
    multi_sample_draws = 5L,
    temperature = 1.25,
    ttts_beta = 0.5,
    ranking_top_k = 3L,
    forced_every = 8L,
    forced_min_allocations = 2L) {
  allocation_policy <- bg_match_allocation_policy_public(allocation_policy)
  active_idx <- which(active)
  if (length(active_idx) < 1L) {
    stop("No active actions remain.", call. = FALSE)
  }
  if (length(active_idx) == 1L) {
    return(active_idx[[1L]])
  }

  local_draws <- draw_mat[, active_idx, drop = FALSE]
  local_mean <- posterior_mean[active_idx]
  local_sd <- posterior_sd[active_idx]
  local_count <- allocation_count[active_idx]

  if (identical(allocation_policy, "forced_exploration_thompson")) {
    forced_every <- bg_coerce_integerish(forced_every, "forced_every", 1L)
    forced_min_allocations <- bg_coerce_integerish(forced_min_allocations, "forced_min_allocations", 1L)

    if (any(local_count < forced_min_allocations) ||
        ((spent + 1L) %% max(1L, forced_every) == 0L)) {
      return(active_idx[[which.min(local_count)]])
    }
  }

  local_choice <- switch(
    allocation_policy,
    thompson = {
      bg_posterior_pick_index(local_draws[1L, ], local_count, tie_break = local_mean)
    },
    multi_sample_thompson = {
      bg_posterior_pick_index(colMeans(local_draws), local_count, tie_break = local_mean)
    },
    top_two_thompson = {
      bg_posterior_top_two_choice(local_draws, local_count, ttts_beta = ttts_beta)
    },
    forced_exploration_thompson = {
      bg_posterior_pick_index(local_draws[1L, ], local_count, tie_break = local_mean)
    },
    top_k_thompson = {
      bg_posterior_ranking_choice(
        draw_mat = local_draws,
        posterior_mean = local_mean,
        allocation_count = local_count,
        focus_top_k = ranking_top_k
      )
    },
    soft_elimination_thompson = {
      bg_posterior_pick_index(local_draws[1L, ], local_count, tie_break = local_mean)
    },
    stop("Unsupported posterior Thompson variant.", call. = FALSE)
  )

  active_idx[[local_choice]]
}

bg_run_posterior_ts <- function(
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
    forced_every = 8L,
    forced_min_allocations = 2L,
    ttts_beta = 0.5,
    prob_best_draws = 256L) {
  allocation_policy <- bg_match_allocation_policy_public(allocation_policy)
  dice_mode <- match.arg(dice_mode)
  task_block_size <- bg_coerce_integerish(task_block_size, "task_block_size", 1L)
  multi_sample_draws <- bg_coerce_integerish(multi_sample_draws, "multi_sample_draws", 1L)
  ranking_top_k <- bg_coerce_integerish(ranking_top_k, "ranking_top_k", 1L)
  ranking_draws <- bg_coerce_integerish(ranking_draws, "ranking_draws", 1L)
  prob_best_draws <- bg_coerce_integerish(prob_best_draws, "prob_best_draws", 1L)

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
        reward_model = problem$settings$reward_model,
        posterior_model = problem$settings$posterior_model,
        engine_path = "explicit_posterior_engine"
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
        selection_draws <- bg_posterior_selection_draws(
          allocation_policy = allocation_policy,
          spent = spent,
          budget = budget,
          multi_sample_draws = multi_sample_draws,
          ranking_draws = ranking_draws
        )
        draw_mat <- bg_posterior_draw_matrix(
          problem = problem,
          stats_table = stats_table,
          draws = selection_draws,
          seed = bg_derive_seed(
            seed,
            "posterior-select",
            problem$settings$reward_model_canonical,
            problem$settings$posterior_model_canonical,
            allocation_policy,
            spent + 1L
          )
        )
        draw_summary <- bg_draw_matrix_summary(draw_mat)

        if (identical(allocation_policy, "soft_elimination_thompson")) {
          updated <- bg_posterior_update_active_set(
            active = active,
            posterior_mean = draw_summary$mean,
            posterior_sd = draw_summary$sd,
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
          if (sum(active) < 1L) {
            active[] <- TRUE
          }
        }

        chosen_pos <- bg_posterior_select_candidate(
          allocation_policy = allocation_policy,
          draw_mat = draw_mat,
          posterior_mean = draw_summary$mean,
          posterior_sd = draw_summary$sd,
          allocation_count = stats_table$allocation_count,
          spent = spent,
          budget = budget,
          active = active,
          multi_sample_draws = multi_sample_draws,
          temperature = temperature,
          ttts_beta = ttts_beta,
          ranking_top_k = ranking_top_k,
          forced_every = forced_every,
          forced_min_allocations = forced_min_allocations
        )

        block_results <- bg_call_rollout_blocks(
          problem = problem,
          candidate_index = candidate_table$candidate_index[chosen_pos],
          block_rollouts = 1L,
          start_counts = stats_table$allocation_count[chosen_pos],
          task_block_size = task_block_size,
          dice_mode = dice_mode,
          crn = crn,
          seed = bg_derive_seed(
            seed,
            "posterior-rollout",
            problem$settings$reward_model_canonical,
            problem$settings$posterior_model_canonical,
            allocation_policy,
            spent + 1L
          )
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

      action_table_ck <- bg_posterior_action_table_from_stats(
        problem = problem,
        stats_table = stats_table,
        allocation_policy = allocation_policy,
        reference = reference,
        prob_best_draws = prob_best_draws,
        seed = bg_derive_seed(
          seed,
          "posterior-checkpoint",
          problem$settings$reward_model_canonical,
          problem$settings$posterior_model_canonical,
          allocation_policy,
          checkpoint_target
        )
      )
      action_match <- match(action_table_ck$candidate_index, candidate_table$candidate_index)
      action_table_ck$eligible <- active[action_match]
      action_table_ck$eliminated_at <- eliminated_at[action_match]
      if (identical(allocation_policy, "soft_elimination_thompson")) {
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
  action_table <- bg_posterior_action_table_from_stats(
    problem = problem,
    stats_table = stats_table,
    allocation_policy = allocation_policy,
    reference = reference,
    prob_best_draws = prob_best_draws,
    seed = bg_derive_seed(
      seed,
      "posterior-final-table",
      problem$settings$reward_model_canonical,
      problem$settings$posterior_model_canonical,
      allocation_policy
    )
  )
  action_match <- match(action_table$candidate_index, candidate_table$candidate_index)
  action_table$eligible <- active[action_match]
  action_table$eliminated_at <- eliminated_at[action_match]
  if (identical(allocation_policy, "soft_elimination_thompson")) {
    action_table <- bg_mark_recommended_from_eligibility(action_table)
  }

  warnings <- bg_collect_run_warnings(problem, action_table, checkpoint_table, reference = reference, ts_mode = "sequential")
  warnings <- unique(c(
    warnings,
    sprintf(
      "%s is running on the explicit `%s + %s` posterior stack.",
      bg_allocation_policy_label(allocation_policy),
      problem$settings$reward_model,
      problem$settings$posterior_model
    )
  ))
  if (!isTRUE(problem$settings$model_exact)) {
    warnings <- unique(c(warnings, problem$settings$model_note))
  }

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
      prob_best_draws = prob_best_draws,
      reward_model = problem$settings$reward_model,
      posterior_model = problem$settings$posterior_model,
      reward_model_canonical = problem$settings$reward_model_canonical,
      posterior_model_canonical = problem$settings$posterior_model_canonical,
      engine_path = "explicit_posterior_engine"
    ),
    warnings = warnings
  )
}

# -----------------------------------------------------------------------------
# Reward-model and posterior-model validation
# -----------------------------------------------------------------------------

bg_match_reward_model_public <- function(reward_model) {
  # Canonicalize the user-facing reward-model label and reject the removed
  # legacy alias before it leaks into the rest of the package.
  if (length(reward_model) > 1L) {
    reward_model <- reward_model[[1L]]
  }
  if (identical(reward_model, "win_indicator")) {
    stop(
      "`reward_model = 'win_indicator'` has been removed. Choose one of ",
      "`'win_loss'`, `'scalar_payoff'`, or `'categorical_outcome'` explicitly.",
      call. = FALSE
    )
  }
  match.arg(
    reward_model,
    choices = c(
      "win_loss",
      "categorical_outcome",
      "scalar_payoff"
    )
  )
}

# Keep the full supported posterior family list centralized even though only a
# smaller subset is presentation-central.
bg_match_posterior_model_public <- function(posterior_model) {
  # Canonicalize the posterior-family label once so downstream workflows do not
  # repeat match.arg() logic.
  if (length(posterior_model) > 1L) {
    posterior_model <- posterior_model[[1L]]
  }
  match.arg(
    posterior_model,
    choices = c(
      "beta_bernoulli",
      "beta_pseudo",
      "dirichlet_multinomial",
      "gaussian_approx",
      "normal_inverse_gamma",
      "student_t_marginal",
      "bootstrap"
    )
  )
}

# Supported models are all valid combinations; recommended models are the ones
# the package is willing to foreground in docs and examples.
bg_supported_posterior_models <- function(reward_model) {
  # Return every posterior family allowed for the chosen reward type.
  reward_model <- bg_match_reward_model_public(reward_model)
  switch(
    reward_model,
    win_loss = c("beta_bernoulli", "gaussian_approx", "bootstrap"),
    categorical_outcome = c("dirichlet_multinomial", "bootstrap"),
    scalar_payoff = c("beta_pseudo", "gaussian_approx", "normal_inverse_gamma", "student_t_marginal", "bootstrap")
  )
}

bg_recommended_posterior_models <- function(reward_model) {
  # Return the smaller set of model families the package treats as central.
  reward_model <- bg_match_reward_model_public(reward_model)
  switch(
    reward_model,
    win_loss = c("beta_bernoulli", "bootstrap"),
    categorical_outcome = c("dirichlet_multinomial", "bootstrap"),
    scalar_payoff = c("beta_pseudo", "student_t_marginal", "bootstrap")
  )
}

bg_validate_named_numeric_scalar <- function(x, name, lower = -Inf, open_lower = FALSE) {
  # Shared scalar validator for prior hyperparameters.
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    stop(sprintf("`%s` must be a finite numeric scalar.", name), call. = FALSE)
  }
  if (open_lower) {
    if (!(x > lower)) {
      stop(sprintf("`%s` must be greater than %s.", name, lower), call. = FALSE)
    }
  } else if (x < lower) {
    stop(sprintf("`%s` must be at least %s.", name, lower), call. = FALSE)
  }
  x
}

bg_validate_probability_triplet <- function(x, name) {
  # Validate three-category probability vectors used by collapsed categorical
  # outcome models.
  if (!is.numeric(x) || length(x) != 3L || anyNA(x) || any(!is.finite(x)) || any(x <= 0)) {
    stop(sprintf("`%s` must be a positive numeric vector of length 3.", name), call. = FALSE)
  }
  as.numeric(x)
}

bg_validate_positive_numeric_vector <- function(x, name, expected_lengths = NULL) {
  # Validate positive alpha/payoff vectors with optional length constraints.
  if (!is.numeric(x) || anyNA(x) || any(!is.finite(x)) || any(x <= 0)) {
    stop(sprintf("`%s` must be a positive numeric vector.", name), call. = FALSE)
  }
  if (!is.null(expected_lengths) && !length(x) %in% expected_lengths) {
    stop(
      sprintf(
        "`%s` must have length %s.",
        name,
        paste(expected_lengths, collapse = " or ")
      ),
      call. = FALSE
    )
  }
  as.numeric(x)
}

bg_scored_outcome_names <- function() {
  # Canonical seven-category scored-outcome labels used by the Dirichlet path.
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

bg_default_scored_payoff_map <- function(unresolved_value) {
  # Default mapping from scored categorical outcomes onto the package's bounded
  # scalar reward scale.
  stats::setNames(
    c(1 / 3, 1 / 6, 0, unresolved_value, 2 / 3, 5 / 6, 1),
    bg_scored_outcome_names()
  )
}

# Resolve one coherent prior object for the chosen reward/posterior stack, then
# validate any user overrides before the problem is constructed.
bg_resolve_posterior_prior <- function(
    reward_model_canonical,
    posterior_model_canonical,
    unresolved_value,
    prior_alpha = 1,
    prior_beta = 1,
    posterior_prior = NULL) {
  # Build one validated, stack-specific prior object for downstream Thompson
  # and posterior-summary code.
  prior_alpha <- bg_validate_named_numeric_scalar(prior_alpha, "prior_alpha", lower = 0, open_lower = TRUE)
  prior_beta <- bg_validate_named_numeric_scalar(prior_beta, "prior_beta", lower = 0, open_lower = TRUE)

  resolved <- switch(
    posterior_model_canonical,
    beta_bernoulli = list(alpha = prior_alpha, beta = prior_beta),
    beta_pseudo = list(alpha = prior_alpha, beta = prior_beta),
    dirichlet_multinomial = list(
      alpha = stats::setNames(rep(1, 7L), bg_scored_outcome_names()),
      payoff = bg_default_scored_payoff_map(unresolved_value)
    ),
    gaussian_approx = list(mean = 0.5, weight = 1, variance_floor = 0.125),
    normal_inverse_gamma = list(mean = 0.5, kappa = 1, shape = 2.5, scale = 0.125),
    student_t_marginal = list(mean = 0.5, kappa = 1, shape = 2.5, scale = 0.125),
    bootstrap = list(smoothing = 0)
  )

  if (!is.null(posterior_prior)) {
    if (!is.list(posterior_prior)) {
      stop("`posterior_prior` must be NULL or a named list.", call. = FALSE)
    }
    resolved[names(posterior_prior)] <- posterior_prior
  }

  if (posterior_model_canonical %in% c("beta_bernoulli", "beta_pseudo")) {
    resolved$alpha <- bg_validate_named_numeric_scalar(resolved$alpha, "posterior_prior$alpha", lower = 0, open_lower = TRUE)
    resolved$beta <- bg_validate_named_numeric_scalar(resolved$beta, "posterior_prior$beta", lower = 0, open_lower = TRUE)
  } else if (posterior_model_canonical == "dirichlet_multinomial") {
    resolved$alpha <- bg_validate_positive_numeric_vector(
      resolved$alpha,
      "posterior_prior$alpha",
      expected_lengths = c(3L, 7L)
    )
    if (length(resolved$alpha) == 3L) {
      names(resolved$alpha) <- c("loss", "unresolved", "win")
    } else {
      names(resolved$alpha) <- bg_scored_outcome_names()
    }
    if (is.null(resolved$payoff)) {
      resolved$payoff <- if (length(resolved$alpha) == 3L) {
        c(loss = 0, unresolved = unresolved_value, win = 1)
      } else {
        bg_default_scored_payoff_map(unresolved_value)
      }
    }
    resolved$payoff <- as.numeric(resolved$payoff)
    if (!length(resolved$payoff) %in% c(3L, 7L)) {
      stop("`posterior_prior$payoff` must have length 3 or 7 for Dirichlet-multinomial models.", call. = FALSE)
    }
    if (anyNA(resolved$payoff) || any(!is.finite(resolved$payoff)) || any(resolved$payoff < 0) || any(resolved$payoff > 1)) {
      stop("`posterior_prior$payoff` must contain finite values in [0, 1].", call. = FALSE)
    }
    if (length(resolved$payoff) != length(resolved$alpha)) {
      stop("`posterior_prior$alpha` and `posterior_prior$payoff` must have the same length.", call. = FALSE)
    }
    if (length(resolved$payoff) == 3L) {
      names(resolved$payoff) <- c("loss", "unresolved", "win")
    } else {
      names(resolved$payoff) <- bg_scored_outcome_names()
    }
  } else if (posterior_model_canonical == "gaussian_approx") {
    resolved$mean <- bg_validate_named_numeric_scalar(resolved$mean, "posterior_prior$mean", lower = 0)
    if (resolved$mean > 1) {
      stop("`posterior_prior$mean` must lie in [0, 1].", call. = FALSE)
    }
    resolved$weight <- bg_validate_named_numeric_scalar(resolved$weight, "posterior_prior$weight", lower = 0, open_lower = TRUE)
    resolved$variance_floor <- bg_validate_named_numeric_scalar(resolved$variance_floor, "posterior_prior$variance_floor", lower = 0, open_lower = TRUE)
  } else if (posterior_model_canonical %in% c("normal_inverse_gamma", "student_t_marginal")) {
    resolved$mean <- bg_validate_named_numeric_scalar(resolved$mean, "posterior_prior$mean", lower = 0)
    if (resolved$mean > 1) {
      stop("`posterior_prior$mean` must lie in [0, 1].", call. = FALSE)
    }
    resolved$kappa <- bg_validate_named_numeric_scalar(resolved$kappa, "posterior_prior$kappa", lower = 0, open_lower = TRUE)
    resolved$shape <- bg_validate_named_numeric_scalar(resolved$shape, "posterior_prior$shape", lower = 0, open_lower = TRUE)
    resolved$scale <- bg_validate_named_numeric_scalar(resolved$scale, "posterior_prior$scale", lower = 0, open_lower = TRUE)
  } else if (posterior_model_canonical == "bootstrap") {
    resolved$smoothing <- bg_validate_named_numeric_scalar(resolved$smoothing, "posterior_prior$smoothing", lower = 0)
  }

  resolved$reward_model_canonical <- reward_model_canonical
  resolved$posterior_model_canonical <- posterior_model_canonical
  resolved$unresolved_value <- unresolved_value
  resolved
}

bg_model_spec_signature <- function(prior) {
  # Serialize the resolved prior into a stable signature string for caching and
  # reproducibility metadata.
  pieces <- unlist(
    lapply(
      sort(names(prior)),
      function(name) {
        value <- prior[[name]]
        if (is.numeric(value)) {
          value <- format(as.numeric(value), scientific = FALSE, trim = TRUE)
        }
        paste(name, paste(value, collapse = ","), sep = "=")
      }
    ),
    use.names = FALSE
  )
  paste(pieces, collapse = ";")
}

# Build one canonical model-spec object so later code can rely on:
# - requested labels for display,
# - canonical labels for routing,
# - exact/approximate status,
# - and one validated prior list.
bg_resolve_model_spec <- function(
    reward_model = c("scalar_payoff"),
    posterior_model = c("beta_pseudo"),
    unresolved_value = 0.5,
    prior_alpha = 1,
    prior_beta = 1,
    posterior_prior = NULL) {
  reward_model_request <- bg_match_reward_model_public(reward_model)
  posterior_model_request <- bg_match_posterior_model_public(posterior_model)
  unresolved_value <- bg_validate_named_numeric_scalar(unresolved_value, "unresolved_value", lower = 0)
  if (unresolved_value > 1) {
    stop("`unresolved_value` must lie in [0, 1].", call. = FALSE)
  }

  legacy_alias <- FALSE
  reward_model_canonical <- reward_model_request
  posterior_model_canonical <- posterior_model_request
  note <- NULL

  compatible <- bg_supported_posterior_models(reward_model_canonical)
  if (!posterior_model_canonical %in% compatible) {
    stop(
      sprintf(
        "`posterior_model = '%s'` is not compatible with `reward_model = '%s'`.",
        posterior_model_request,
        reward_model_request
      ),
      call. = FALSE
    )
  }

  if (identical(reward_model_canonical, "win_loss") && !unresolved_value %in% c(0, 1)) {
    stop(
      "`reward_model = 'win_loss'` requires `unresolved_value` to be either 0 or 1, ",
      "so the rollout reward remains binary.",
      call. = FALSE
    )
  }

  prior <- bg_resolve_posterior_prior(
    reward_model_canonical = reward_model_canonical,
    posterior_model_canonical = posterior_model_canonical,
    unresolved_value = unresolved_value,
    prior_alpha = prior_alpha,
    prior_beta = prior_beta,
    posterior_prior = posterior_prior
  )

  family <- switch(
    posterior_model_canonical,
    beta_bernoulli = "beta",
    beta_pseudo = "beta",
    dirichlet_multinomial = "dirichlet",
    gaussian_approx = "gaussian",
    normal_inverse_gamma = "normal_inverse_gamma",
    student_t_marginal = "student_t",
    bootstrap = "bootstrap"
  )

  exact <- posterior_model_canonical %in% c("beta_bernoulli", "dirichlet_multinomial")
  reward_support <- switch(
    reward_model_canonical,
    win_loss = "{0, 1}",
    categorical_outcome = "normalized scored categories {single, gammon, backgammon} plus unresolved",
    scalar_payoff = sprintf("{0, %.3f, 1}", unresolved_value)
  )

  if (is.null(note)) {
    note <- switch(
      posterior_model_canonical,
      beta_bernoulli = "Exact conjugate Beta-Bernoulli model for binary rollout rewards.",
      beta_pseudo = paste(
        "Approximate Beta-style model on the scalar payoff. It preserves the",
        "current package semantics but is not exact conjugacy once unresolved",
        "rollouts receive fractional payoff."
      ),
      dirichlet_multinomial = paste(
        "Exact conjugate Dirichlet-multinomial model on scored categorical",
        "rollout outcomes. The default uses seven buckets:",
        "single/gammon/backgammon loss, unresolved, and",
        "single/gammon/backgammon win."
      ),
      gaussian_approx = "Plug-in Gaussian approximation on the scalar payoff mean.",
      normal_inverse_gamma = "Normal-inverse-gamma posterior on the scalar payoff mean and variance.",
      student_t_marginal = "Student-t marginal mean posterior induced by a normal-inverse-gamma prior.",
      bootstrap = "Nonparametric bootstrap approximation over the empirical rollout outcome distribution."
    )
  }

  list(
    reward_model = reward_model_request,
    posterior_model = posterior_model_request,
    reward_model_canonical = reward_model_canonical,
    posterior_model_canonical = posterior_model_canonical,
    posterior_family = family,
    reward_support = reward_support,
    exact = exact,
    legacy_alias = legacy_alias,
    prior = prior,
    posterior_prior = prior,
    model_signature = bg_model_spec_signature(prior),
    note = note,
    next_coherent_models = c(
      "categorical_outcome + richer backgammon scoring categories",
      "feature-informed priors once state batteries stabilize"
    )
  )
}

bg_problem_model_spec <- function(problem) {
  if (!inherits(problem, "bg_problem")) {
    stop("`problem` must inherit from class 'bg_problem'.", call. = FALSE)
  }
  list(
    reward_model = problem$settings$reward_model,
    posterior_model = problem$settings$posterior_model,
    reward_model_canonical = problem$settings$reward_model_canonical,
    posterior_model_canonical = problem$settings$posterior_model_canonical,
    posterior_prior = problem$settings$posterior_prior,
    unresolved_value = problem$settings$unresolved_value
  )
}

bg_problem_reward_model <- function(problem) {
  bg_problem_model_spec(problem)$reward_model_canonical
}

bg_problem_posterior_model <- function(problem) {
  bg_problem_model_spec(problem)$posterior_model_canonical
}

bg_model_spec_summary <- function(problem) {
  if (!inherits(problem, "bg_problem")) {
    stop("`problem` must inherit from class 'bg_problem'.", call. = FALSE)
  }

  data.frame(
    reward_model = problem$settings$reward_model,
    reward_model_canonical = problem$settings$reward_model_canonical,
    posterior_model = problem$settings$posterior_model,
    posterior_model_canonical = problem$settings$posterior_model_canonical,
    unresolved_value = problem$settings$unresolved_value,
    stringsAsFactors = FALSE
  )
}

bg_allocation_policy_label <- function(policy) {
  policy <- bg_match_allocation_policy_public(policy)
  switch(
    policy,
    thompson = "Canonical TS",
    top_two_thompson = "Top-Two TS",
    multi_sample_thompson = "Multi-Sample TS",
    soft_elimination_thompson = "Soft-Elimination TS",
    forced_exploration_thompson = "Forced-Exploration TS",
    top_k_thompson = "Top-K TS",
    equal = "Equal Allocation",
    greedy = "Greedy",
    ucb = "UCB",
    ocba = "OCBA",
    policy
  )
}

# -----------------------------------------------------------------------------
# Source: bg_ts_policies.R
# -----------------------------------------------------------------------------
# Allocation-policy routing, TS/TTTS run assembly, and budget-path helpers.
#
# This is the main control file for finite-budget decision runs. It decides
# which engine path is used, keeps the routing rules explicit, and assembles
# the run/checkpoint objects consumed by the diagnostics layer.

# Normalize public policy names once so every front door shares the same
# accepted spellings and error messages.
bg_match_allocation_policy_public <- function(policy) {
  if (length(policy) > 1L) {
    policy <- policy[[1L]]
  }

  if (identical(policy, "elimination_thompson")) {
    policy <- "soft_elimination_thompson"
  }
  if (identical(policy, "ranking_aware_thompson")) {
    policy <- "top_k_thompson"
  }

  match.arg(
    policy,
    choices = c(
      "thompson",
      "top_two_thompson",
      "multi_sample_thompson",
      "soft_elimination_thompson",
      "forced_exploration_thompson",
      "top_k_thompson",
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
    "soft_elimination_thompson",
    "forced_exploration_thompson",
    "top_k_thompson"
  )
}

# Keep the experimental TS list explicit so docs, warnings, and plots all
# demote the same policies consistently.
bg_experimental_ts_policies <- function() {
  c(
    "multi_sample_thompson",
    "soft_elimination_thompson",
    "forced_exploration_thompson",
    "top_k_thompson"
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
    soft_elimination_thompson = "thompson",
    forced_exploration_thompson = "thompson",
    top_k_thompson = "thompson",
    equal = "equal",
    greedy = "greedy",
    ucb = "ucb",
    ocba = "ocba"
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
    return(list(
      entropy = NA_real_,
      hhi = NA_real_,
      hhi_normalized = NA_real_,
      max_share = NA_real_,
      effective_arm_fraction = NA_real_
    ))
  }

  share <- counts / total
  positive <- share[share > 0]
  n_actions <- length(share)
  entropy <- if (length(positive) <= 1L) {
    0
  } else {
    -sum(positive * log(positive)) / log(length(share))
  }
  hhi <- sum(share^2)
  hhi_normalized <- if (n_actions <= 1L) {
    1
  } else {
    (hhi - (1 / n_actions)) / (1 - (1 / n_actions))
  }

  list(
    entropy = entropy,
    hhi = hhi,
    hhi_normalized = hhi_normalized,
    max_share = max(share),
    effective_arm_fraction = (1 / hhi) / n_actions
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
        problem$settings$unresolved_value,
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

# Internal Thompson front door used by the exported wrappers in bg_ts_api.R.
bg_ts_decide <- function(
    problem,
    budget = 256L,
    allocation_policy = c(
      "thompson",
      "top_two_thompson",
      "multi_sample_thompson",
      "soft_elimination_thompson",
      "forced_exploration_thompson",
      "top_k_thompson"
    ),
    proxy_reference = NULL,
    checkpoints = NULL,
    ts_mode = c("sequential", "batched"),
    multi_sample_draws = 5L,
    ttts_beta = 0.5,
    ranking_top_k = 3L,
    ranking_draws = 128L,
    elimination_min_allocations = 4L,
    elimination_keep_top = 2L,
    elimination_margin = 0,
    forced_every = 8L,
    forced_min_allocations = 2L,
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
      ranking_top_k = ranking_top_k,
      ranking_draws = ranking_draws,
      elimination_min_allocations = elimination_min_allocations,
      elimination_keep_top = elimination_keep_top,
      elimination_margin = elimination_margin,
      forced_every = forced_every,
      forced_min_allocations = forced_min_allocations,
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

# -----------------------------------------------------------------------------
# Source: bg_ts_api.R
# -----------------------------------------------------------------------------
# Research-facing TS/TTTS front doors and shared study infrastructure.
#
# This file owns the stable front-door helpers that package users are supposed
# to call. Policy-specific selection logic lives in `bg_ts_policies.R`; this
# file keeps the cache paths, study persistence, and run-object plumbing
# explicit.

bg_local_package_version <- function() {
  desc <- tryCatch(utils::packageDescription("backgammonr"), error = function(e) NULL)
  if (!is.null(desc) && !is.null(desc$Version) && nzchar(desc$Version)) {
    return(as.character(desc$Version))
  }

  desc_path <- file.path(getwd(), "DESCRIPTION")
  if (file.exists(desc_path)) {
    fields <- tryCatch(read.dcf(desc_path), error = function(e) NULL)
    if (!is.null(fields) && "Version" %in% colnames(fields)) {
      return(as.character(fields[1L, "Version"]))
    }
  }

  NA_character_
}

bg_default_truth_cache_dir <- function(scope = c("state", "opening", "battery")) {
  scope <- match.arg(scope)
  file.path(tools::R_user_dir("backgammonr", which = "cache"), "truth", scope)
}

bg_repo_root <- function(start = getwd()) {
  path <- normalizePath(start, mustWork = FALSE)
  previous <- ""

  while (!identical(path, previous)) {
    if (file.exists(file.path(path, "DESCRIPTION"))) {
      return(path)
    }
    previous <- path
    path <- dirname(path)
  }

  NULL
}

bg_repo_opening_truth_cache_dir <- function(start = getwd()) {
  root <- bg_repo_root(start)
  if (is.null(root)) {
    return(NULL)
  }

  path <- file.path(root, "cache", "opening_truths_restart")
  if (!dir.exists(path)) {
    return(NULL)
  }

  normalizePath(path, mustWork = FALSE)
}

bg_resolve_opening_truth_cache_dir <- function(cache_dir = NULL) {
  if (!is.null(cache_dir)) {
    return(cache_dir)
  }

  repo_cache <- bg_repo_opening_truth_cache_dir()
  if (!is.null(repo_cache)) {
    return(repo_cache)
  }

  bg_default_truth_cache_dir("opening")
}

bg_default_study_cache_dir <- function(scope = c("profile", "comparison", "state_battery", "opening", "traces", "crn")) {
  scope <- match.arg(scope)
  file.path(tools::R_user_dir("backgammonr", which = "cache"), "study", scope)
}

bg_hash_value <- function(x, n = 24L) {
  raw <- serialize(x, NULL, version = 2L)
  hex <- paste(sprintf("%02x", as.integer(raw)), collapse = "")
  substr(hex, 1L, min(bg_coerce_integerish(n, "n", 1L), nchar(hex)))
}

bg_truth_reward_signature <- function(problem) {
  reward_map <- bg_reward_map_from_problem(problem)

  list(
    reward_model = problem$settings$reward_model_canonical,
    reward_values = unname(as.numeric(reward_map)),
    reward_names = names(reward_map)
  )
}

bg_truth_problem_identity <- function(problem) {
  list(
    board_points = unclass(problem$board)$points,
    board_bar = unclass(problem$board)$bar,
    board_off = unclass(problem$board)$off,
    board_turn = unclass(problem$board)$turn,
    roll = bg_as_roll(problem$roll)$dice,
    simulation_policy_engine = problem$settings$simulation_policy_engine,
    max_rollout_turns = problem$settings$max_rollout_turns,
    unresolved_value = problem$settings$unresolved_value,
    reward = bg_truth_reward_signature(problem)
  )
}

bg_truth_reference_identity <- function(
    reference_mode = c("equal", "focused"),
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE) {
  list(
    reference_mode = match.arg(reference_mode),
    dice_mode = match.arg(dice_mode),
    crn = isTRUE(crn)
  )
}

bg_truth_identity <- function(
    problem,
    reference_mode = c("equal", "focused"),
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE) {
  list(
    estimand = bg_truth_problem_identity(problem),
    reference = bg_truth_reference_identity(
      reference_mode = reference_mode,
      dice_mode = dice_mode,
      crn = crn
    )
  )
}

bg_truth_problem_hash <- function(
    problem,
    reference_mode = c("equal", "focused"),
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE) {
  bg_hash_value(bg_truth_identity(
    problem = problem,
    reference_mode = reference_mode,
    dice_mode = dice_mode,
    crn = crn
  ))
}

bg_safe_file_label <- function(x) {
  x <- gsub("[^A-Za-z0-9_-]+", "_", as.character(x))
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  if (!nzchar(x)) {
    return("bg_object")
  }
  x
}

bg_truth_roll_label <- function(roll) {
  roll <- bg_as_roll(roll)
  paste(roll$dice, collapse = "-")
}

bg_truth_environment_metadata <- function(problem) {
  list(
    estimand = paste(
      "Expected rollout reward after committing to a legal move and then",
      "simulating the rest of the game under the configured continuation policy."
    ),
    scientific_scope = paste(
      "This is proxy truth for the random-play rollout environment, not expert",
      "backgammon truth and not game-theoretic truth."
    ),
    simulation_policy = problem$settings$simulation_policy,
    simulation_policy_engine = problem$settings$simulation_policy_engine,
    reward_model = problem$settings$reward_model,
    reward_model_canonical = problem$settings$reward_model_canonical,
    posterior_model = problem$settings$posterior_model,
    posterior_model_canonical = problem$settings$posterior_model_canonical,
    max_rollout_turns = problem$settings$max_rollout_turns,
    unresolved_value = problem$settings$unresolved_value,
    truth_identity_note = paste(
      "Truth identity depends on board, roll, rollout environment, reward mapping,",
      "reference mode, dice mode, and CRN settings, but not on posterior-family",
      "settings used later for inference."
    ),
    scoring = paste(
      "`", problem$settings$reward_model, "` reward with unresolved rollouts mapped to ",
      format(problem$settings$unresolved_value, digits = 4L),
      "; posterior summaries use `", problem$settings$posterior_model, "`."
    )
  )
}

bg_truth_storage_path <- function(
    problem,
    scope = c("state", "opening", "battery"),
    cache_dir = NULL,
    reference_mode = c("equal", "focused"),
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE) {
  scope <- match.arg(scope)
  if (is.null(cache_dir)) {
    cache_dir <- if (identical(scope, "opening")) {
      bg_resolve_opening_truth_cache_dir()
    } else {
      bg_default_truth_cache_dir(scope)
    }
  }
  file.path(
    cache_dir,
    paste0(
      bg_safe_file_label(problem$problem_id),
      "_",
      bg_safe_file_label(bg_truth_problem_hash(
        problem = problem,
        reference_mode = reference_mode,
        dice_mode = dice_mode,
        crn = crn
      )),
      ".rds"
    )
  )
}

bg_normalize_truth_reference <- function(x, arg_name = "x") {
  if (inherits(x, "bg_reference")) {
    return(x)
  }
  if (inherits(x, "bg_truth_state")) {
    return(x$reference)
  }
  stop(sprintf("`%s` must be a `bg_reference` or `bg_truth_state` object.", arg_name), call. = FALSE)
}

bg_dense_trace_checkpoints <- function(budget) {
  budget <- bg_coerce_integerish(budget, "budget", 1L)
  if (budget <= 32L) {
    return(seq_len(budget))
  }

  dense_small <- 1:16
  tail_grid <- unique(round(exp(seq(log(20), log(budget), length.out = 18L))))
  sort(unique(c(dense_small, tail_grid, budget)))
}

bg_relabel_run_policy <- function(run, label, warning = NULL) {
  run$allocation_policy <- label
  if (!is.null(run$checkpoint_table) && nrow(run$checkpoint_table) > 0L) {
    run$checkpoint_table$allocation_policy <- label
  }
  if (!is.null(run$settings)) {
    run$settings$policy_label <- label
  }
  if (!is.null(warning)) {
    run$warnings <- unique(c(run$warnings, warning))
  }
  run
}

bg_save_serialized_object <- function(x, path, overwrite = FALSE) {
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

bg_maybe_load_saved_study <- function(path = NULL, overwrite = FALSE) {
  if (is.null(path) || isTRUE(overwrite) || !file.exists(path)) {
    return(NULL)
  }
  bg_study_load(path)
}

bg_task_apply <- function(
    tasks,
    worker,
    n_cores = 1L,
    parallel = FALSE,
    progress = FALSE,
    progress_label = "tasks") {
  bg_assert_scalar_flag(parallel, "parallel")
  bg_assert_scalar_flag(progress, "progress")
  n_cores <- bg_coerce_integerish(n_cores, "n_cores", 1L)

  if (length(tasks) == 0L) {
    return(list())
  }

  workers <- if (isTRUE(parallel)) max(1L, n_cores) else 1L
  can_fork <- identical(.Platform$OS.type, "unix")

  if (workers > 1L && length(tasks) > 1L && isTRUE(can_fork)) {
    if (isTRUE(progress)) {
      message(sprintf(
        "Running %d %s in parallel on %d cores.",
        length(tasks),
        progress_label,
        workers
      ))
    }
    return(parallel::mclapply(
      X = tasks,
      FUN = worker,
      mc.cores = workers,
      mc.preschedule = FALSE,
      mc.set.seed = FALSE
    ))
  }

  if (workers > 1L && !isTRUE(can_fork) && isTRUE(progress)) {
    message(sprintf(
      "Parallel execution for %s currently falls back to sequential mode on this platform.",
      progress_label
    ))
  }

  out <- vector("list", length(tasks))
  pb <- if (isTRUE(progress)) utils::txtProgressBar(min = 0, max = length(tasks), style = 3) else NULL
  on.exit(if (!is.null(pb)) close(pb), add = TRUE)

  for (i in seq_along(tasks)) {
    out[[i]] <- worker(tasks[[i]])
    if (!is.null(pb)) {
      utils::setTxtProgressBar(pb, i)
    }
  }

  out
}

#' Save a study object to disk
#'
#' @param x A supported study object.
#' @param path Output `.rds` path.
#' @param overwrite Logical scalar; if `FALSE`, existing files cause an error.
#'
#' @return The input object, invisibly.
#' @export
bg_study_save <- function(x, path, overwrite = FALSE) {
  bg_save_serialized_object(x, path, overwrite = overwrite)
}

#' Load a study object from disk
#'
#' @param path Path created by `bg_study_save()`.
#'
#' @return A deserialized study object.
#' @export
bg_study_load <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("`path` must be a non-empty character scalar.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("`path` does not exist.", call. = FALSE)
  }
  readRDS(path)
}

#' Run canonical Thompson sampling on one problem
#'
#' `bg_ts_run()` is the main front door for canonical sequential Thompson
#' sampling on one `bg_problem`.
#'
#' @param problem A `bg_problem` object.
#' @param budget Integer-like simulation budget.
#' @param proxy_reference Optional `bg_reference` object used for regret and
#'   ranking diagnostics.
#' @param checkpoints Optional integer checkpoint vector.
#' @param seed Optional integer-like seed.
#' @param ... Passed through to the underlying Thompson engine.
#'
#' @return A `bg_ts_run` object.
#' @export
bg_ts_run <- function(
    problem,
    budget = 256L,
    proxy_reference = NULL,
    checkpoints = NULL,
    seed = NULL,
    ...) {
  bg_ts_decide(
    problem = problem,
    budget = budget,
    allocation_policy = "thompson",
    proxy_reference = proxy_reference,
    checkpoints = checkpoints,
    seed = seed,
    ...
  )
}

#' Run top-two Thompson sampling on one problem
#'
#' `bg_ttts_run()` is the front door for top-two Thompson sampling on one
#' `bg_problem`.
#'
#' @param problem A `bg_problem` object.
#' @param budget Integer-like simulation budget.
#' @param proxy_reference Optional `bg_reference` object used for regret and
#'   ranking diagnostics.
#' @param checkpoints Optional integer checkpoint vector.
#' @param seed Optional integer-like seed.
#' @param ttts_beta Probability of resampling the current Thompson winner.
#' @param ... Passed through to the underlying Thompson engine.
#'
#' @return A `bg_ts_run` object.
#' @export
bg_ttts_run <- function(
    problem,
    budget = 256L,
    proxy_reference = NULL,
    checkpoints = NULL,
    seed = NULL,
    ttts_beta = 0.5,
    ...) {
  bg_ts_decide(
    problem = problem,
    budget = budget,
    allocation_policy = "top_two_thompson",
    proxy_reference = proxy_reference,
    checkpoints = checkpoints,
    seed = seed,
    ttts_beta = ttts_beta,
    ...
  )
}

#' Run multi-sample Thompson sampling on one problem
#'
#' `bg_multi_sample_ts_run()` averages several posterior draws per action
#' before selecting the next arm. This is an explicitly experimental TS-family
#' variant, but it is supported as part of the cleaned method set.
#'
#' @param problem A `bg_problem` object.
#' @param budget Integer-like simulation budget.
#' @param proxy_reference Optional `bg_reference` object used for regret and
#'   ranking diagnostics.
#' @param checkpoints Optional integer checkpoint vector.
#' @param seed Optional integer-like seed.
#' @param multi_sample_draws Number of posterior draws averaged per action at
#'   each selection step.
#' @param ... Passed through to the underlying Thompson engine.
#'
#' @return A `bg_ts_run` object.
#' @export
bg_multi_sample_ts_run <- function(
    problem,
    budget = 256L,
    proxy_reference = NULL,
    checkpoints = NULL,
    seed = NULL,
    multi_sample_draws = 5L,
    ...) {
  bg_ts_decide(
    problem = problem,
    budget = budget,
    allocation_policy = "multi_sample_thompson",
    proxy_reference = proxy_reference,
    checkpoints = checkpoints,
    seed = seed,
    multi_sample_draws = multi_sample_draws,
    ...
  )
}

#' Run soft-elimination Thompson sampling on one problem
#'
#' `bg_soft_elimination_ts_run()` uses posterior intervals to retire clearly
#' dominated arms while keeping a protected top set active.
#'
#' @param problem A `bg_problem` object.
#' @param budget Integer-like simulation budget.
#' @param proxy_reference Optional `bg_reference` object used for regret and
#'   ranking diagnostics.
#' @param checkpoints Optional integer checkpoint vector.
#' @param seed Optional integer-like seed.
#' @param elimination_min_allocations Minimum allocations before elimination can
#'   start.
#' @param elimination_keep_top Number of currently best-looking actions that are
#'   always kept active.
#' @param elimination_margin Additional elimination slack on the interval
#'   comparison scale.
#' @param ... Passed through to the underlying Thompson engine.
#'
#' @return A `bg_ts_run` object.
#' @export
bg_soft_elimination_ts_run <- function(
    problem,
    budget = 256L,
    proxy_reference = NULL,
    checkpoints = NULL,
    seed = NULL,
    elimination_min_allocations = 4L,
    elimination_keep_top = 2L,
    elimination_margin = 0,
    ...) {
  bg_ts_decide(
    problem = problem,
    budget = budget,
    allocation_policy = "soft_elimination_thompson",
    proxy_reference = proxy_reference,
    checkpoints = checkpoints,
    seed = seed,
    elimination_min_allocations = elimination_min_allocations,
    elimination_keep_top = elimination_keep_top,
    elimination_margin = elimination_margin,
    ...
  )
}

#' Run forced-exploration Thompson sampling on one problem
#'
#' `bg_forced_exploration_ts_run()` interleaves Thompson decisions with regular
#' least-sampled exploration steps.
#'
#' @param problem A `bg_problem` object.
#' @param budget Integer-like simulation budget.
#' @param proxy_reference Optional `bg_reference` object used for regret and
#'   ranking diagnostics.
#' @param checkpoints Optional integer checkpoint vector.
#' @param seed Optional integer-like seed.
#' @param forced_every Force one exploration step every `forced_every`
#'   allocations.
#' @param forced_min_allocations Ensure every action reaches at least this many
#'   allocations before pure Thompson steps dominate.
#' @param ... Passed through to the underlying Thompson engine.
#'
#' @return A `bg_ts_run` object.
#' @export
bg_forced_exploration_ts_run <- function(
    problem,
    budget = 256L,
    proxy_reference = NULL,
    checkpoints = NULL,
    seed = NULL,
    forced_every = 8L,
    forced_min_allocations = 2L,
    ...) {
  bg_ts_decide(
    problem = problem,
    budget = budget,
    allocation_policy = "forced_exploration_thompson",
    proxy_reference = proxy_reference,
    checkpoints = checkpoints,
    seed = seed,
    forced_every = forced_every,
    forced_min_allocations = forced_min_allocations,
    ...
  )
}

#' Run top-k-focused Thompson sampling on one problem
#'
#' `bg_top_k_ts_run()` biases exploration toward the currently strongest-looking
#' subset of actions rather than the full action set.
#'
#' @param problem A `bg_problem` object.
#' @param budget Integer-like simulation budget.
#' @param proxy_reference Optional `bg_reference` object used for regret and
#'   ranking diagnostics.
#' @param checkpoints Optional integer checkpoint vector.
#' @param seed Optional integer-like seed.
#' @param top_k Number of currently strongest-looking actions to prioritize.
#' @param ranking_draws Number of posterior draws used to stabilize the top-k
#'   selection step.
#' @param ... Passed through to the underlying Thompson engine.
#'
#' @return A `bg_ts_run` object.
#' @export
bg_top_k_ts_run <- function(
    problem,
    budget = 256L,
    proxy_reference = NULL,
    checkpoints = NULL,
    seed = NULL,
    top_k = 3L,
    ranking_draws = 128L,
    ...) {
  bg_ts_decide(
    problem = problem,
    budget = budget,
    allocation_policy = "top_k_thompson",
    proxy_reference = proxy_reference,
    checkpoints = checkpoints,
    seed = seed,
    ranking_top_k = top_k,
    ranking_draws = ranking_draws,
    ...
  )
}

#' Run a named Thompson-family variant on one problem
#'
#' Internal convenience wrapper around the Thompson engine for experimental
#' Thompson-inspired policies.
#'
#' @param problem A `bg_problem` object.
#' @param budget Integer-like simulation budget.
#' @param variant Thompson-family variant.
#' @param ... Passed to the underlying Thompson engine.
#'
#' @return A `bg_ts_run` object.
#' @keywords internal
#' @noRd
bg_ts_variant_run <- function(
    problem,
    budget = 256L,
    variant = c(
      "thompson",
      "top_two_thompson",
      "multi_sample_thompson",
      "soft_elimination_thompson",
      "forced_exploration_thompson",
      "top_k_thompson"
    ),
    ...) {
  variant <- bg_match_allocation_policy_public(variant)
  if (!bg_is_thompson_policy_public(variant)) {
    stop("`variant` must be a Thompson-family allocation policy.", call. = FALSE)
  }

  bg_ts_decide(
    problem = problem,
    budget = budget,
    allocation_policy = variant,
    ...
  )
}

#' Run equal-allocation baseline on one problem
#'
#' `bg_equal_run()` is the main non-TS baseline kept in the live package
#' surface. It currently uses the scalar rollout engine and is therefore
#' available only for `scalar_payoff + beta_pseudo` problems.
#'
#' @param problem A `bg_problem` object.
#' @param budget Integer-like simulation budget.
#' @param checkpoints Optional checkpoint vector.
#' @param proxy_reference Optional `bg_reference` object.
#' @param ... Passed to the internal rollout runner.
#'
#' @return A `bg_ts_run` object with `allocation_policy = "equal"`.
#' @export
bg_equal_run <- function(
    problem,
    budget = 256L,
    checkpoints = NULL,
    proxy_reference = NULL,
    seed = NULL,
    ...) {
  bg_run_method_path(
    problem = problem,
    allocation_policy = "equal",
    budget = budget,
    checkpoints = checkpoints,
    reference = proxy_reference,
    seed = seed,
    ...
  )
}

#' Deprecated alias for [bg_equal_run()]
#'
#' `bg_uniform_run()` is retained for backward compatibility only. New code
#' should call [bg_equal_run()] directly.
#'
#' @inheritParams bg_equal_run
#' @return A `bg_ts_run` object with `allocation_policy = "equal"`.
#' @export
bg_uniform_run <- function(
    problem,
    budget = 256L,
    checkpoints = NULL,
    proxy_reference = NULL,
    seed = NULL,
    ...) {
  .Deprecated("bg_equal_run")
  bg_equal_run(
    problem = problem,
    budget = budget,
    checkpoints = checkpoints,
    proxy_reference = proxy_reference,
    seed = seed,
    ...
  )
}

#' Run a UCB baseline on one problem
#'
#' This is the public front door for the optimism-under-uncertainty comparator
#' family. The current implementation is a UCB1-style rule on the legacy
#' scalar rollout engine, so it is currently available only for
#' `scalar_payoff + beta_pseudo` problems.
#'
#' @param problem A `bg_problem` object.
#' @param budget Integer-like simulation budget.
#' @param checkpoints Optional checkpoint vector.
#' @param proxy_reference Optional `bg_reference` object.
#' @param ucb_variant UCB variant. The current front door supports only
#'   `"ucb1"`.
#' @param ucb_exploration Exploration multiplier used in the bonus term.
#' @param ... Passed to the internal rollout runner.
#'
#' @return A `bg_ts_run` object with `allocation_policy = "ucb"`.
#' @export
bg_ucb_run <- function(
    problem,
    budget = 256L,
    checkpoints = NULL,
    proxy_reference = NULL,
    seed = NULL,
    ucb_variant = c("ucb1"),
    ucb_exploration = 1,
    ...) {
  ucb_variant <- match.arg(ucb_variant)
  out <- bg_run_method_path(
    problem = problem,
    allocation_policy = "ucb",
    budget = budget,
    checkpoints = checkpoints,
    reference = proxy_reference,
    seed = seed,
    ucb_exploration = ucb_exploration,
    ...
  )
  out$settings$ucb_variant <- ucb_variant
  out
}

#' Run the OCBA-style baseline on one problem
#'
#' This front door exposes the package's OCBA-inspired baseline under a name
#' that matches the implemented allocation rule.
#'
#' @param problem A `bg_problem` object.
#' @param budget Integer-like simulation budget.
#' @param checkpoints Optional checkpoint vector.
#' @param proxy_reference Optional `bg_reference` object.
#' @param ... Passed to the internal rollout runner.
#'
#' @return A `bg_ts_run` object with `allocation_policy = "ocba"`.
#' @keywords internal
#' @noRd
bg_ocba_run <- function(
    problem,
    budget = 256L,
    checkpoints = NULL,
    proxy_reference = NULL,
    ...) {
  bg_run_method_path(
    problem = problem,
    allocation_policy = "ocba",
    budget = budget,
    checkpoints = checkpoints,
    reference = proxy_reference,
    ...
  )
}

#' Sweep UCB over seeds and budgets
#'
#' @param problem A `bg_problem` object.
#' @param budgets Integer-like budget vector.
#' @param seeds Integer-like seed vector.
#' @param proxy_reference Optional `bg_reference` object.
#' @param reference_budget Integer-like proxy-reference budget used when a
#'   reference is built automatically.
#' @param ucb_variant UCB variant. The current front door supports only
#'   `"ucb1"`.
#' @param ucb_exploration Exploration multiplier used in the bonus term.
#' @param n_cores Integer-like worker count used for study-level parallelism.
#' @param parallel Logical scalar; if `TRUE`, parallelize the repeated-seed
#'   sweep.
#' @param progress Logical scalar; if `TRUE`, report progress.
#' @param save_path Optional `.rds` path for the returned study object.
#' @param overwrite Logical scalar controlling replacement of an existing
#'   `save_path`.
#' @param ... Passed to [bg_compare_methods()].
#'
#' @return A `bg_ucb_profile` object inheriting from `bg_method_compare`.
#' @keywords internal
#' @noRd
bg_ucb_profile <- function(
    problem,
    budgets = c(32L, 64L, 128L, 256L),
    seeds = 1:20,
    proxy_reference = NULL,
    reference_budget = max(4096L, 8L * max(budgets)),
    ucb_variant = c("ucb1"),
    ucb_exploration = 1,
    n_cores = 1L,
    parallel = FALSE,
    progress = interactive(),
    save_path = NULL,
    overwrite = FALSE,
    ...) {
  ucb_variant <- match.arg(ucb_variant)
  if (is.null(proxy_reference)) {
    proxy_reference <- bg_reference(
      problem = problem,
      budget = reference_budget,
      seed = bg_derive_seed(min(seeds), "ucb-profile-reference")
    )
  }

  out <- bg_compare_methods(
    problems = problem,
    methods = "ucb",
    budgets = budgets,
    seeds = seeds,
    proxy_references = proxy_reference,
    n_cores = n_cores,
    parallel = parallel,
    progress = progress,
    ucb_exploration = ucb_exploration,
    ...
  )
  out$settings$ucb_variant <- ucb_variant
  out$settings$ucb_exploration <- as.numeric(ucb_exploration)
  class(out) <- c("bg_ucb_profile", class(out))
  if (!is.null(save_path)) {
    bg_study_save(out, save_path, overwrite = overwrite)
  }
  out
}

#' Trace Thompson sampling over budget
#'
#' This wrapper requests a denser checkpoint grid from the Thompson engine so the
#' returned object is directly usable for allocation-path plots.
#'
#' @param problem A `bg_problem` object.
#' @param budget Integer-like simulation budget.
#' @param checkpoints Optional checkpoint vector. When omitted, a dense grid is
#'   constructed automatically.
#' @param ... Passed to the underlying Thompson engine.
#'
#' @return A `bg_ts_run` object.
#' @keywords internal
#' @noRd
bg_ts_trace <- function(problem, budget = 256L, checkpoints = NULL, ...) {
  if (is.null(checkpoints)) {
    checkpoints <- bg_dense_trace_checkpoints(budget)
  }

  bg_ts_decide(
    problem = problem,
    budget = budget,
    checkpoints = checkpoints,
    ...
  )
}

#' Sweep Thompson sampling over seeds and budgets
#'
#' Wrapper around the internal repeated-budget Thompson study helper.
#'
#' @param problem A `bg_problem` object.
#' @param budgets Integer-like budget vector.
#' @param seeds Integer-like seed vector.
#' @param ... Passed to the underlying repeated-study helper.
#'
#' @return A `bg_ts_profile` object.
#' @keywords internal
#' @noRd
bg_ts_seed_sweep <- function(
    problem,
    budgets = c(32L, 64L, 128L, 256L),
    seeds = 1:20,
    save_path = NULL,
    overwrite = FALSE,
    n_cores = 1L,
    parallel = FALSE,
    progress = interactive(),
    ...) {
  out <- bg_ts_profile(
    problem = problem,
    budgets = budgets,
    seeds = seeds,
    n_cores = n_cores,
    parallel = parallel,
    progress = progress,
    ...
  )
  if (!is.null(save_path)) {
    bg_study_save(out, save_path, overwrite = overwrite)
  }
  out
}

#' Compare algorithms on one or many decision problems
#'
#' Wrapper around [bg_compare_methods()] using the compact public `bg_*`
#' workflow vocabulary.
#'
#' The main package comparison is TS, TTTS, and the equal-allocation baseline.
#' Legacy scalar comparators such as `"ucb"` remain available when requested,
#' but they are no longer part of the default public story.
#'
#' @param problems A `bg_problem` object or list of them.
#' @param ... Passed to [bg_compare_methods()].
#'
#' @return A `bg_method_compare` object.
#' @export
bg_compare_algorithms <- function(
    problems,
    methods = c("thompson", "top_two_thompson", "equal"),
    budgets = c(32L, 64L, 128L, 256L),
    seeds = 1:20,
    proxy_references = NULL,
    reference_budget = max(4096L, 8L * max(budgets)),
    save_path = NULL,
    overwrite = FALSE,
    n_cores = 1L,
    parallel = FALSE,
    progress = interactive(),
    ...) {
  cached <- bg_maybe_load_saved_study(save_path, overwrite = overwrite)
  if (!is.null(cached)) {
    return(cached)
  }

  out <- bg_compare_methods(
    problems = problems,
    methods = methods,
    budgets = budgets,
    seeds = seeds,
    proxy_references = proxy_references,
    reference_budget = reference_budget,
    n_cores = n_cores,
    parallel = parallel,
    progress = progress,
    ...
  )
  if (!is.null(save_path)) {
    bg_study_save(out, save_path, overwrite = overwrite)
  }
  out
}

#' Summarize Thompson posterior-like quantities at one checkpoint
#'
#' @param x A `bg_ts_run` object.
#' @param checkpoint Optional checkpoint. Defaults to the final budget.
#' @param top_n Integer-like number of actions to return.
#'
#' @return A compact data frame.
#' @keywords internal
#' @noRd
bg_ts_posterior_summary <- function(x, checkpoint = NULL, top_n = 8L) {
  if (!inherits(x, "bg_ts_run")) {
    stop("`x` must inherit from class 'bg_ts_run'.", call. = FALSE)
  }
  top_n <- bg_coerce_integerish(top_n, "top_n", 1L)
  if (is.null(checkpoint) || checkpoint == x$budget) {
    tab <- as.data.frame(x$action_table, stringsAsFactors = FALSE)
  } else {
    checkpoint <- bg_coerce_integerish(checkpoint, "checkpoint", 1L)
    tab <- x$checkpoint_actions[x$checkpoint_actions$checkpoint == checkpoint, , drop = FALSE]
    if (nrow(tab) > 0L) {
      tab <- tab[order(-tab$estimate, tab$candidate_index), , drop = FALSE]
      tab$rank <- seq_len(nrow(tab))
    }
  }

  if (nrow(tab) == 0L) {
    return(tab)
  }

  utils::head(
    tab[, c(
      "rank",
      "candidate_index",
      "move_label",
      "allocation_count",
      "estimate",
      "posterior_sd",
      "model_relative_prob_best",
      "model_relative_expected_regret"
    ), drop = FALSE],
    top_n
  )
}

#' Compute move-level structural features
#'
#' @param x A `bg_problem`, `bg_move_sequence`, or list of move sequences.
#'
#' @return A data frame of move features.
#' @export
bg_move_features <- function(x) {
  if (inherits(x, "bg_problem")) {
    return(x$candidate_table[, c(
      "candidate_index",
      "move_label",
      "n_steps",
      "n_hits",
      "n_bar_entries",
      "n_bear_off",
      "total_step_distance",
      "n_equivalent_sequences"
    ), drop = FALSE])
  }

  moves <- if (is_bg_move_sequence(x)) {
    list(x)
  } else if (is.list(x)) {
    bg_normalize_move_sequence_list(x)
  } else {
    stop("`x` must be a `bg_problem`, `bg_move_sequence`, or list of moves.", call. = FALSE)
  }

  rows <- lapply(
    seq_along(moves),
    function(i) {
      move <- bg_as_move_sequence(moves[[i]])
      feats <- bg_move_action_features(move)
      data.frame(
        candidate_index = i,
        move_label = bg_move_label(move),
        n_equivalent_sequences = 1L,
        feats,
        stringsAsFactors = FALSE
      )
    }
  )
  do.call(rbind, rows)
}

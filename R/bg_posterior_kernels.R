# Explicit-posterior helpers that wrap the C++ posterior kernels for TS workflows.
# One explicit test for the legacy scalar shortcut. Everything else routes
# through the explicit-posterior kernels below.
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

  bg_variant_pick_index(
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
    tempered_thompson = 1L,
    elimination_thompson = max(64L, ranking_draws),
    multi_sample_thompson = max(2L, multi_sample_draws),
    top_two_thompson = max(64L, ranking_draws),
    ranking_aware_thompson = max(64L, ranking_draws),
    budget_aware_thompson = {
      progress <- if (budget <= 0L) 1 else spent / budget
      if (progress < 0.3) {
        max(3L, multi_sample_draws)
      } else {
        max(64L, ranking_draws)
      }
    },
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
    ranking_top_k = 3L) {
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

  if (identical(allocation_policy, "tempered_thompson")) {
    local_draws <- bg_temper_draw_matrix(local_draws, local_mean, temperature = temperature)
  }

  local_choice <- switch(
    allocation_policy,
    thompson = {
      bg_variant_pick_index(local_draws[1L, ], local_count, tie_break = local_mean)
    },
    top_two_thompson = {
      bg_posterior_top_two_choice(local_draws, local_count, ttts_beta = ttts_beta)
    },
    multi_sample_thompson = {
      winners <- apply(local_draws, 1L, function(x) bg_variant_pick_index(x, local_count, tie_break = local_mean))
      winner_freq <- tabulate(winners, nbins = ncol(local_draws))
      bg_variant_pick_index(winner_freq, local_count, tie_break = local_mean)
    },
    tempered_thompson = {
      bg_variant_pick_index(local_draws[1L, ], local_count, tie_break = local_mean)
    },
    ranking_aware_thompson = {
      bg_posterior_ranking_choice(
        draw_mat = local_draws,
        posterior_mean = local_mean,
        allocation_count = local_count,
        focus_top_k = ranking_top_k
      )
    },
    budget_aware_thompson = {
      progress <- if (budget <= 0L) 1 else spent / budget
      if (progress < 0.3) {
        winners <- apply(local_draws, 1L, function(x) bg_variant_pick_index(x, local_count, tie_break = local_mean))
        winner_freq <- tabulate(winners, nbins = ncol(local_draws))
        bg_variant_pick_index(winner_freq, local_count, tie_break = local_mean)
      } else {
        sorted_mean <- sort(local_mean, decreasing = TRUE)
        top_gap <- if (length(sorted_mean) >= 2L) sorted_mean[[1L]] - sorted_mean[[2L]] else Inf
        ordered_sd <- local_sd[order(local_mean, decreasing = TRUE)]
        gap_sd <- if (length(ordered_sd) >= 2L) sqrt(sum(ordered_sd[1:2]^2)) else 0
        if (progress >= 0.75 && is.finite(top_gap) && is.finite(gap_sd) && top_gap <= 1.25 * gap_sd) {
          bg_posterior_top_two_choice(local_draws, local_count, ttts_beta = ttts_beta)
        } else if (progress >= 0.75) {
          bg_posterior_ranking_choice(
            draw_mat = local_draws,
            posterior_mean = local_mean,
            allocation_count = local_count,
            focus_top_k = ranking_top_k
          )
        } else {
          bg_variant_pick_index(local_draws[1L, ], local_count, tie_break = local_mean)
        }
      }
    },
    elimination_thompson = {
      bg_variant_pick_index(local_draws[1L, ], local_count, tie_break = local_mean)
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

        if (identical(allocation_policy, "elimination_thompson")) {
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
          ranking_top_k = ranking_top_k
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
  if (identical(allocation_policy, "elimination_thompson")) {
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

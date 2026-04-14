# Shared sequential posterior engine for the Thompson-family methods.
#
# Reading guide:
# - For the public front doors, read the per-method files:
#   * bg_policy_ts.R
#   * bg_policy_ttts.R
#   * bg_policy_multi_sample.R
#   * bg_policy_soft_elimination.R
#   * bg_policy_forced_exploration.R
#   * bg_policy_top_k.R
#   * bg_policy_equal.R
# - For the fast native scalar/Beta path used by canonical TS and TTTS, read:
#   * src/policy_ts.cpp
#   * src/policy_ttts.cpp
#   * src/alloc_core.cpp
# - For richer posterior stacks such as Student-t and Dirichlet, the loop below
#   remains the authoritative execution path while posterior draws/summaries are
#   produced in C++ through src/rcpp_entrypoints.cpp and the model_*.cpp files.
#
# This file intentionally keeps only the logic that is shared by more than one
# Thompson-family method. Method-specific selection rules live in the method
# files so users can inspect each algorithm separately.

# Check whether the currently loaded DLL actually contains a given compiled
# selector. This matters in development sessions where `RcppExports.R` may
# define the wrapper function but the package was loaded without recompiling the
# shared object. In that case the method files fall back to their equivalent R
# implementations so the study scripts continue to run.
bg_has_native_call <- local({
  cache <- new.env(parent = emptyenv())

  function(symbol_name) {
    if (exists(symbol_name, envir = cache, inherits = FALSE)) {
      return(get(symbol_name, envir = cache, inherits = FALSE))
    }

    dlls <- getLoadedDLLs()
    ok <- FALSE
    if ("backgammonr" %in% names(dlls)) {
      dll <- dlls[["backgammonr"]]
      if (inherits(dll, "DLLInfo")) {
        regs <- getDLLRegisteredRoutines(dll)
        call_names <- names(regs[[".Call"]])
        ok <- is.character(call_names) && symbol_name %in% call_names
      }
    }

    assign(symbol_name, ok, envir = cache)
    ok
  }
})

# Different TS variants need different draw budgets. Keep that rule centralized
# so the sequential TS loop can stay simple and all methods share the same
# bookkeeping and checkpoint logic.
bg_posterior_selection_draws <- function(
    allocation_policy,
    spent,
    budget,
    multi_sample_draws,
    ranking_draws) {
  switch(
    allocation_policy,
    equal = 1L,
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
# Thompson-family policy. The individual policy helpers called here live beside
# their public front doors so users can inspect one file per method.
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

  if (identical(allocation_policy, "equal")) {
    return(bg_posterior_equal_choice(active_idx, spent))
  }

  local_draws <- draw_mat[, active_idx, drop = FALSE]
  local_mean <- posterior_mean[active_idx]
  local_sd <- posterior_sd[active_idx]
  local_count <- allocation_count[active_idx]

  if (identical(allocation_policy, "forced_exploration_thompson")) {
    forced_local_choice <- bg_posterior_forced_choice(
      allocation_count = local_count,
      spent = spent,
      forced_every = forced_every,
      forced_min_allocations = forced_min_allocations
    )
    if (!is.na(forced_local_choice)) {
      return(active_idx[[forced_local_choice]])
    }
  }

  local_choice <- switch(
    allocation_policy,
    thompson = {
      bg_posterior_thompson_choice(local_draws, local_count, local_mean)
    },
    multi_sample_thompson = {
      bg_posterior_multi_sample_choice(local_draws, local_count, local_mean)
    },
    top_two_thompson = {
      bg_posterior_top_two_choice(local_draws, local_count, ttts_beta = ttts_beta)
    },
    forced_exploration_thompson = {
      bg_posterior_thompson_choice(local_draws, local_count, local_mean)
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
      bg_posterior_thompson_choice(local_draws, local_count, local_mean)
    },
    stop("Unsupported posterior Thompson variant.", call. = FALSE)
  )

  active_idx[[local_choice]]
}

# Shared explicit-posterior run loop.
#
# This loop is the common execution path for:
# - every non-fast Thompson-family method; and
# - every richer posterior stack, including Student-t and Dirichlet.
# It is intentionally verbose because it is the main research-facing engine for
# the package's adaptive local-allocation studies.
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
        if (identical(allocation_policy, "equal")) {
          chosen_pos <- ((spent %% nrow(stats_table)) + 1L)
        } else {
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
        }

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

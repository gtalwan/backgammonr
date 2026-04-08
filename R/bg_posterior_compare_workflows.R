# Repeated-study workflows for posterior-family and reward-stack comparisons.
bg_default_reward_model_map <- function() {
  c(
    scalar_payoff = "beta_pseudo",
    categorical_outcome = "dirichlet_multinomial",
    win_loss = "beta_bernoulli"
  )
}

bg_posterior_compare_summary <- function(results) {
  summary <- aggregate(
    results[, c(
      "recommended_prob_best",
      "simple_regret",
      "allocation_entropy",
      "allocation_hhi",
      "unresolved_fraction",
      "runtime_seconds"
    )],
    by = list(
      method = results$allocation_policy,
      checkpoint = results$checkpoint
    ),
    FUN = mean,
    na.rm = TRUE
  )
  summary <- merge(
    summary,
    bg_checkpoint_match_summary(results),
    by = c("method", "checkpoint"),
    all.x = TRUE,
    sort = FALSE
  )
  summary[order(summary$method, summary$checkpoint), , drop = FALSE]
}

bg_compare_posteriors <- function(
    problems,
    posterior_models = NULL,
    reward_model = NULL,
    budgets = c(32L, 64L, 128L, 256L),
    seeds = 1:20,
    allocation_policy = c("thompson", "top_two_thompson"),
    proxy_references = NULL,
    reference_budget = max(4096L, 8L * max(budgets)),
    n_cores = 1L,
    parallel = FALSE,
    progress = interactive(),
    save_path = NULL,
    overwrite = FALSE,
    ...) {
  cached <- bg_maybe_load_saved_study(save_path, overwrite = overwrite)
  if (!is.null(cached)) {
    return(cached)
  }

  if (inherits(problems, "bg_problem")) {
    problems <- list(problems)
  }
  if (!is.list(problems) || length(problems) < 1L || !all(vapply(problems, inherits, logical(1L), what = "bg_problem"))) {
    stop("`problems` must be a `bg_problem` object or a non-empty list of them.", call. = FALSE)
  }

  budgets <- sort(unique(bg_normalize_study_budgets(budgets)))
  seeds <- sort(unique(bg_coerce_integerish(seeds, "seeds", length(seeds))))
  allocation_policy <- bg_match_allocation_policy_public(allocation_policy)
  n_cores <- bg_coerce_integerish(n_cores, "n_cores", 1L)
  bg_assert_scalar_flag(parallel, "parallel")
  bg_assert_scalar_flag(progress, "progress")
  bg_assert_scalar_flag(overwrite, "overwrite")

  resolved_reward <- if (is.null(reward_model)) {
    problems[[1L]]$settings$reward_model_canonical
  } else {
    bg_match_reward_model_public(reward_model)
  }
  if (is.null(posterior_models)) {
    posterior_models <- bg_recommended_posterior_models(resolved_reward)
  }
  posterior_models <- unique(vapply(posterior_models, bg_match_posterior_model_public, character(1L), USE.NAMES = FALSE))

  base_problems <- lapply(
    problems,
    function(problem) {
      bg_problem_clone_models(
        problem = problem,
        reward_model = resolved_reward,
        posterior_model = posterior_models[[1L]],
        problem_id = problem$problem_id,
        cache = FALSE
      )
    }
  )

  if (is.null(proxy_references)) {
    proxy_references <- lapply(
      seq_along(base_problems),
      function(i) {
        bg_reference(
          problem = base_problems[[i]],
          budget = reference_budget,
          seed = bg_derive_seed(min(seeds), "posterior-compare-reference", i, resolved_reward)
        )
      }
    )
  } else if (inherits(proxy_references, "bg_reference")) {
    proxy_references <- list(proxy_references)
  }
  if (length(proxy_references) != length(base_problems)) {
    stop("`proxy_references` must be NULL, a single `bg_reference`, or a list aligned with `problems`.", call. = FALSE)
  }

  task_grid <- expand.grid(
    p_idx = seq_along(problems),
    posterior_model = posterior_models,
    seed = seeds,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  tasks <- split(task_grid, seq_len(nrow(task_grid)))

  task_runs <- bg_task_apply(
    tasks = tasks,
    n_cores = n_cores,
    parallel = parallel,
    progress = progress,
    progress_label = "posterior-family runs",
    worker = function(task) {
      p_idx <- task$p_idx[[1L]]
      posterior_model_i <- task$posterior_model[[1L]]
      seed_i <- task$seed[[1L]]
      problem_i <- bg_problem_clone_models(
        problem = problems[[p_idx]],
        reward_model = resolved_reward,
        posterior_model = posterior_model_i,
        problem_id = problems[[p_idx]]$problem_id,
        cache = FALSE
      )
      reference_i <- proxy_references[[p_idx]]
      label <- problem_i$settings$posterior_model_canonical

      run <- bg_ts_decide(
        problem = problem_i,
        budget = max(budgets),
        allocation_policy = allocation_policy,
        proxy_reference = reference_i,
        checkpoints = budgets,
        seed = seed_i,
        ...
      )
      run <- bg_relabel_run_policy(
        run,
        label = label,
        warning = sprintf(
          "Posterior comparison label `%s` represents `%s + %s` under `%s`.",
          label,
          problem_i$settings$reward_model,
          problem_i$settings$posterior_model,
          allocation_policy
        )
      )
      run$settings$base_allocation_policy <- allocation_policy
      run$settings$comparison_reward_model <- problem_i$settings$reward_model_canonical
      run$settings$comparison_posterior_model <- problem_i$settings$posterior_model_canonical

      df <- run$checkpoint_table
      df$problem_id <- problem_i$problem_id
      df$seed <- seed_i
      df$reward_model <- problem_i$settings$reward_model_canonical
      df$posterior_model <- problem_i$settings$posterior_model_canonical

      list(
        run_id = paste(problem_i$problem_id, label, seed_i, sep = "::"),
        run = run,
        checkpoint_table = df
      )
    }
  )

  runs <- lapply(task_runs, `[[`, "run")
  names(runs) <- vapply(task_runs, `[[`, character(1L), "run_id")
  results <- do.call(rbind, lapply(task_runs, `[[`, "checkpoint_table"))
  rownames(results) <- NULL
  summary <- bg_posterior_compare_summary(results)

  out <- structure(
    list(
      problems = problems,
      reward_model = resolved_reward,
      posterior_models = posterior_models,
      references = proxy_references,
      results = results,
      summary = summary,
      runs = runs,
      settings = list(
        budgets = budgets,
        seeds = seeds,
        allocation_policy = allocation_policy,
        reward_model = resolved_reward,
        n_cores = n_cores,
        parallel = isTRUE(parallel)
      )
    ),
    class = c("bg_posterior_compare", "bg_method_compare")
  )
  if (!is.null(save_path)) {
    bg_study_save(out, save_path, overwrite = overwrite)
  }
  out
}

bg_compare_reward_models <- function(
    problems,
    reward_model_map = bg_default_reward_model_map(),
    budgets = c(32L, 64L, 128L, 256L),
    seeds = 1:20,
    allocation_policy = c("thompson", "top_two_thompson"),
    win_loss_unresolved_value = 0,
    reference_budget = max(4096L, 8L * max(budgets)),
    n_cores = 1L,
    parallel = FALSE,
    progress = interactive(),
    save_path = NULL,
    overwrite = FALSE,
    ...) {
  cached <- bg_maybe_load_saved_study(save_path, overwrite = overwrite)
  if (!is.null(cached)) {
    return(cached)
  }

  if (inherits(problems, "bg_problem")) {
    problems <- list(problems)
  }
  if (!is.list(problems) || length(problems) < 1L || !all(vapply(problems, inherits, logical(1L), what = "bg_problem"))) {
    stop("`problems` must be a `bg_problem` object or a non-empty list of them.", call. = FALSE)
  }
  if (is.null(names(reward_model_map)) || any(!nzchar(names(reward_model_map)))) {
    stop("`reward_model_map` must be a named character vector or list.", call. = FALSE)
  }

  reward_models <- names(reward_model_map)
  reward_models <- vapply(reward_models, bg_match_reward_model_public, character(1L), USE.NAMES = FALSE)
  posterior_models <- vapply(unname(reward_model_map), bg_match_posterior_model_public, character(1L), USE.NAMES = FALSE)
  names(posterior_models) <- reward_models

  budgets <- sort(unique(bg_normalize_study_budgets(budgets)))
  seeds <- sort(unique(bg_coerce_integerish(seeds, "seeds", length(seeds))))
  allocation_policy <- bg_match_allocation_policy_public(allocation_policy)
  n_cores <- bg_coerce_integerish(n_cores, "n_cores", 1L)
  bg_assert_scalar_flag(parallel, "parallel")
  bg_assert_scalar_flag(progress, "progress")
  bg_assert_scalar_flag(overwrite, "overwrite")

  task_grid <- expand.grid(
    p_idx = seq_along(problems),
    reward_model = reward_models,
    seed = seeds,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  tasks <- split(task_grid, seq_len(nrow(task_grid)))

  task_runs <- bg_task_apply(
    tasks = tasks,
    n_cores = n_cores,
    parallel = parallel,
    progress = progress,
    progress_label = "reward-model runs",
    worker = function(task) {
      p_idx <- task$p_idx[[1L]]
      reward_model_i <- task$reward_model[[1L]]
      posterior_model_i <- posterior_models[[reward_model_i]]
      seed_i <- task$seed[[1L]]
      unresolved_i <- if (identical(reward_model_i, "win_loss")) win_loss_unresolved_value else problems[[p_idx]]$settings$unresolved_value

      problem_i <- bg_problem_clone_models(
        problem = problems[[p_idx]],
        reward_model = reward_model_i,
        posterior_model = posterior_model_i,
        unresolved_value = unresolved_i,
        problem_id = problems[[p_idx]]$problem_id,
        cache = FALSE
      )
      reference_i <- bg_reference(
        problem = problem_i,
        budget = reference_budget,
        seed = bg_derive_seed(min(seeds), "reward-model-reference", p_idx, reward_model_i)
      )
      label <- paste(problem_i$settings$reward_model_canonical, problem_i$settings$posterior_model_canonical, sep = "::")

      run <- bg_ts_decide(
        problem = problem_i,
        budget = max(budgets),
        allocation_policy = allocation_policy,
        proxy_reference = reference_i,
        checkpoints = budgets,
        seed = seed_i,
        ...
      )
      run <- bg_relabel_run_policy(
        run,
        label = label,
        warning = sprintf(
          "Reward-model comparison label `%s` represents `%s + %s` under `%s`.",
          label,
          problem_i$settings$reward_model,
          problem_i$settings$posterior_model,
          allocation_policy
        )
      )
      run$settings$base_allocation_policy <- allocation_policy
      run$settings$comparison_reward_model <- problem_i$settings$reward_model_canonical
      run$settings$comparison_posterior_model <- problem_i$settings$posterior_model_canonical

      df <- run$checkpoint_table
      df$problem_id <- problem_i$problem_id
      df$seed <- seed_i
      df$reward_model <- problem_i$settings$reward_model_canonical
      df$posterior_model <- problem_i$settings$posterior_model_canonical

      list(
        run_id = paste(problem_i$problem_id, label, seed_i, sep = "::"),
        run = run,
        checkpoint_table = df,
        reference = reference_i
      )
    }
  )

  runs <- lapply(task_runs, `[[`, "run")
  names(runs) <- vapply(task_runs, `[[`, character(1L), "run_id")
  results <- do.call(rbind, lapply(task_runs, `[[`, "checkpoint_table"))
  rownames(results) <- NULL
  summary <- bg_posterior_compare_summary(results)

  out <- structure(
    list(
      problems = problems,
      reward_model_map = posterior_models,
      results = results,
      summary = summary,
      runs = runs,
      settings = list(
        budgets = budgets,
        seeds = seeds,
        allocation_policy = allocation_policy,
        n_cores = n_cores,
        parallel = isTRUE(parallel),
        win_loss_unresolved_value = win_loss_unresolved_value
      )
    ),
    class = c("bg_reward_model_compare", "bg_method_compare")
  )
  if (!is.null(save_path)) {
    bg_study_save(out, save_path, overwrite = overwrite)
  }
  out
}

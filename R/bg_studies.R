# Study workflows and synthetic sanity checks.
#
# This file groups repeated-budget comparison workflows, posterior/reward-model
# studies, runtime profiling, and the small non-backgammon sanity lab.

# -----------------------------------------------------------------------------
# Source: bg_studies.R
# -----------------------------------------------------------------------------
# Repeated-study workflows.
#
# This file groups the higher-level study builders that sit above one-off runs:
# - repeated-seed TS/TTTS budget profiles;
# - multi-method comparison studies;
# - posterior-family and reward-stack sensitivity studies; and
# - lightweight runtime profiling of rollout-heavy components.
#
# The package treats these as research workflows, not core engine code. They
# should stay readable and explicit about what is being held fixed and what is
# being compared.
#' Profile Thompson sampling over budgets and seeds
#'
#' `bg_ts_profile()` is the main repeated-study function for a single decision
#' problem. It runs one Thompson-family path per seed up to the largest budget,
#' then slices the path at the requested budget checkpoints. This preserves the
#' canonical sequential semantics while making repeated budget studies much
#' cheaper than rerunning each budget from scratch.
#'
#' @param problem A `bg_problem` object.
#' @param budgets Integer-like budget vector.
#' @param seeds Integer-like seed vector.
#' @param proxy_reference Optional `bg_reference` object. If omitted, a proxy
#'   reference is built automatically.
#' @param reference_budget Integer-like budget used when auto-building the
#'   proxy reference.
#' @param allocation_policy Thompson-family policy.
#' @param ts_mode Either `"sequential"` or `"batched"`.
#' @param n_cores Integer-like worker count used for study-level parallel
#'   execution.
#' @param parallel Logical scalar; if `TRUE`, parallelize the repeated-seed
#'   sweep.
#' @param progress Logical scalar; if `TRUE`, report seed progress.
#' @param save_path Optional `.rds` path for the returned profile object.
#' @param overwrite Logical scalar controlling replacement of an existing
#'   `save_path`.
#' @param ... Additional arguments passed through to the underlying Thompson
#'   engine.
#'
#' @return A `bg_ts_profile` object.
#' @keywords internal
#' @noRd
bg_ts_profile <- function(
    problem,
    budgets = c(32L, 64L, 128L, 256L),
    seeds = 1:20,
    proxy_reference = NULL,
    reference_budget = max(4096L, 8L * max(budgets)),
    allocation_policy = c("thompson", "top_two_thompson"),
    ts_mode = c("sequential", "batched"),
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

  if (!inherits(problem, "bg_problem")) {
    stop("`problem` must inherit from class 'bg_problem'.", call. = FALSE)
  }

  budgets <- sort(unique(bg_normalize_study_budgets(budgets)))
  seeds <- sort(unique(bg_coerce_integerish(seeds, "seeds", length(seeds))))
  allocation_policy <- bg_match_allocation_policy_public(allocation_policy)
  ts_mode <- match.arg(ts_mode)
  n_cores <- bg_coerce_integerish(n_cores, "n_cores", 1L)
  bg_assert_scalar_flag(parallel, "parallel")
  bg_assert_scalar_flag(progress, "progress")
  bg_assert_scalar_flag(overwrite, "overwrite")

  if (is.null(proxy_reference)) {
    proxy_reference <- bg_reference(
      problem = problem,
      budget = reference_budget,
      seed = bg_derive_seed(min(seeds), "profile-reference")
    )
  }

  max_budget <- max(budgets)
  seed_runs <- bg_task_apply(
    tasks = as.list(seeds),
    n_cores = n_cores,
    parallel = parallel,
    progress = progress,
    progress_label = "TS profile runs",
    worker = function(seed_i) {
      run <- bg_ts_decide(
        problem = problem,
        budget = max_budget,
        allocation_policy = allocation_policy,
        proxy_reference = proxy_reference,
        checkpoints = budgets,
        ts_mode = ts_mode,
        seed = seed_i,
        ...
      )
      df <- run$checkpoint_table
      df$seed <- seed_i
      list(seed = seed_i, run = run, checkpoint_table = df)
    }
  )

  runs <- lapply(seed_runs, `[[`, "run")
  names(runs) <- vapply(seed_runs, function(x) as.character(x$seed), character(1L))
  results <- do.call(rbind, lapply(seed_runs, `[[`, "checkpoint_table"))
  rownames(results) <- NULL
  summary <- aggregate(
    results[, c(
      "recommended_prob_best",
      "simple_regret",
      "allocation_entropy",
      "allocation_hhi",
      "unresolved_fraction",
      "runtime_seconds",
      "rollout_throughput"
    )],
    by = list(
      allocation_policy = results$allocation_policy,
      checkpoint = results$checkpoint
    ),
    FUN = mean,
    na.rm = TRUE
  )
  summary <- merge(
    summary,
    bg_checkpoint_match_summary(results),
    by.x = c("allocation_policy", "checkpoint"),
    by.y = c("method", "checkpoint"),
    all.x = TRUE,
    sort = FALSE
  )
  summary <- summary[order(summary$allocation_policy, summary$checkpoint), , drop = FALSE]
  rownames(summary) <- NULL

  out <- structure(
    list(
      problem = problem,
      reference = proxy_reference,
      results = results,
      summary = summary,
      runs = runs,
      settings = list(
        budgets = budgets,
        seeds = seeds,
        allocation_policy = allocation_policy,
        ts_mode = ts_mode,
        n_cores = n_cores,
        parallel = isTRUE(parallel)
      )
    ),
    class = "bg_ts_profile"
  )
  if (!is.null(save_path)) {
    bg_study_save(out, save_path, overwrite = overwrite)
  }
  out
}

#' Compare Thompson sampling to baselines
#'
#' `bg_compare_methods()` compares Thompson-family methods and baselines on one
#' or many `bg_problem` objects. It is the internal comparison engine behind
#' the public `bg_compare_algorithms()` front door.
#'
#' @param problems A single `bg_problem` object or a list of `bg_problem`
#'   objects.
#' @param methods Character vector of allocation policies.
#' @param budgets Integer-like budget vector.
#' @param seeds Integer-like seed vector.
#' @param proxy_references Optional list of `bg_reference` objects aligned with
#'   `problems`, or a single `bg_reference` object for a single problem.
#' @param reference_budget Integer-like proxy-reference budget used when
#'   references are built automatically.
#' @param n_cores Integer-like worker count used for study-level parallel
#'   execution.
#' @param parallel Logical scalar; if `TRUE`, parallelize the repeated
#'   method-state-seed sweep.
#' @param progress Logical scalar; if `TRUE`, report nested-loop progress.
#' @param save_path Optional `.rds` path for the returned comparison object.
#' @param overwrite Logical scalar controlling replacement of an existing
#'   `save_path`.
#' @param ... Additional arguments passed to the underlying run functions.
#'
#' @return A `bg_method_compare` object.
bg_compare_methods <- function(
    problems,
    methods = c("thompson", "top_two_thompson", "equal"),
    budgets = c(32L, 64L, 128L, 256L),
    seeds = 1:20,
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
    stop("`problems` must be a `bg_problem` object or a non-empty list of `bg_problem` objects.", call. = FALSE)
  }

  methods <- unique(vapply(methods, bg_match_allocation_policy_public, character(1L), USE.NAMES = FALSE))
  budgets <- sort(unique(bg_normalize_study_budgets(budgets)))
  seeds <- sort(unique(bg_coerce_integerish(seeds, "seeds", length(seeds))))
  n_cores <- bg_coerce_integerish(n_cores, "n_cores", 1L)
  bg_assert_scalar_flag(parallel, "parallel")
  bg_assert_scalar_flag(progress, "progress")
  bg_assert_scalar_flag(overwrite, "overwrite")

  scalar_engine_methods <- methods[!vapply(methods, bg_is_thompson_policy_public, logical(1L))]
  incompatible_scalar <- vapply(problems, function(problem) !bg_problem_supports_legacy_scalar_engine(problem), logical(1L))
  if (length(scalar_engine_methods) > 0L && any(incompatible_scalar)) {
    stop(
      "Scalar-engine comparators (",
      paste(unique(scalar_engine_methods), collapse = ", "),
      ") are currently available only for `scalar_payoff + beta_pseudo` problems. ",
      "Use Thompson-family methods on richer model stacks, or rebuild the problem on the legacy scalar stack before comparing against scalar-engine baselines.",
      call. = FALSE
    )
  }

  if (is.null(proxy_references)) {
    proxy_references <- lapply(
      seq_along(problems),
      function(i) {
        bg_reference(
          problem = problems[[i]],
          budget = reference_budget,
          seed = bg_derive_seed(min(seeds), "compare-reference", i)
        )
      }
    )
  } else if (inherits(proxy_references, "bg_reference")) {
    proxy_references <- list(proxy_references)
  }

  if (length(proxy_references) != length(problems)) {
    stop("`proxy_references` must be NULL, a single `bg_reference`, or a list aligned with `problems`.", call. = FALSE)
  }

  task_grid <- expand.grid(
    p_idx = seq_along(problems),
    method = methods,
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
    progress_label = "method-comparison runs",
    worker = function(task) {
      p_idx <- task$p_idx[[1L]]
      method <- task$method[[1L]]
      seed <- task$seed[[1L]]
      problem <- problems[[p_idx]]
      reference <- proxy_references[[p_idx]]

      run <- if (bg_is_thompson_policy_public(method)) {
        bg_ts_decide(
          problem = problem,
          budget = max(budgets),
          allocation_policy = method,
          proxy_reference = reference,
          checkpoints = budgets,
          seed = seed,
          ...
        )
      } else {
        bg_run_method_path(
          problem = problem,
          allocation_policy = method,
          budget = max(budgets),
          checkpoints = budgets,
          reference = reference,
          seed = seed,
          ...
        )
      }

      run_id <- paste(problem$problem_id, method, seed, sep = "::")
      df <- run$checkpoint_table
      df$problem_id <- problem$problem_id
      df$seed <- seed
      list(run_id = run_id, run = run, checkpoint_table = df)
    }
  )

  runs <- lapply(task_runs, `[[`, "run")
  names(runs) <- vapply(task_runs, `[[`, character(1L), "run_id")
  results <- do.call(rbind, lapply(task_runs, `[[`, "checkpoint_table"))
  rownames(results) <- NULL

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
    by.x = c("method", "checkpoint"),
    by.y = c("method", "checkpoint"),
    all.x = TRUE,
    sort = FALSE
  )
  summary <- summary[order(summary$method, summary$checkpoint), , drop = FALSE]
  rownames(summary) <- NULL

  out <- structure(
    list(
      problems = problems,
      references = proxy_references,
      results = results,
      summary = summary,
      runs = runs,
      settings = list(
        methods = methods,
        budgets = budgets,
        seeds = seeds,
        n_cores = n_cores,
        parallel = isTRUE(parallel)
      )
    ),
    class = "bg_method_compare"
  )
  if (!is.null(save_path)) {
    bg_study_save(out, save_path, overwrite = overwrite)
  }
  out
}

# -----------------------------------------------------------------------------
# Posterior-family and reward-stack sensitivity studies
# -----------------------------------------------------------------------------

bg_default_reward_model_map <- function() {
  # Default one-to-one mapping from reward definitions to their canonical
  # posterior families for reward-stack comparisons.
  c(
    scalar_payoff = "beta_pseudo",
    categorical_outcome = "dirichlet_multinomial",
    win_loss = "beta_bernoulli"
  )
}

bg_posterior_compare_summary <- function(results) {
  # Collapse repeated-seed checkpoint results to one mean summary table per
  # method and budget.
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
  # Compare posterior families while holding the reward model fixed.
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
  # Compare coherent reward/posterior stacks under one common study protocol.
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

# -----------------------------------------------------------------------------
# Runtime profiling helpers
# -----------------------------------------------------------------------------

#' Profile rollout runtime components
#'
#' Profiles key runtime components for random-policy simulation workloads:
#' legal move generation, move application, one-rollout cost, and batched
#' rollout evaluation cost.
#'
#' @param board A `bg_board` object.
#' @param roll A `bg_roll` object.
#' @param legal_reps Number of repetitions for legal-move generation timing.
#' @param apply_reps Number of repetitions for move-application timing.
#' @param one_rollout_reps Number of repetitions for one-rollout timing.
#' @param total_budget Rollout budget for batched rollout timing.
#' @param rollout_policy Rollout baseline policy.
#' @param max_rollout_turns Maximum turns for each rollout.
#' @param seed Optional integer-like seed for reproducibility.
#'
#' @return A named list with timing values in seconds.
#' @keywords internal
#' @noRd
bg_profile_runtime <- function(
    board,
    roll,
    legal_reps = 200L,
    apply_reps = 2000L,
    one_rollout_reps = 25L,
    total_budget = 256L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 200L,
    seed = NULL) {
  if (!is_bg_board(board)) {
    stop("`board` must inherit from class 'bg_board'.", call. = FALSE)
  }
  bg_validate_board(board)
  roll <- bg_as_roll(roll)

  legal_reps <- bg_coerce_integerish(legal_reps, "legal_reps", 1L)
  apply_reps <- bg_coerce_integerish(apply_reps, "apply_reps", 1L)
  one_rollout_reps <- bg_coerce_integerish(one_rollout_reps, "one_rollout_reps", 1L)
  total_budget <- bg_coerce_integerish(total_budget, "total_budget", 1L)
  rollout_policy <- bg_match_rollout_policy(rollout_policy)
  max_rollout_turns <- bg_coerce_integerish(max_rollout_turns, "max_rollout_turns", 1L)

  seed_args <- bg_normalize_seed_args(seed)

  out <- bg_cpp_profile_rollout_runtime(
    unclass(board),
    unclass(roll),
    legal_reps,
    apply_reps,
    one_rollout_reps,
    total_budget,
    rollout_policy,
    max_rollout_turns,
    seed_args$seed,
    seed_args$use_seed
  )

  out$settings <- list(
    legal_reps = legal_reps,
    apply_reps = apply_reps,
    one_rollout_reps = one_rollout_reps,
    total_budget = total_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns,
    seed = if (is.null(seed)) NULL else bg_coerce_integerish(seed, "seed", 1L)
  )
  class(out) <- "bg_runtime_profile"
  out
}

#' @export
#' @noRd
print.bg_runtime_profile <- function(x, ...) {
  cat("<bg_runtime_profile>\n", sep = "")
  cat("n_legal_moves:            ", x$n_legal_moves[[1L]], "\n", sep = "")
  cat("legal_generation_seconds: ", format(x$legal_generation_seconds[[1L]], digits = 6), "\n", sep = "")
  cat("move_application_seconds: ", format(x$move_application_seconds[[1L]], digits = 6), "\n", sep = "")
  cat("one_rollout_seconds:      ", format(x$one_rollout_seconds[[1L]], digits = 6), "\n", sep = "")
  cat("batched_rollout_seconds:  ", format(x$batched_rollout_seconds[[1L]], digits = 6), "\n", sep = "")
  invisible(x)
}

# -----------------------------------------------------------------------------
# Source: bg_sanity_lab.R
# -----------------------------------------------------------------------------
# Lightweight synthetic lab for separating TS behavior from backgammon rollouts.
#
# The sanity lab is not a second package or a generic bandit framework. It is a
# deliberately small set of synthetic cases whose only purpose is to answer:
# are the patterns we see in backgammon openings generic pure-exploration
# effects, or artifacts of the rollout simulator?

bg_sanity_case_library <- function() {
  list(
    bernoulli_clear = list(
      case_id = "bernoulli_clear",
      case_group = "clear_gap",
      case_type = "bernoulli",
      truth_mean = c(0.62, 0.57, 0.46, 0.39)
    ),
    bernoulli_near_tie = list(
      case_id = "bernoulli_near_tie",
      case_group = "near_tie",
      case_type = "bernoulli",
      truth_mean = c(0.545, 0.54, 0.48, 0.35, 0.3)
    ),
    bernoulli_many_dominated = list(
      case_id = "bernoulli_many_dominated",
      case_group = "many_dominated",
      case_type = "bernoulli",
      truth_mean = c(0.61, 0.31, 0.29, 0.27, 0.25, 0.23, 0.21)
    ),
    bernoulli_many_near_optimal = list(
      case_id = "bernoulli_many_near_optimal",
      case_group = "many_near_optimal",
      case_type = "bernoulli",
      truth_mean = c(0.552, 0.551, 0.549, 0.547, 0.38, 0.31)
    ),
    categorical_variance_asymmetry = list(
      case_id = "categorical_variance_asymmetry",
      case_group = "variance_asymmetry",
      case_type = "categorical",
      payoff = c(0, 0.25, 0.75, 1),
      truth_prob = rbind(
        c(0.02, 0.84, 0.12, 0.02),
        c(0.44, 0.04, 0.06, 0.46),
        c(0.55, 0.08, 0.06, 0.31),
        c(0.70, 0.10, 0.08, 0.12)
      )
    ),
    categorical_multimodal_bounded = list(
      case_id = "categorical_multimodal_bounded",
      case_group = "multimodal_bounded",
      case_type = "categorical",
      payoff = c(0, 0.2, 0.5, 0.8, 1),
      truth_prob = rbind(
        c(0.45, 0.00, 0.05, 0.00, 0.50),
        c(0.35, 0.05, 0.10, 0.05, 0.45),
        c(0.15, 0.15, 0.45, 0.15, 0.10),
        c(0.60, 0.10, 0.10, 0.10, 0.10),
        c(0.20, 0.20, 0.20, 0.20, 0.20)
      )
    )
  )
}

bg_sanity_case_truth <- function(case) {
  if (identical(case$case_type, "bernoulli")) {
    truth_mean <- as.numeric(case$truth_mean)
  } else {
    payoff <- as.numeric(case$payoff)
    truth_mean <- as.numeric(case$truth_prob %*% payoff)
  }

  rank_order <- order(-truth_mean, seq_along(truth_mean))
  truth_rank <- integer(length(truth_mean))
  truth_rank[rank_order] <- seq_along(rank_order)

  list(
    truth_mean = truth_mean,
    truth_rank = truth_rank,
    best_arm = rank_order[[1L]],
    top_two = utils::head(rank_order, 2L),
    top_k = utils::head(rank_order, min(3L, length(rank_order)))
  )
}

bg_sanity_supported_policies <- function() {
  c("equal", "thompson", "top_two_thompson")
}

bg_sanity_match_policy <- function(allocation_policy) {
  allocation_policy <- bg_match_allocation_policy_public(allocation_policy)
  if (!allocation_policy %in% bg_sanity_supported_policies()) {
    stop(
      "`bg_sanity_lab()` currently supports only `equal`, `thompson`, and `top_two_thompson`.",
      call. = FALSE
    )
  }
  allocation_policy
}

bg_sanity_draw_scores <- function(case, state) {
  n_arms <- length(state$allocation_count)

  if (identical(case$case_type, "bernoulli")) {
    scores <- numeric(n_arms)
    for (arm in seq_len(n_arms)) {
      scores[[arm]] <- stats::rbeta(1L, state$success[[arm]] + 1, state$failure[[arm]] + 1)
    }
    return(scores)
  }

  payoff <- as.numeric(case$payoff)
  scores <- numeric(n_arms)
  for (arm in seq_len(n_arms)) {
    alpha <- state$category_count[arm, ] + 1
    gamma_draw <- stats::rgamma(length(alpha), shape = alpha, rate = 1)
    probs <- gamma_draw / sum(gamma_draw)
    scores[[arm]] <- sum(probs * payoff)
  }
  scores
}

bg_sanity_posterior_mean <- function(case, state) {
  n_arms <- length(state$allocation_count)

  if (identical(case$case_type, "bernoulli")) {
    return((state$success + 1) / (state$success + state$failure + 2))
  }

  payoff <- as.numeric(case$payoff)
  out <- numeric(n_arms)
  for (arm in seq_len(n_arms)) {
    alpha <- state$category_count[arm, ] + 1
    out[[arm]] <- sum((alpha / sum(alpha)) * payoff)
  }
  out
}

bg_sanity_sample_arm <- function(case, arm, state) {
  if (identical(case$case_type, "bernoulli")) {
    reward <- stats::rbinom(1L, size = 1L, prob = case$truth_mean[[arm]])
    state$success[[arm]] <- state$success[[arm]] + reward
    state$failure[[arm]] <- state$failure[[arm]] + (1L - reward)
    state$reward_sum[[arm]] <- state$reward_sum[[arm]] + reward
    state$reward_sum_sq[[arm]] <- state$reward_sum_sq[[arm]] + reward^2
  } else {
    draw <- sample.int(
      ncol(case$truth_prob),
      size = 1L,
      prob = case$truth_prob[arm, ]
    )
    reward <- case$payoff[[draw]]
    state$category_count[arm, draw] <- state$category_count[arm, draw] + 1L
    state$reward_sum[[arm]] <- state$reward_sum[[arm]] + reward
    state$reward_sum_sq[[arm]] <- state$reward_sum_sq[[arm]] + reward^2
  }

  state$allocation_count[[arm]] <- state$allocation_count[[arm]] + 1L
  state
}

bg_sanity_choose_arm <- function(case, state, allocation_policy, ttts_beta) {
  n_arms <- length(state$allocation_count)
  if (allocation_policy == "equal") {
    return(which.min(state$allocation_count))
  }

  scores <- bg_sanity_draw_scores(case, state)
  top1 <- bg_posterior_pick_index(scores, state$allocation_count)
  if (allocation_policy == "thompson" || n_arms == 1L) {
    return(top1)
  }

  if (stats::runif(1L) <= ttts_beta) {
    return(top1)
  }

  for (attempt in seq_len(64L)) {
    alt_scores <- bg_sanity_draw_scores(case, state)
    top2 <- bg_posterior_pick_index(alt_scores, state$allocation_count)
    if (top2 != top1) {
      return(top2)
    }
  }

  top1
}

bg_sanity_run_one <- function(case, allocation_policy, budget, seed, ttts_beta) {
  truth <- bg_sanity_case_truth(case)
  n_arms <- length(truth$truth_mean)
  state <- list(
    allocation_count = integer(n_arms),
    reward_sum = numeric(n_arms),
    reward_sum_sq = numeric(n_arms),
    success = integer(n_arms),
    failure = integer(n_arms),
    category_count = matrix(0L, nrow = n_arms, ncol = if (identical(case$case_type, "categorical")) ncol(case$truth_prob) else 0L)
  )

  state <- bg_ts_with_seed(seed, {
    warm_start <- min(n_arms, budget)
    for (arm in seq_len(warm_start)) {
      state <- bg_sanity_sample_arm(case, arm, state)
    }
    if (budget > warm_start) {
      for (step in seq_len(budget - warm_start)) {
        arm <- bg_sanity_choose_arm(case, state, allocation_policy, ttts_beta)
        state <- bg_sanity_sample_arm(case, arm, state)
      }
    }
    state
  })

  posterior_mean <- bg_sanity_posterior_mean(case, state)
  selected_arm <- which.max(posterior_mean)
  concentration <- bg_allocation_concentration(state$allocation_count)
  truth_gap <- truth$truth_mean[truth$top_two[[1L]]] - truth$truth_mean[truth$top_two[[2L]]]
  truth_gaps <- max(truth$truth_mean) - truth$truth_mean
  names(truth_gaps) <- seq_along(truth_gaps)
  restricted_pairwise <- bg_eval_pairwise_metrics(
    ids = as.character(truth$top_k),
    est_values = stats::setNames(posterior_mean, seq_along(posterior_mean)),
    truth_values = stats::setNames(truth$truth_mean, seq_along(truth$truth_mean))
  )

  data.frame(
    case_id = case$case_id,
    case_group = case$case_group,
    case_type = case$case_type,
    allocation_policy = allocation_policy,
    seed = seed,
    budget = budget,
    n_arms = n_arms,
    selected_arm = selected_arm,
    truth_best_arm = truth$best_arm,
    selected_truth_rank = truth$truth_rank[[selected_arm]],
    top1_match = selected_arm == truth$best_arm,
    simple_regret = max(truth$truth_mean) - truth$truth_mean[[selected_arm]],
    truth_top2_hit = truth$truth_rank[[selected_arm]] <= 2L,
    truth_top_k_hit = truth$truth_rank[[selected_arm]] <= length(truth$top_k),
    share_truth_best = state$allocation_count[[truth$best_arm]] / budget,
    share_truth_top2 = sum(state$allocation_count[truth$top_two]) / budget,
    share_truth_top_k = sum(state$allocation_count[truth$top_k]) / budget,
    share_screened_suboptimal = sum(state$allocation_count[truth_gaps >= 0.1]) / budget,
    gap_weighted_wasted_allocation = sum((state$allocation_count / budget) * truth_gaps),
    allocation_entropy = concentration$entropy,
    allocation_hhi = concentration$hhi,
    allocation_hhi_normalized = concentration$hhi_normalized,
    posterior_best_mean = max(posterior_mean),
    truth_gap = truth_gap,
    restricted_pairwise_ordering_accuracy = restricted_pairwise$pairwise_ordering_accuracy,
    stringsAsFactors = FALSE
  )
}

#' Run a lightweight synthetic sanity lab for TS and TTTS
#'
#' `bg_sanity_lab()` runs a small set of non-backgammon allocation problems with
#' known truth so Thompson behavior can be checked without rollout-engine
#' confounding.
#'
#' The lab is intentionally narrow:
#'
#' - Bernoulli arms with known means;
#' - bounded categorical arms with a known payoff map;
#' - problem geometries chosen to mimic near ties, many dominated arms, many
#'   near-optimal arms, variance asymmetry, and multimodal bounded rewards;
#' - only `equal`, `thompson`, and `top_two_thompson`.
#'
#' This is a sanity screen for allocation behavior, not a second package inside
#' the package.
#'
#' @param allocation_policies Character vector of supported policies.
#' @param budget Integer-like total budget per run.
#' @param seeds Integer-like seed vector.
#' @param ttts_beta Probability of keeping the Thompson winner in TTTS.
#'
#' @return A list with `cases`, `results`, and `summary`.
#' @export
bg_sanity_lab <- function(
    allocation_policies = c("thompson", "top_two_thompson", "equal"),
    budget = 256L,
    seeds = 1:20,
    ttts_beta = 0.5) {
  budget <- bg_coerce_integerish(budget, "budget", 1L)
  if (budget < 1L) {
    stop("`budget` must be at least 1.", call. = FALSE)
  }
  seeds <- bg_coerce_integerish(seeds, "seeds", length(seeds))
  if (length(seeds) < 1L) {
    stop("`seeds` must contain at least one value.", call. = FALSE)
  }

  allocation_policies <- unique(vapply(
    allocation_policies,
    bg_sanity_match_policy,
    character(1L),
    USE.NAMES = FALSE
  ))

  cases <- bg_sanity_case_library()
  case_table <- do.call(
    rbind,
    lapply(
      cases,
      function(case) {
        truth <- bg_sanity_case_truth(case)
        data.frame(
          case_id = case$case_id,
          case_group = case$case_group,
          case_type = case$case_type,
          n_arms = length(truth$truth_mean),
          truth_best_arm = truth$best_arm,
          top_two_gap = truth$truth_mean[truth$top_two[[1L]]] - truth$truth_mean[truth$top_two[[2L]]],
          stringsAsFactors = FALSE
        )
      }
    )
  )

  results <- do.call(
    rbind,
    lapply(
      allocation_policies,
      function(policy) {
        do.call(
          rbind,
          lapply(
            cases,
            function(case) {
              do.call(
                rbind,
                lapply(
                  seeds,
                  function(seed) {
                    bg_sanity_run_one(
                      case = case,
                      allocation_policy = policy,
                      budget = budget,
                      seed = seed,
                      ttts_beta = ttts_beta
                    )
                  }
                )
              )
            }
          )
        )
      }
    )
  )

  summary <- aggregate(
    results[c(
      "top1_match",
      "simple_regret",
      "truth_top2_hit",
      "truth_top_k_hit",
      "share_truth_best",
      "share_truth_top2",
      "share_truth_top_k",
      "share_screened_suboptimal",
      "gap_weighted_wasted_allocation",
      "allocation_entropy",
      "allocation_hhi_normalized",
      "restricted_pairwise_ordering_accuracy"
    )],
    by = list(
      case_id = results$case_id,
      case_group = results$case_group,
      allocation_policy = results$allocation_policy
    ),
    FUN = mean
  )
  recommendation_instability <- lapply(
    split(results, interaction(results$case_id, results$allocation_policy, drop = TRUE)),
    function(df) {
      recommendation_modal_share <- bg_truth_modal_share(df$selected_arm)
      data.frame(
        case_id = df$case_id[[1L]],
        allocation_policy = df$allocation_policy[[1L]],
        recommendation_instability = if (is.na(recommendation_modal_share)) NA_real_ else 1 - recommendation_modal_share,
        stringsAsFactors = FALSE
      )
    }
  )
  recommendation_instability <- do.call(rbind, recommendation_instability)
  summary <- merge(
    summary,
    recommendation_instability,
    by = c("case_id", "allocation_policy"),
    all.x = TRUE,
    sort = FALSE
  )

  list(
    cases = case_table,
    results = results,
    summary = summary
  )
}

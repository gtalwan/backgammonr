# Repeated Thompson-study helpers that compare budgets, seeds, and policy variants.
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
    methods = c("thompson", "top_two_thompson", "ucb", "equal"),
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

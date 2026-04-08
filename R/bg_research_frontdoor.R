# Research-facing wrappers that keep the current engine semantics intact while
# presenting a cleaner function family around Thompson sampling.

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

bg_default_study_cache_dir <- function(scope = c("profile", "comparison", "state_battery", "opening", "traces", "crn")) {
  scope <- match.arg(scope)
  file.path(tools::R_user_dir("backgammonr", which = "cache"), "study", scope)
}

bg_truth_problem_hash <- function(problem) {
  key <- bg_problem_key(
    board = problem$board,
    roll = problem$roll,
    simulation_policy_engine = problem$settings$simulation_policy_engine,
    max_rollout_turns = problem$settings$max_rollout_turns,
    unresolved_value = problem$settings$unresolved_value,
    reward_model = problem$settings$reward_model_canonical,
    posterior_model = problem$settings$posterior_model_canonical,
    model_signature = problem$settings$model_signature
  )
  substr(key, 1L, 16L)
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
    scoring = paste(
      "`", problem$settings$reward_model, "` reward with unresolved rollouts mapped to ",
      format(problem$settings$unresolved_value, digits = 4L),
      "; posterior summaries use `", problem$settings$posterior_model, "`."
    )
  )
}

bg_truth_storage_path <- function(problem, scope = c("state", "opening", "battery"), cache_dir = NULL) {
  scope <- match.arg(scope)
  if (is.null(cache_dir)) {
    cache_dir <- bg_default_truth_cache_dir(scope)
  }
  file.path(
    cache_dir,
    paste0(
      bg_safe_file_label(problem$problem_id),
      "_",
      bg_safe_file_label(bg_truth_problem_hash(problem)),
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
      "tempered_thompson",
      "budget_aware_thompson",
      "elimination_thompson",
      "ranking_aware_thompson"
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
#' This is the clean front-door alias for the uniform allocation baseline.
#' It currently uses the legacy scalar allocation engine and is therefore
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
bg_uniform_run <- function(
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
#' Non-Thompson comparators currently remain on the legacy scalar engine. That
#' means methods such as `"ucb"` and `"equal"` are only available for
#' `scalar_payoff + beta_pseudo` problems in this rescue pass.
#'
#' @param problems A `bg_problem` object or list of them.
#' @param ... Passed to [bg_compare_methods()].
#'
#' @return A `bg_method_compare` object.
#' @export
bg_compare_algorithms <- function(
    problems,
    methods = c("thompson", "top_two_thompson", "ucb", "equal"),
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

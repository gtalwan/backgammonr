# Truth-object construction, normalization, persistence, and diagnostics.
# Truth state objects are intentionally thin wrappers around one problem and one
# proxy reference.
bg_new_truth_state <- function(x) {
  x$problem <- x$problem
  x$reference <- bg_normalize_truth_reference(x$reference, "x$reference")
  x$summary <- bg_truth_state_summary_aliases(x$summary)
  structure(x, class = "bg_truth_state")
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
  problem_hash <- bg_truth_problem_hash(problem)
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
      problem_hash = problem_hash,
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
    target_path <- bg_truth_storage_path(problem, scope = "state", cache_dir = cache_dir)
  }

  # Reuse an existing saved truth object when the stored problem hash still
  # matches the current problem definition.
  cached_truth <- NULL
  if (!is.null(target_path) && file.exists(target_path) && !isTRUE(overwrite)) {
    maybe_cached <- bg_truth_load(target_path)
    if (inherits(maybe_cached, "bg_truth_state") &&
        identical(maybe_cached$metadata$problem_hash, bg_truth_problem_hash(problem))) {
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
      state_path <- bg_truth_storage_path(problem, scope = "battery", cache_dir = cache_dir)
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

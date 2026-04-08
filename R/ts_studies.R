# Internal opening, game-trace, and structure study workflows.
bg_opening_roll_grid <- function(include_doubles = TRUE) {
  bg_assert_scalar_flag(include_doubles, "include_doubles")

  out <- list()
  for (die1 in 1:6) {
    for (die2 in die1:6) {
      if (!isTRUE(include_doubles) && die1 == die2) {
        next
      }
      out[[length(out) + 1L]] <- list(
        roll = bg_roll(die1, die2),
        roll_label = paste0(die1, "-", die2),
        die1 = die1,
        die2 = die2,
        is_double = die1 == die2,
        roll_group = if (die1 == die2) "double" else "non_double"
      )
    }
  }
  out
}

bg_opening_truth_lookup <- function(truth) {
  if (!inherits(truth, "bg_truth_battery")) {
    stop("`truth` must inherit from class 'bg_truth_battery'.", call. = FALSE)
  }

  lookup <- truth$summary
  keep <- intersect(
    c(
      "problem_id",
      "opening_roll",
      "die1",
      "die2",
      "is_double",
      "roll_group",
      "n_moves",
      "best_move_label",
      "top_two_gap_estimate",
      "top_two_gap_mc_lower_95",
      "mc_gap_excludes_zero",
      "difficulty_label",
      "n_near_optimal",
      "mc_not_separated_from_best_set_size",
      "mean_reference_se"
    ),
    names(lookup)
  )
  lookup <- lookup[, keep, drop = FALSE]
  if ("best_move_label" %in% names(lookup)) {
    names(lookup)[names(lookup) == "best_move_label"] <- "truth_best_move_label"
  }
  lookup[order(lookup$die1, lookup$die2), , drop = FALSE]
}

bg_opening_attach_lookup <- function(df, lookup) {
  if (nrow(df) == 0L) {
    return(df)
  }
  idx <- match(df$problem_id, lookup$problem_id)
  cbind(df, lookup[idx, setdiff(names(lookup), "problem_id"), drop = FALSE], stringsAsFactors = FALSE)
}

bg_opening_metric_leaderboard <- function(metric_panel) {
  aggregate(
    metric_panel[, c(
      "top1_match",
      "simple_regret",
      "spearman",
      "top_k_overlap",
      "share_top_k_truth",
      "share_mc_screened_suboptimal",
      "allocation_entropy"
    )],
    by = list(
      opening_roll = metric_panel$opening_roll,
      roll_group = metric_panel$roll_group,
      method = metric_panel$allocation_policy,
      checkpoint = metric_panel$checkpoint
    ),
    FUN = mean,
    na.rm = TRUE
  )
}

bg_opening_budget_summary <- function(metric_panel) {
  aggregate(
    metric_panel[, c(
      "top1_match",
      "simple_regret",
      "spearman",
      "top_k_overlap",
      "share_top_k_truth",
      "share_mc_screened_suboptimal",
      "allocation_entropy"
    )],
    by = list(
      roll_group = metric_panel$roll_group,
      method = metric_panel$allocation_policy,
      checkpoint = metric_panel$checkpoint
    ),
    FUN = mean,
    na.rm = TRUE
  )
}

bg_progress_step <- function(pb, value) {
  if (!is.null(pb)) {
    utils::setTxtProgressBar(pb, value)
  }
}

#' Benchmark opening-roll decision problems
#'
#' `bg_opening_study()` turns the initial board into a flagship repeated-study
#' workflow by enumerating the unordered opening-roll battery, building or
#' loading cached proxy-reference objects, and comparing Thompson-family and
#' baseline allocation rules over repeated budgets and seeds.
#'
#' @param budgets Integer-like budget vector.
#' @param seeds Integer-like seed vector.
#' @param methods Allocation policies to compare.
#' @param simulation_policy Continuation policy inside rollouts.
#' @param include_doubles Logical scalar; if `TRUE`, analyze all 21 unordered
#'   opening rolls. If `FALSE`, keep only the 15 non-double rolls.
#' @param truth Optional `bg_truth_battery` object from [bg_truth_opening()].
#' @param truth_path Optional `.rds` path used to load or save the opening truth
#'   battery.
#' @param reference_budget Integer-like proxy-reference budget per opening roll
#'   when `truth` is not supplied.
#' @param workers_truth Number of workers used for proxy-reference generation.
#' @param truth_block_size Integer-like block size for proxy-reference
#'   simulation.
#' @param cache Logical scalar; if `TRUE`, reuse cached per-roll truth files
#'   when building truth objects.
#' @param cache_dir Optional cache directory for per-roll truth files.
#' @param save_path Optional `.rds` path for the returned study object.
#' @param overwrite Logical scalar controlling whether saved truth/study objects
#'   may be replaced.
#' @param top_k Integer-like top-k value used when building the evaluation
#'   panel.
#' @param verbose Logical scalar; if `TRUE`, show progress for long studies.
#'
#' @return A `bg_opening_study` object.
#' @keywords internal
#' @noRd
bg_opening_study <- function(
    budgets = c(32L, 64L, 128L, 256L),
    seeds = 1:20,
    methods = c("thompson", "top_two_thompson", "ucb", "equal"),
    simulation_policy = c("random", "heuristic", "aggressive", "defensive", "ts_local"),
    include_doubles = TRUE,
    truth = NULL,
    truth_path = NULL,
    reference_budget = max(4096L, 8L * max(budgets)),
    workers_truth = bg_default_workers_truth(),
    n_cores = 1L,
    parallel = FALSE,
    truth_block_size = 128L,
    cache = TRUE,
    cache_dir = NULL,
    save_path = NULL,
    overwrite = FALSE,
    top_k = 3L,
    verbose = interactive()) {
  cached <- bg_maybe_load_saved_study(save_path, overwrite = overwrite)
  if (!is.null(cached)) {
    return(cached)
  }

  budgets <- sort(unique(bg_normalize_study_budgets(budgets)))
  seeds <- sort(unique(bg_coerce_integerish(seeds, "seeds", length(seeds))))
  methods <- unique(vapply(methods, bg_match_allocation_policy_public, character(1L), USE.NAMES = FALSE))
  n_cores <- bg_coerce_integerish(n_cores, "n_cores", 1L)
  bg_assert_scalar_flag(parallel, "parallel")
  bg_assert_scalar_flag(verbose, "verbose")
  bg_assert_scalar_flag(cache, "cache")
  bg_assert_scalar_flag(overwrite, "overwrite")
  bg_assert_scalar_flag(include_doubles, "include_doubles")
  top_k <- bg_coerce_integerish(top_k, "top_k", 1L)

  if (is.null(truth)) {
    if (!is.null(truth_path) && file.exists(truth_path) && !isTRUE(overwrite)) {
      truth <- bg_truth_load(truth_path)
    } else {
      truth <- bg_truth_opening(
        budget = reference_budget,
        simulation_policy = simulation_policy,
        include_doubles = include_doubles,
        n_cores = workers_truth,
        parallel = workers_truth > 1L,
        truth_block_size = truth_block_size,
        cache = cache,
        cache_dir = cache_dir,
        save_path = truth_path,
        overwrite = overwrite,
        seed = bg_derive_seed(min(seeds), "opening-truth", reference_budget, include_doubles),
        verbose = verbose
      )
    }
  }
  if (!inherits(truth, "bg_truth_battery")) {
    stop("`truth` must inherit from class 'bg_truth_battery'.", call. = FALSE)
  }

  lookup <- bg_opening_truth_lookup(truth)
  problems <- lapply(truth$truths, `[[`, "problem")
  references <- lapply(truth$truths, `[[`, "reference")

  comparison <- bg_compare_methods(
    problems = problems,
    methods = methods,
    budgets = budgets,
    seeds = seeds,
    proxy_references = references,
    n_cores = n_cores,
    parallel = parallel,
    progress = verbose
  )

  metric_panel <- bg_opening_attach_lookup(
    bg_eval_reference_aware(
      x = comparison,
      truth = truth,
      top_k = top_k
    ),
    lookup = lookup
  )

  roll_summary <- bg_opening_attach_lookup(comparison$results, lookup = lookup)
  difficulty_table <- lookup[order(lookup$top_two_gap_estimate), , drop = FALSE]
  rownames(difficulty_table) <- NULL
  seed_stability <- bg_opening_attach_lookup(
    bg_eval_seed_stability(comparison, truth = truth, metric = "simple_regret"),
    lookup = lookup
  )

  leaderboard <- bg_opening_metric_leaderboard(metric_panel)
  budget_summary <- bg_opening_budget_summary(metric_panel)

  out <- structure(
    list(
      truth = truth,
      problems = problems,
      references = references,
      comparison = comparison,
      roll_summary = roll_summary,
      metric_panel = metric_panel,
      seed_stability = seed_stability,
      difficulty_table = difficulty_table,
      leaderboard = leaderboard,
      budget_summary = budget_summary,
      settings = list(
        budgets = budgets,
        seeds = seeds,
        methods = methods,
        simulation_policy = bg_match_simulation_policy_public(simulation_policy),
        include_doubles = include_doubles,
        reference_budget = reference_budget,
        truth_path = truth_path,
        top_k = top_k,
        n_cores = n_cores,
        parallel = isTRUE(parallel)
      )
    ),
    class = "bg_opening_study"
  )

  if (!is.null(save_path)) {
    bg_study_save(out, save_path, overwrite = overwrite)
  }

  out
}

bg_game_trace_problems_from_game <- function(game, simulation_policy) {
  turns <- game$turns
  problems <- vector("list", length(turns))
  for (i in seq_along(turns)) {
    problems[[i]] <- bg_problem(
      state = turns[[i]]$board_before,
      roll = turns[[i]]$roll,
      simulation_policy = simulation_policy,
      problem_id = paste0("turn_", i)
    )
  }
  problems
}

#' Study Thompson behavior through a game trace
#'
#' `bg_game_trace()` analyzes a sequence of local decision problems extracted
#' from a supplied game trace or from a sampled game. It is intended for
#' move-by-move TS behavior studies, not exhaustive proxy-reference analysis at
#' every node.
#'
#' @param game Optional `bg_game_result` object.
#' @param problems Optional list of `bg_problem` objects.
#' @param board Optional starting board used when sampling a game.
#' @param sample_selection Selection policy used when sampling a game.
#' @param simulation_policy Continuation policy for the local TS problems.
#' @param local_budget Integer-like TS budget per node.
#' @param seeds Integer-like vector of TS seeds.
#' @param n_reference_nodes Integer-like number of nodes that receive proxy
#'   references. Nodes are selected by largest action count.
#' @param reference_budget Proxy-reference budget for selected nodes.
#' @param max_turns Integer-like game length cap when sampling a game.
#' @param seed Optional integer-like seed.
#'
#' @return A `bg_game_trace` object.
#' @export
bg_game_trace <- function(
    game = NULL,
    problems = NULL,
    board = bg_initial_board(),
    sample_selection = c("random", "aggressive", "defensive"),
    simulation_policy = c("random", "heuristic", "aggressive", "defensive", "ts_local"),
    local_budget = 128L,
    seeds = 1:5,
    n_reference_nodes = 3L,
    reference_budget = 2048L,
    max_turns = 40L,
    seed = NULL) {
  if (is.null(game) && is.null(problems)) {
    sample_selection <- match.arg(sample_selection)
    game <- bg_play_game(
      board = board,
      max_turns = max_turns,
      selection = sample_selection,
      seed = seed
    )
  }

  if (!is.null(game) && !inherits(game, "bg_game_result")) {
    stop("`game` must inherit from class 'bg_game_result' when supplied.", call. = FALSE)
  }

  if (!is.null(game)) {
    problems <- bg_game_trace_problems_from_game(game, simulation_policy = simulation_policy)
  }

  if (!is.list(problems) || length(problems) < 1L || !all(vapply(problems, inherits, logical(1L), what = "bg_problem"))) {
    stop("`problems` must be a non-empty list of `bg_problem` objects.", call. = FALSE)
  }

  local_budget <- bg_coerce_integerish(local_budget, "local_budget", 1L)
  seeds <- sort(unique(bg_coerce_integerish(seeds, "seeds", length(seeds))))
  n_reference_nodes <- bg_coerce_integerish(n_reference_nodes, "n_reference_nodes", 1L)

  action_counts <- vapply(problems, function(problem) nrow(problem$candidate_table), integer(1L))
  node_priority <- order(action_counts, decreasing = TRUE)
  reference_candidates <- node_priority[action_counts[node_priority] > 0L]
  reference_nodes <- head(reference_candidates, min(n_reference_nodes, length(reference_candidates)))
  references <- vector("list", length(problems))

  rows <- list()
  runs <- list()
  row_id <- 1L

  for (node_index in seq_along(problems)) {
    problem <- problems[[node_index]]
    if (node_index %in% reference_nodes) {
      references[[node_index]] <- bg_reference(
        problem = problem,
        budget = reference_budget,
        seed = bg_derive_seed(seed, "game-trace-reference", node_index)
      )
    }

    for (run_seed in seeds) {
      run <- bg_ts_decide(
        problem = problem,
        budget = local_budget,
        proxy_reference = references[[node_index]],
        seed = run_seed
      )
      runs[[paste0(problem$problem_id, "::", run_seed)]] <- run
      row <- run$checkpoint_table[run$checkpoint_table$checkpoint == local_budget, , drop = FALSE]
      row$node_index <- node_index
      row$problem_id <- problem$problem_id
      row$seed <- run_seed
      row$phase <- if (node_index <= 4L) {
        "opening"
      } else if (node_index <= 20L) {
        "midgame"
      } else {
        "endgame"
      }
      rows[[row_id]] <- row
      row_id <- row_id + 1L
    }
  }

  node_table <- do.call(rbind, rows)
  rownames(node_table) <- NULL
  node_summary <- aggregate(
    node_table[, c("recommended_prob_best", "simple_regret", "allocation_entropy", "runtime_seconds")],
    by = list(phase = node_table$phase),
    FUN = mean,
    na.rm = TRUE
  )

  structure(
    list(
      game = game,
      problems = problems,
      references = references,
      node_table = node_table,
      summary = node_summary,
      runs = runs,
      settings = list(
        local_budget = local_budget,
        seeds = seeds,
        n_reference_nodes = n_reference_nodes,
        reference_budget = reference_budget
      )
    ),
    class = "bg_game_trace"
  )
}

#' Experimental structure study
#'
#' `bg_structure_study()` is an experimental feature-based study of where
#' Thompson sampling appears to gain over a baseline across many decision
#' problems. It does **not** claim to implement a fully pooled structured
#' Thompson posterior. Instead it builds a held-out feature map of where
#' unstructured Thompson seems to help.
#'
#' @param problems A list of `bg_problem` objects.
#' @param budget Integer-like evaluation budget.
#' @param seeds Integer-like seed vector.
#' @param baseline Baseline comparator.
#' @param reference_budget Proxy-reference budget.
#' @param train_fraction Fraction of problems used for the training split.
#' @param seed Optional integer-like split seed.
#'
#' @return A `bg_structure_study` object.
#' @export
bg_structure_study <- function(
    problems,
    budget = 128L,
    seeds = 1:10,
    baseline = c("equal", "top_two_thompson", "ucb", "ocba", "greedy"),
    reference_budget = 2048L,
    train_fraction = 0.7,
    seed = NULL) {
  if (!is.list(problems) || length(problems) < 4L || !all(vapply(problems, inherits, logical(1L), what = "bg_problem"))) {
    stop("`problems` must be a list of at least four `bg_problem` objects.", call. = FALSE)
  }
  baseline <- match.arg(baseline)
  budget <- bg_coerce_integerish(budget, "budget", 1L)
  seeds <- sort(unique(bg_coerce_integerish(seeds, "seeds", length(seeds))))
  if (!is.numeric(train_fraction) || length(train_fraction) != 1L || is.na(train_fraction) || train_fraction <= 0 || train_fraction >= 1) {
    stop("`train_fraction` must be a numeric scalar in (0, 1).", call. = FALSE)
  }

  comparison <- bg_compare_methods(
    problems = problems,
    methods = c("thompson", baseline),
    budgets = budget,
    seeds = seeds,
    reference_budget = reference_budget
  )

  feature_rows <- do.call(
    rbind,
    lapply(
      problems,
      function(problem) {
        board_features <- bg_board_features(problem)$board_features
        data.frame(problem_id = problem$problem_id, board_features, stringsAsFactors = FALSE)
      }
    )
  )

  results <- comparison$results
  results <- results[results$checkpoint == budget, , drop = FALSE]
  by_problem_method <- aggregate(
    results[, c("simple_regret", "recommended_prob_best", "allocation_entropy")],
    by = list(problem_id = results$problem_id, method = results$allocation_policy),
    FUN = mean,
    na.rm = TRUE
  )

  th <- by_problem_method[by_problem_method$method == "thompson", , drop = FALSE]
  bl <- by_problem_method[by_problem_method$method == baseline, , drop = FALSE]
  merged <- merge(th, bl, by = "problem_id", suffixes = c("_thompson", "_baseline"))
  merged$ts_regret_gain <- merged$simple_regret_baseline - merged$simple_regret_thompson
  merged$ts_prob_best_gain <- merged$recommended_prob_best_thompson - merged$recommended_prob_best_baseline
  merged <- merge(merged, feature_rows, by = "problem_id", all.x = TRUE)

  split_seed <- if (is.null(seed)) 1L else bg_coerce_integerish(seed, "seed", 1L)
  problem_ids <- merged$problem_id
  train_ids <- bg_ts_with_seed(
    split_seed,
    sample(problem_ids, size = max(1L, floor(length(problem_ids) * train_fraction)))
  )
  merged$split <- ifelse(merged$problem_id %in% train_ids, "train", "test")

  feature_names <- setdiff(
    names(merged),
    c(
      "problem_id",
      "method_thompson",
      "method_baseline",
      "split"
    )
  )
  feature_names <- feature_names[grepl("^own_|^opponent_", feature_names)]
  formula_terms <- paste(feature_names, collapse = " + ")
  fit_formula <- stats::as.formula(paste("ts_regret_gain ~", formula_terms))
  fit <- stats::lm(fit_formula, data = merged[merged$split == "train", , drop = FALSE])
  merged$predicted_ts_regret_gain <- stats::predict(fit, newdata = merged)

  structure(
    list(
      comparison = comparison,
      feature_table = merged,
      model = fit,
      settings = list(
        budget = budget,
        seeds = seeds,
        baseline = baseline,
        reference_budget = reference_budget,
        train_fraction = train_fraction
      ),
      warnings = "This is an experimental feature-based structure study, not a fully pooled structured Thompson posterior."
    ),
    class = "bg_structure_study"
  )
}

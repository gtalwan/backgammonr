# Shared helpers for the presentation walkthrough scripts.
#
# The presentation layer is intentionally small:
# - load the package from the local repo;
# - load the preserved opening truth cache;
# - run the direct public method front doors for one-opening examples;
# - save plots/tables to one predictable output directory.

presentation_repo_root <- function() {
  if (file.exists("DESCRIPTION")) {
    return(normalizePath(".", mustWork = TRUE))
  }

  if (file.exists(file.path("..", "DESCRIPTION"))) {
    return(normalizePath("..", mustWork = TRUE))
  }

  stop("Run this script from the package root or one level below it.", call. = FALSE)
}

presentation_load_package <- function(repo_root) {
  source_repo_r_layer <- function() {
    ns <- asNamespace("backgammonr")
    cpp_names <- grep("^bg_cpp_", ls(ns, all.names = TRUE), value = TRUE)
    for (nm in cpp_names) {
      assign(nm, get(nm, envir = ns, inherits = FALSE), envir = .GlobalEnv)
    }

    r_dir <- file.path(repo_root, "R")
    r_files <- list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE)
    r_files <- setdiff(r_files, file.path(r_dir, "RcppExports.R"))
    for (path in r_files) {
      sys.source(path, envir = .GlobalEnv)
    }
  }

  native_dll_ready <- function() {
    dlls <- getLoadedDLLs()
    "backgammonr" %in% names(dlls) && inherits(dlls[["backgammonr"]], "DLLInfo")
  }

  reset_backgammonr <- function() {
    if ("package:backgammonr" %in% search()) {
      try(detach("package:backgammonr", unload = TRUE, character.only = TRUE), silent = TRUE)
    }
    if ("backgammonr" %in% loadedNamespaces()) {
      try(unloadNamespace("backgammonr"), silent = TRUE)
    }
  }

  if (requireNamespace("pkgload", quietly = TRUE)) {
    try_load <- try(
      pkgload::load_all(repo_root, quiet = TRUE),
      silent = TRUE
    )

    if (inherits(try_load, "try-error") || !native_dll_ready()) {
      reset_backgammonr()
      try_load_no_compile <- try(
        pkgload::load_all(repo_root, quiet = TRUE, compile = FALSE),
        silent = TRUE
      )

      if (inherits(try_load_no_compile, "try-error") || !native_dll_ready()) {
        reset_backgammonr()
        library(backgammonr)
        source_repo_r_layer()
      }
    }
  } else {
    library(backgammonr)
    source_repo_r_layer()
  }

  invisible(repo_root)
}

presentation_detect_workers <- function(max_cores = 6L) {
  detected <- suppressWarnings(parallel::detectCores(logical = FALSE))
  if (!is.numeric(detected) || length(detected) != 1L || is.na(detected)) {
    detected <- 1L
  }

  n_cores <- max(1L, min(as.integer(max_cores), as.integer(detected) - 1L))
  list(
    n_cores = n_cores,
    parallel = identical(.Platform$OS.type, "unix") && n_cores > 1L
  )
}

presentation_truth_cache_dir <- function(repo_root) {
  normalizePath(
    file.path(repo_root, "cache", "opening_truths_master"),
    mustWork = TRUE
  )
}

presentation_master_reference_budget <- function(truths) {
  budget_values <- unique(truths$summary$reference_budget)
  budget_values <- budget_values[!is.na(budget_values)]

  if (length(budget_values) != 1L) {
    return(budget_values)
  }

  budget_values[[1L]]
}

presentation_run_budget <- function() {
  2048L
}

presentation_checkpoint_grid <- function(max_budget = presentation_run_budget()) {
  base_grid <- c(
    16L, 32L, 48L, 64L, 96L,
    128L, 192L, 256L, 384L, 512L,
    768L, 1024L, 1536L, 2048L, 3072L,
    4096L
  )

  checkpoints <- base_grid[base_grid <= max_budget]

  if (length(checkpoints) < 1L || tail(checkpoints, 1L) != max_budget) {
    checkpoints <- sort(unique(c(checkpoints, as.integer(max_budget))))
  }

  checkpoints
}

presentation_roll_label <- function(roll) {
  if (is.character(roll) && length(roll) == 1L && !is.na(roll)) {
    return(roll)
  }

  if (is_bg_roll(roll)) {
    return(paste(sort(as.integer(roll$dice)), collapse = "-"))
  }

  stop("`roll` must be a roll label like '1-6' or a `bg_roll` object.", call. = FALSE)
}

presentation_truth_summary <- function(truths) {
  lookup <- bg_opening_rolls(include_doubles = TRUE)

  rows <- lapply(
    truths,
    function(truth) {
      cert <- bg_truth_certify(truth)
      ref_summary <- truth$reference$summary[1, , drop = FALSE]
      action_table <- truth$reference$action_table
      roll_label <- presentation_roll_label(truth$problem$roll)
      idx <- match(roll_label, lookup$opening_roll)

      data.frame(
        problem_id = truth$problem$problem_id,
        reference_budget = ref_summary$reference_budget[[1L]],
        reference_mode = ref_summary$reference_mode[[1L]],
        reference_is_approximate = ref_summary$reference_is_approximate[[1L]],
        n_moves = nrow(truth$problem$candidate_table),
        best_move_label = cert$best_move_label[[1L]],
        top_two_gap_estimate = cert$top_two_gap_estimate[[1L]],
        top_two_gap_mc_lower_95 = cert$top_two_gap_mc_lower_95[[1L]],
        top_two_gap_mc_upper_95 = cert$top_two_gap_mc_upper_95[[1L]],
        mc_gap_excludes_zero = cert$mc_gap_excludes_zero[[1L]],
        difficulty_label = cert$certification[[1L]],
        reward_model = truth$problem$settings$reward_model_canonical,
        posterior_model = truth$problem$settings$posterior_model_canonical,
        n_near_optimal = cert$n_near_optimal[[1L]],
        mc_not_separated_from_best_set_size = cert$mc_not_separated_from_best_set_size[[1L]],
        mean_reference_se = mean(action_table$reference_se, na.rm = TRUE),
        max_reference_se = max(action_table$reference_se, na.rm = TRUE),
        mean_unresolved_fraction = mean(action_table$unresolved_fraction, na.rm = TRUE),
        roll = roll_label,
        opening_roll = lookup$opening_roll[[idx]],
        die1 = lookup$die1[[idx]],
        die2 = lookup$die2[[idx]],
        is_double = lookup$is_double[[idx]],
        roll_group = lookup$roll_group[[idx]],
        stringsAsFactors = FALSE
      )
    }
  )

  summary <- do.call(rbind, rows)
  summary <- summary[order(summary$die1, summary$die2), , drop = FALSE]
  rownames(summary) <- NULL
  summary
}

presentation_truth_battery <- function(truths, cache_dir) {
  if (is.null(names(truths)) || any(!nzchar(names(truths)))) {
    names(truths) <- vapply(truths, function(x) x$problem$problem_id, character(1L))
  }

  summary <- presentation_truth_summary(truths)
  truths <- truths[summary$problem_id]

  structure(
    list(
      truths = truths,
      summary = summary,
      settings = list(
        source = "opening_truths_master",
        cache_dir = cache_dir
      )
    ),
    class = "bg_truth_battery"
  )
}

presentation_output_dir <- function(repo_root, subdir = c("plots", "tables", "studies")) {
  subdir <- match.arg(subdir)
  out <- file.path(repo_root, "Presentation", "output", subdir)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  normalizePath(out, mustWork = TRUE)
}

presentation_load_opening_truths <- function(repo_root) {
  cache_dir <- presentation_truth_cache_dir(repo_root)
  paths <- list.files(
    cache_dir,
    pattern = "^opening_.*\\.rds$",
    full.names = TRUE
  )

  truths <- lapply(paths, bg_truth_load)
  presentation_truth_battery(truths, cache_dir = cache_dir)
}

presentation_load_opening_truth <- function(repo_root, roll = "1-6") {
  cache_dir <- presentation_truth_cache_dir(repo_root)
  roll_label <- presentation_roll_label(roll)
  stem <- paste0("^opening_", gsub("-", "_", roll_label, fixed = TRUE), ".*\\.rds$")
  paths <- list.files(
    cache_dir,
    pattern = stem,
    full.names = TRUE
  )

  if (length(paths) < 1L) {
    stop("No cached opening truth found for roll `", roll_label, "` in `", cache_dir, "`.", call. = FALSE)
  }

  bg_truth_load(paths[[1L]])
}

presentation_stack_spec <- function(stack = c("beta_bernoulli", "student_t", "dirichlet")) {
  stack <- match.arg(stack)

  switch(
    stack,
    beta_bernoulli = list(
      stack = stack,
      label = "Win/loss + Beta-Bernoulli",
      reward_model = "win_loss",
      posterior_model = "beta_bernoulli",
      unresolved_value = 0
    ),
    student_t = list(
      stack = stack,
      label = "Scalar payoff + Student-t",
      reward_model = "scalar_payoff",
      posterior_model = "student_t_marginal",
      unresolved_value = 0.5
    ),
    dirichlet = list(
      stack = stack,
      label = "Categorical outcome + Dirichlet",
      reward_model = "categorical_outcome",
      posterior_model = "dirichlet_multinomial",
      unresolved_value = 0.5
    )
  )
}

presentation_project_truths <- function(truths, stack = c("beta_bernoulli", "student_t", "dirichlet")) {
  spec <- presentation_stack_spec(stack)

  bg_truth_project(
    truths,
    reward_model = spec$reward_model,
    posterior_model = spec$posterior_model,
    unresolved_value = spec$unresolved_value
  )
}

presentation_method_label <- function(method) {
  labels <- c(
    thompson = "TS",
    top_two_thompson = "TTTS",
    multi_sample_thompson = "Multi-Sample TS",
    soft_elimination_thompson = "Soft-Elimination TS",
    forced_exploration_thompson = "Forced-Exploration TS",
    top_k_thompson = "Top-K TS",
    equal = "Equal"
  )

  out <- unname(labels[method])
  out[is.na(out)] <- method[is.na(out)]
  out
}

presentation_method_palette <- function(methods) {
  palette <- c(
    thompson = "#0072B2",
    top_two_thompson = "#D55E00",
    multi_sample_thompson = "#009E73",
    soft_elimination_thompson = "#CC79A7",
    forced_exploration_thompson = "#E69F00",
    top_k_thompson = "#56B4E9",
    equal = "#6C757D"
  )

  stats::setNames(unname(palette[methods]), presentation_method_label(methods))
}

presentation_run_method <- function(
    method,
    problem,
    budget,
    checkpoints,
    proxy_reference = NULL,
    seed = NULL,
    ...) {
  method <- as.character(method[[1L]])

  if (identical(method, "thompson")) {
    return(bg_ts_run(problem = problem, budget = budget, checkpoints = checkpoints, proxy_reference = proxy_reference, seed = seed, ...))
  }
  if (identical(method, "top_two_thompson")) {
    return(bg_ttts_run(problem = problem, budget = budget, checkpoints = checkpoints, proxy_reference = proxy_reference, seed = seed, ...))
  }
  if (identical(method, "multi_sample_thompson")) {
    return(bg_multi_sample_ts_run(problem = problem, budget = budget, checkpoints = checkpoints, proxy_reference = proxy_reference, seed = seed, ...))
  }
  if (identical(method, "soft_elimination_thompson")) {
    return(bg_soft_elimination_ts_run(problem = problem, budget = budget, checkpoints = checkpoints, proxy_reference = proxy_reference, seed = seed, ...))
  }
  if (identical(method, "forced_exploration_thompson")) {
    return(bg_forced_exploration_ts_run(problem = problem, budget = budget, checkpoints = checkpoints, proxy_reference = proxy_reference, seed = seed, ...))
  }
  if (identical(method, "top_k_thompson")) {
    return(bg_top_k_ts_run(problem = problem, budget = budget, checkpoints = checkpoints, proxy_reference = proxy_reference, seed = seed, ...))
  }
  if (identical(method, "equal")) {
    return(bg_equal_run(problem = problem, budget = budget, checkpoints = checkpoints, proxy_reference = proxy_reference, seed = seed, ...))
  }

  stop("Unsupported presentation method: ", method, call. = FALSE)
}

presentation_run_panel <- function(
    problem,
    truth,
    methods,
    seeds,
    budget,
    checkpoints,
    top_k = 3L,
    ...) {
  task_grid <- expand.grid(
    allocation_policy = methods,
    seed = seeds,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  run_rows <- lapply(
    seq_len(nrow(task_grid)),
    function(i) {
      method <- task_grid$allocation_policy[[i]]
      seed <- task_grid$seed[[i]]
      run <- presentation_run_method(
        method = method,
        problem = problem,
        budget = budget,
        checkpoints = checkpoints,
        proxy_reference = truth$reference,
        seed = seed,
        ...
      )
      panel <- bg_eval_reference_aware(
        run,
        truth = truth,
        checkpoints = checkpoints,
        top_k = top_k
      )

      list(
        run_id = paste(method, seed, sep = "::"),
        run = run,
        panel = panel
      )
    }
  )

  runs <- lapply(run_rows, `[[`, "run")
  names(runs) <- vapply(run_rows, `[[`, character(1L), "run_id")
  panel <- do.call(rbind, lapply(run_rows, `[[`, "panel"))
  rownames(panel) <- NULL

  panel$top1_match_num <- as.numeric(panel$top1_match)
  panel$truth_top2_hit_num <- as.numeric(panel$truth_top2_hit)

  checkpoint_summary <- aggregate(
    cbind(
      top1_match_num,
      simple_regret,
      selected_reference_rank,
      truth_top2_hit_num,
      share_top2_truth,
      share_mc_screened_suboptimal,
      gap_weighted_wasted_allocation,
      recommended_prob_best
    ) ~ allocation_policy + checkpoint,
    data = panel,
    FUN = mean,
    na.rm = TRUE
  )

  names(checkpoint_summary)[names(checkpoint_summary) == "top1_match_num"] <- "top1_match"
  names(checkpoint_summary)[names(checkpoint_summary) == "truth_top2_hit_num"] <- "truth_top2_hit"

  final_summary <- checkpoint_summary[
    checkpoint_summary$checkpoint == max(checkpoints),
    ,
    drop = FALSE
  ]
  final_summary$method <- presentation_method_label(final_summary$allocation_policy)
  final_summary <- final_summary[
    order(-final_summary$top1_match, final_summary$simple_regret),
    c(
      "method",
      "allocation_policy",
      "top1_match",
      "simple_regret",
      "selected_reference_rank",
      "truth_top2_hit",
      "share_top2_truth",
      "share_mc_screened_suboptimal",
      "gap_weighted_wasted_allocation",
      "recommended_prob_best"
    )
  ]

  list(
    runs = runs,
    panel = panel,
    checkpoint_summary = checkpoint_summary,
    final_summary = final_summary
  )
}

presentation_save_plot <- function(plot, repo_root, stem, width = 10, height = 6, dpi = 200) {
  plot_dir <- presentation_output_dir(repo_root, "plots")
  png_path <- file.path(plot_dir, paste0(stem, ".png"))
  pdf_path <- file.path(plot_dir, paste0(stem, ".pdf"))

  ggplot2::ggsave(filename = png_path, plot = plot, width = width, height = height, dpi = dpi)
  ggplot2::ggsave(filename = pdf_path, plot = plot, width = width, height = height, device = grDevices::pdf)

  invisible(list(png = png_path, pdf = pdf_path))
}

presentation_save_table <- function(x, repo_root, stem) {
  table_dir <- presentation_output_dir(repo_root, "tables")
  csv_path <- file.path(table_dir, paste0(stem, ".csv"))
  utils::write.csv(x, csv_path, row.names = FALSE)
  invisible(csv_path)
}

presentation_save_lines <- function(lines, repo_root, stem) {
  table_dir <- presentation_output_dir(repo_root, "tables")
  txt_path <- file.path(table_dir, paste0(stem, ".txt"))
  writeLines(lines, txt_path, useBytes = TRUE)
  invisible(txt_path)
}

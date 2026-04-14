# Shared helpers for the `results/` walkthrough.
#
# These helpers are intentionally narrow:
# - load the local package from the repo;
# - load one preserved master truth from `cache/opening_truths_master`;
# - project that master truth into one of the headline model stacks;
# - save tables, plots, and study objects under `results/output/`; and
# - build simple metric panels from direct method runs.
#
# The direct allocation method calls themselves should remain visible in the
# individual result scripts.

results_repo_root <- function() {
  if (file.exists("DESCRIPTION")) {
    return(normalizePath(".", mustWork = TRUE))
  }

  if (file.exists(file.path("..", "DESCRIPTION"))) {
    return(normalizePath("..", mustWork = TRUE))
  }

  stop("Run these scripts from the repo root or from `results/`.", call. = FALSE)
}

results_load_package <- function(repo_root = results_repo_root()) {
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
    try_load <- try(pkgload::load_all(repo_root, quiet = TRUE), silent = TRUE)

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

results_truth_cache_dir <- function(repo_root = results_repo_root()) {
  normalizePath(
    file.path(repo_root, "cache", "opening_truths_master"),
    mustWork = TRUE
  )
}

results_run_budget <- function() {
  2048L
}

results_checkpoint_grid <- function(max_budget = results_run_budget()) {
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

results_roll_label <- function(roll) {
  if (is.character(roll) && length(roll) == 1L && !is.na(roll)) {
    return(roll)
  }

  if (inherits(roll, "bg_roll")) {
    return(paste(sort(as.integer(roll$dice)), collapse = "-"))
  }

  stop("`roll` must be a roll label like '1-6' or a `bg_roll` object.", call. = FALSE)
}

results_roll_tag <- function(roll) {
  gsub("-", "_", results_roll_label(roll), fixed = TRUE)
}

results_stack_spec <- function(stack = c("beta_bernoulli", "student_t", "dirichlet")) {
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

results_load_master_truth <- function(repo_root, roll = "1-6") {
  roll_label <- results_roll_label(roll)
  pattern <- paste0("^opening_", gsub("-", "_", roll_label, fixed = TRUE), ".*\\.rds$")
  paths <- list.files(
    results_truth_cache_dir(repo_root),
    pattern = pattern,
    full.names = TRUE
  )

  if (length(paths) < 1L) {
    stop("No master truth found for roll `", roll_label, "`.", call. = FALSE)
  }

  bg_truth_load(paths[[1L]])
}

results_project_truth <- function(master_truth, stack = c("beta_bernoulli", "student_t", "dirichlet")) {
  spec <- results_stack_spec(stack)

  bg_truth_project(
    master_truth,
    reward_model = spec$reward_model,
    posterior_model = spec$posterior_model,
    unresolved_value = spec$unresolved_value
  )
}

results_load_all_master_truths <- function(repo_root = results_repo_root()) {
  paths <- list.files(
    results_truth_cache_dir(repo_root),
    pattern = "^opening_.*\\.rds$",
    full.names = TRUE
  )

  truths <- lapply(paths, bg_truth_load)
  names(truths) <- vapply(truths, function(x) x$problem$problem_id, character(1L))

  summary_rows <- lapply(
    truths,
    function(truth) {
      action_table <- truth$reference$action_table
      best_idx <- which.min(action_table$rank)
      data.frame(
        problem_id = truth$problem$problem_id,
        opening_roll = results_roll_label(truth$problem$roll),
        reference_budget = truth$reference$summary$reference_budget[[1L]],
        best_move_label = action_table$move_label[[best_idx]],
        top_two_gap_estimate = truth$reference$summary$top_two_gap_estimate[[1L]],
        top_two_gap_mc_lower_95 = truth$reference$summary$top_two_gap_mc_lower_95[[1L]],
        top_two_gap_mc_upper_95 = truth$reference$summary$top_two_gap_mc_upper_95[[1L]],
        mc_gap_excludes_zero = truth$reference$summary$mc_gap_excludes_zero[[1L]],
        difficulty_label = truth$reference$summary$difficulty_label[[1L]],
        n_moves = nrow(truth$problem$candidate_table),
        n_near_optimal = truth$reference$summary$n_near_optimal[[1L]],
        mean_reference_se = mean(action_table$reference_se, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }
  )

  summary <- do.call(rbind, summary_rows)
  summary <- summary[order(summary$opening_roll), , drop = FALSE]
  rownames(summary) <- NULL

  structure(
    list(
      truths = truths,
      summary = summary
    ),
    class = "bg_truth_battery"
  )
}

results_method_label <- function(method) {
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

results_method_palette <- function(methods) {
  palette <- c(
    thompson = "#0072B2",
    top_two_thompson = "#D55E00",
    multi_sample_thompson = "#009E73",
    soft_elimination_thompson = "#CC79A7",
    forced_exploration_thompson = "#E69F00",
    top_k_thompson = "#56B4E9",
    equal = "#6C757D"
  )

  stats::setNames(unname(palette[methods]), results_method_label(methods))
}

results_output_dir <- function(repo_root, subdir = c("plots", "tables", "studies")) {
  subdir <- match.arg(subdir)
  out <- file.path(repo_root, "results", "output", subdir)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  normalizePath(out, mustWork = TRUE)
}

results_save_plot <- function(plot, repo_root, stem, width = 10, height = 6, dpi = 200) {
  plot_dir <- results_output_dir(repo_root, "plots")
  png_path <- file.path(plot_dir, paste0(stem, ".png"))
  pdf_path <- file.path(plot_dir, paste0(stem, ".pdf"))

  suppressWarnings(
    ggplot2::ggsave(filename = png_path, plot = plot, width = width, height = height, dpi = dpi)
  )
  suppressWarnings(
    ggplot2::ggsave(filename = pdf_path, plot = plot, width = width, height = height, device = grDevices::pdf)
  )

  invisible(list(png = png_path, pdf = pdf_path))
}

results_save_table <- function(x, repo_root, stem) {
  table_dir <- results_output_dir(repo_root, "tables")
  csv_path <- file.path(table_dir, paste0(stem, ".csv"))
  utils::write.csv(x, csv_path, row.names = FALSE)
  invisible(csv_path)
}

results_save_study <- function(x, repo_root, stem) {
  study_dir <- results_output_dir(repo_root, "studies")
  rds_path <- file.path(study_dir, paste0(stem, ".rds"))
  saveRDS(x, rds_path)
  invisible(rds_path)
}

results_plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      plot.margin = ggplot2::margin(10, 14, 10, 10)
    )
}

results_truth_overview <- function(master_truth, truth, roll, stack_label) {
  truth_summary <- truth$reference$summary[1, , drop = FALSE]
  action_table <- truth$reference$action_table
  best_idx <- which.min(action_table$rank)

  data.frame(
    opening_roll = results_roll_label(roll),
    stack = stack_label,
    master_reference_budget = master_truth$reference$summary$reference_budget[[1L]],
    projected_reference_budget = truth_summary$reference_budget[[1L]],
    reward_model = truth$problem$settings$reward_model_canonical,
    posterior_model = truth$problem$settings$posterior_model_canonical,
    n_moves = nrow(truth$problem$candidate_table),
    truth_best_move = action_table$move_label[[best_idx]],
    truth_best_value = action_table$reference_mean[[best_idx]],
    top_two_gap_estimate = truth_summary$top_two_gap_estimate[[1L]],
    difficulty_label = truth_summary$difficulty_label[[1L]],
    stringsAsFactors = FALSE
  )
}

results_truth_action_table <- function(truth, top_n = 10L) {
  keep_cols <- intersect(
    c(
      "candidate_index",
      "move_label",
      "reference_mean",
      "rank",
      "reference_se",
      "unresolved_fraction",
      "wins",
      "losses",
      "unresolved"
    ),
    names(truth$reference$action_table)
  )

  out <- truth$reference$action_table[
    order(truth$reference$action_table$rank),
    keep_cols,
    drop = FALSE
  ]

  out[seq_len(min(top_n, nrow(out))), , drop = FALSE]
}

results_ensure_columns <- function(df, cols) {
  missing_cols <- setdiff(cols, names(df))

  if (length(missing_cols) > 0L) {
    for (col in missing_cols) {
      df[[col]] <- NA
    }
  }

  df[, cols, drop = FALSE]
}

results_collect_run_panels <- function(run, truth, checkpoints) {
  panel <- bg_eval_reference_aware(run, truth = truth, checkpoints = checkpoints)
  rank_panel <- bg_eval_rank(run, truth = truth, checkpoints = checkpoints)
  alloc_panel <- bg_eval_allocation(run, truth = truth, checkpoints = checkpoints, top_k = 2L)

  panel$top1_match <- as.numeric(panel$top1_match)
  panel$truth_top2_hit <- as.numeric(panel$truth_top2_hit)

  merged <- merge(
    results_ensure_columns(panel, c(
      "allocation_policy",
      "checkpoint",
      "recommended_move_label",
      "recommended_prob_best",
      "posterior_top_k_mass",
      "simple_regret",
      "selected_reference_rank",
      "top1_match",
      "truth_top2_hit"
    )),
    results_ensure_columns(rank_panel, c(
      "allocation_policy",
      "checkpoint",
      "spearman",
      "top_k_overlap",
      "weighted_rank_loss",
      "restricted_pairwise_ordering_accuracy"
    )),
    by = c("allocation_policy", "checkpoint"),
    all = TRUE,
    sort = FALSE
  )
  merged <- merge(
    merged,
    results_ensure_columns(alloc_panel, c(
      "allocation_policy",
      "checkpoint",
      "share_best_truth",
      "share_top2_truth",
      "share_mc_screened_suboptimal",
      "gap_weighted_wasted_allocation"
    )),
    by = c("allocation_policy", "checkpoint"),
    all = TRUE,
    sort = FALSE
  )

  merged <- merged[order(merged$checkpoint), , drop = FALSE]
  rownames(merged) <- NULL

  list(
    panel = panel,
    rank_panel = rank_panel,
    alloc_panel = alloc_panel,
    checkpoint_metrics = merged
  )
}

results_final_summary <- function(method, checkpoint_metrics) {
  final_row <- checkpoint_metrics[checkpoint_metrics$checkpoint == max(checkpoint_metrics$checkpoint), , drop = FALSE]

  data.frame(
    method = results_method_label(method),
    allocation_policy = method,
    recommended_move_label = final_row$recommended_move_label,
    top1_match = final_row$top1_match,
    simple_regret = final_row$simple_regret,
    selected_reference_rank = final_row$selected_reference_rank,
    recommended_prob_best = final_row$recommended_prob_best,
    posterior_top_k_mass = final_row$posterior_top_k_mass,
    spearman = final_row$spearman,
    top_k_overlap = final_row$top_k_overlap,
    weighted_rank_loss = final_row$weighted_rank_loss,
    restricted_pairwise_ordering_accuracy = final_row$restricted_pairwise_ordering_accuracy,
    truth_top2_hit = final_row$truth_top2_hit,
    share_best_truth = final_row$share_best_truth,
    share_top2_truth = final_row$share_top2_truth,
    share_mc_screened_suboptimal = final_row$share_mc_screened_suboptimal,
    gap_weighted_wasted_allocation = final_row$gap_weighted_wasted_allocation,
    stringsAsFactors = FALSE
  )
}

results_plot_checkpoint_lines <- function(df, metric, title, subtitle, y_label = metric, methods = NULL) {
  if (is.null(methods)) {
    methods <- unique(df$allocation_policy)
  }

  palette <- results_method_palette(methods)
  plot_df <- df[, c("allocation_policy", "checkpoint", metric), drop = FALSE]
  names(plot_df)[[3L]] <- "metric_value"
  plot_df$method <- factor(
    results_method_label(plot_df$allocation_policy),
    levels = results_method_label(methods)
  )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = checkpoint, y = metric_value, color = method)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Budget checkpoint",
      y = y_label,
      color = "Method"
    ) +
    results_plot_theme()
}

results_plot_top1_heatmap <- function(df, title, subtitle, methods = NULL) {
  if (is.null(methods)) {
    methods <- unique(df$allocation_policy)
  }

  plot_df <- df[, c("allocation_policy", "checkpoint", "top1_match"), drop = FALSE]
  plot_df$top1_match <- as.numeric(plot_df$top1_match)
  plot_df$method <- factor(
    results_method_label(plot_df$allocation_policy),
    levels = results_method_label(methods)
  )
  checkpoint_levels <- sort(unique(plot_df$checkpoint))
  plot_df$checkpoint_label <- factor(
    as.character(plot_df$checkpoint),
    levels = as.character(checkpoint_levels)
  )
  plot_df$tile_label <- ifelse(plot_df$top1_match >= 0.5, "Match", "Miss")

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = checkpoint_label, y = method, fill = top1_match)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.35) +
    ggplot2::geom_text(
      ggplot2::aes(label = tile_label),
      size = 3,
      color = "#1F1F1F",
      fontface = "bold",
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_gradient(
      low = "#F4F1EA",
      high = "#0B4F6C",
      limits = c(0, 1),
      breaks = c(0, 1),
      labels = c("Miss", "Match")
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Budget checkpoint",
      y = NULL,
      fill = "Top-1\nmatch"
    ) +
    results_plot_theme() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)
    )
}

results_plot_truth_top2_heatmap <- function(df, title, subtitle, methods = NULL) {
  if (is.null(methods)) {
    methods <- unique(df$allocation_policy)
  }

  plot_df <- df[, c("allocation_policy", "checkpoint", "truth_top2_hit"), drop = FALSE]
  plot_df$truth_top2_hit <- as.numeric(plot_df$truth_top2_hit)
  plot_df$method <- factor(
    results_method_label(plot_df$allocation_policy),
    levels = results_method_label(methods)
  )
  checkpoint_levels <- sort(unique(plot_df$checkpoint))
  plot_df$checkpoint_label <- factor(
    as.character(plot_df$checkpoint),
    levels = as.character(checkpoint_levels)
  )
  plot_df$tile_label <- ifelse(plot_df$truth_top2_hit >= 0.5, "Top-2", "Outside")

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = checkpoint_label, y = method, fill = truth_top2_hit)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.35) +
    ggplot2::geom_text(
      ggplot2::aes(label = tile_label),
      size = 3,
      color = "#1F1F1F",
      fontface = "bold",
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_gradient(
      low = "#F4F1EA",
      high = "#1B6C5A",
      limits = c(0, 1),
      breaks = c(0, 1),
      labels = c("Outside top-2", "Inside top-2")
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Budget checkpoint",
      y = NULL,
      fill = "Truth top-2\nhit"
    ) +
    results_plot_theme() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)
    )
}

results_plot_confidence_heatmap <- function(df, title, subtitle, methods = NULL) {
  if (is.null(methods)) {
    methods <- unique(df$allocation_policy)
  }

  plot_df <- df[, c("allocation_policy", "checkpoint", "recommended_prob_best"), drop = FALSE]
  plot_df <- plot_df[stats::complete.cases(plot_df), , drop = FALSE]
  plot_df$method <- factor(
    results_method_label(plot_df$allocation_policy),
    levels = results_method_label(methods)
  )
  checkpoint_levels <- sort(unique(plot_df$checkpoint))
  plot_df$checkpoint_label <- factor(
    as.character(plot_df$checkpoint),
    levels = as.character(checkpoint_levels)
  )
  plot_df$tile_label <- sprintf("%.2f", plot_df$recommended_prob_best)

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = checkpoint_label, y = method, fill = recommended_prob_best)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.35) +
    ggplot2::geom_text(
      ggplot2::aes(label = tile_label),
      size = 2.9,
      color = "#1F1F1F",
      fontface = "bold",
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_gradient(
      low = "#F4F1EA",
      high = "#6A3D9A",
      limits = c(0, 1)
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Budget checkpoint",
      y = NULL,
      fill = "Posterior\nconfidence"
    ) +
    results_plot_theme() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)
    )
}

results_plot_confidence_regret_scatter <- function(df, title, subtitle, methods = NULL) {
  if (is.null(methods)) {
    methods <- unique(df$allocation_policy)
  }

  plot_df <- df[, c(
    "allocation_policy",
    "checkpoint",
    "recommended_prob_best",
    "simple_regret"
  ), drop = FALSE]
  plot_df <- plot_df[stats::complete.cases(plot_df), , drop = FALSE]
  plot_df$method <- factor(
    results_method_label(plot_df$allocation_policy),
    levels = results_method_label(methods)
  )
  palette <- results_method_palette(methods)
  facet_methods <- length(unique(plot_df$allocation_policy)) > 2L
  label_rows <- do.call(
    rbind,
    lapply(
      split(plot_df, plot_df$allocation_policy),
      function(chunk) {
        chunk <- chunk[order(chunk$checkpoint), , drop = FALSE]
        idx <- unique(round(c(1, nrow(chunk) / 2, nrow(chunk))))
        chunk[idx, , drop = FALSE]
      }
    )
  )

  base_plot <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = recommended_prob_best,
      y = simple_regret,
      color = method
    )
  ) +
    ggplot2::geom_vline(xintercept = 0.5, linewidth = 0.4, linetype = "dashed", color = "#9A9A9A") +
    ggplot2::geom_hline(yintercept = 0.01, linewidth = 0.4, linetype = "dashed", color = "#9A9A9A") +
    ggplot2::geom_point(
      ggplot2::aes(size = checkpoint),
      alpha = 0.9,
      show.legend = !facet_methods
    ) +
    ggplot2::geom_text(
      data = label_rows,
      ggplot2::aes(label = checkpoint),
      size = 3,
      nudge_y = 0.0025,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::scale_size_continuous(range = c(1.8, 4.0), guide = if (facet_methods) "none" else "legend") +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "recommended_prob_best",
      y = "simple_regret",
      color = "Method",
      size = "Checkpoint"
    ) +
    results_plot_theme()

  if (facet_methods) {
    base_plot +
      ggplot2::facet_wrap(~method, ncol = 2) +
      ggplot2::guides(color = "none")
  } else {
    base_plot
  }
}

results_plot_rank_confidence <- function(df, title, subtitle, methods = NULL) {
  if (is.null(methods)) {
    methods <- unique(df$allocation_policy)
  }

  plot_df <- results_ensure_columns(df, c(
    "allocation_policy",
    "checkpoint",
    "selected_reference_rank",
    "recommended_prob_best"
  ))
  plot_df <- plot_df[stats::complete.cases(plot_df[, c("checkpoint", "selected_reference_rank")]), , drop = FALSE]
  plot_df$recommended_prob_best[is.na(plot_df$recommended_prob_best)] <- 0.5
  plot_df$method <- factor(
    results_method_label(plot_df$allocation_policy),
    levels = results_method_label(methods)
  )
  palette <- results_method_palette(methods)
  rank_breaks <- sort(unique(plot_df$selected_reference_rank))

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = checkpoint,
      y = selected_reference_rank,
      color = method,
      group = method
    )
  ) +
    ggplot2::geom_line(linewidth = 0.9, alpha = 0.9) +
    ggplot2::geom_point(
      ggplot2::aes(fill = recommended_prob_best),
      shape = 21,
      size = 3.4,
      stroke = 1.0
    ) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::scale_fill_gradient(
      low = "#F4F1EA",
      high = "#6A3D9A",
      limits = c(0, 1)
    ) +
    ggplot2::scale_y_reverse(breaks = rank_breaks) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Budget checkpoint",
      y = "Truth rank of recommended move",
      color = "Method",
      fill = "Posterior\nconfidence"
    ) +
    results_plot_theme()
}

results_plot_recommended_timeline <- function(df, title, subtitle, methods = NULL) {
  if (is.null(methods)) {
    methods <- unique(df$allocation_policy)
  }

  plot_df <- results_ensure_columns(df, c(
    "allocation_policy",
    "checkpoint",
    "recommended_move_label",
    "selected_reference_rank",
    "recommended_prob_best"
  ))
  plot_df$recommended_prob_best[is.na(plot_df$recommended_prob_best)] <- 0.5
  plot_df$method <- factor(
    results_method_label(plot_df$allocation_policy),
    levels = results_method_label(methods)
  )
  checkpoint_levels <- sort(unique(plot_df$checkpoint))
  plot_df$checkpoint_label <- factor(
    as.character(plot_df$checkpoint),
    levels = as.character(checkpoint_levels)
  )
  plot_df$move_label_wrapped <- gsub(", ", "\n", plot_df$recommended_move_label, fixed = TRUE)

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = checkpoint_label,
      y = method,
      fill = selected_reference_rank,
      alpha = recommended_prob_best
    )
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.35) +
    ggplot2::geom_text(
      ggplot2::aes(label = move_label_wrapped),
      size = 2.4,
      color = "#1F1F1F",
      lineheight = 0.9,
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_gradient(
      low = "#0B4F6C",
      high = "#F4F1EA",
      trans = "reverse"
    ) +
    ggplot2::scale_alpha_continuous(
      range = c(0.45, 1),
      limits = c(0, 1)
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Budget checkpoint",
      y = NULL,
      fill = "Truth rank\nof recommended\nmove",
      alpha = "Posterior\nconfidence"
    ) +
    results_plot_theme() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)
    )
}

results_plot_final_metric_bars <- function(df, metric, title, subtitle, methods = NULL, y_label = metric) {
  if (is.null(methods)) {
    methods <- df$allocation_policy
  }

  palette <- results_method_palette(methods)
  plot_df <- df[, c("allocation_policy", metric), drop = FALSE]
  names(plot_df)[[2L]] <- "metric_value"
  plot_df$method <- factor(
    results_method_label(plot_df$allocation_policy),
    levels = results_method_label(methods)
  )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = method, y = metric_value, fill = method)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = y_label
    ) +
    results_plot_theme()
}


# Legacy output-table formatters and report-facing table builders.
bg_has_non_missing <- function(x) {
  # Convenience predicate for dropping entirely empty summary columns.
  any(!is.na(x))
}

bg_truncate_rows <- function(df, n, arg_name = "n") {
  # Optionally keep only the leading rows of a pre-sorted summary table.
  if (is.null(n)) {
    return(df)
  }

  n <- bg_coerce_integerish(n, arg_name, 1L)
  if (n < 1L) {
    stop(sprintf("`%s` must be at least 1.", arg_name), call. = FALSE)
  }

  utils::head(df, n)
}

bg_first_present_column <- function(df, candidates) {
  # Pick the first available column from a preferred-name list.
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0L) {
    return(NULL)
  }
  hit[[1L]]
}

bg_compact_action_table <- function(results, n = NULL, include_interval = TRUE) {
  # Reformat action-level truth/posterior tables into a compact report-ready
  # layout with stable column names.
  results <- as.data.frame(results, stringsAsFactors = FALSE)
  if (nrow(results) == 0L) {
    return(results)
  }

  if ("rank" %in% names(results)) {
    results <- results[order(results$rank), , drop = FALSE]
  } else if (all(c("estimate", "candidate_index") %in% names(results))) {
    results <- results[order(-results$estimate, results$candidate_index), , drop = FALSE]
  }

  results <- bg_truncate_rows(results, n, "n")

  estimate_col <- bg_first_present_column(results, c("estimate", "reference_mean"))
  uncertainty_col <- bg_first_present_column(results, c("posterior_sd", "reference_se"))
  prob_best_col <- bg_first_present_column(results, c("prob_best", "model_relative_prob_best"))
  regret_col <- bg_first_present_column(results, c("posterior_expected_regret", "model_relative_expected_regret"))
  lower_col <- bg_first_present_column(results, c("lower_95", "reference_mc_lower_95"))
  upper_col <- bg_first_present_column(results, c("upper_95", "reference_mc_upper_95"))

  out <- data.frame(
    rank = if ("rank" %in% names(results)) results$rank else seq_len(nrow(results)),
    action_id = if ("candidate_index" %in% names(results)) results$candidate_index else seq_len(nrow(results)),
    action = if ("move_label" %in% names(results)) results$move_label else NA_character_,
    recommended = if ("recommended" %in% names(results)) {
      as.logical(results$recommended)
    } else {
      rep(FALSE, nrow(results))
    },
    alloc_n = if ("allocation_count" %in% names(results)) results$allocation_count else NA_integer_,
    estimate = if (!is.null(estimate_col)) results[[estimate_col]] else NA_real_,
    uncertainty_sd = if (!is.null(uncertainty_col)) results[[uncertainty_col]] else NA_real_,
    prob_best = if (!is.null(prob_best_col)) results[[prob_best_col]] else NA_real_,
    exp_regret = if (!is.null(regret_col)) {
      results[[regret_col]]
    } else {
      NA_real_
    },
    stringsAsFactors = FALSE
  )

  if (isTRUE(include_interval) &&
      !is.null(lower_col) &&
      !is.null(upper_col) &&
      bg_has_non_missing(results[[lower_col]]) &&
      bg_has_non_missing(results[[upper_col]])) {
    out$ci95_low <- results[[lower_col]]
    out$ci95_high <- results[[upper_col]]
  }

  if ("proxy_reference_rank" %in% names(results)) {
    out$reference_rank <- results$proxy_reference_rank
  }
  if ("simple_regret" %in% names(results)) {
    out$simple_regret <- results$simple_regret
  }
  if ("unresolved_fraction" %in% names(results)) {
    out$unresolved_frac <- results$unresolved_fraction
  } else if (all(c("unresolved", "allocation_count") %in% names(results))) {
    out$unresolved_frac <- ifelse(
      results$allocation_count > 0L,
      results$unresolved / results$allocation_count,
      NA_real_
    )
  }

  drop_cols <- character(0L)
  if (!bg_has_non_missing(out$prob_best)) {
    drop_cols <- c(drop_cols, "prob_best")
  }
  if (!bg_has_non_missing(out$exp_regret)) {
    drop_cols <- c(drop_cols, "exp_regret")
  }
  if (!bg_has_non_missing(out$uncertainty_sd)) {
    drop_cols <- c(drop_cols, "uncertainty_sd")
  }
  if ("reference_rank" %in% names(out) && !bg_has_non_missing(out$reference_rank)) {
    drop_cols <- c(drop_cols, "reference_rank")
  }
  if ("simple_regret" %in% names(out) && !bg_has_non_missing(out$simple_regret)) {
    drop_cols <- c(drop_cols, "simple_regret")
  }
  if ("unresolved_frac" %in% names(out) && !bg_has_non_missing(out$unresolved_frac)) {
    drop_cols <- c(drop_cols, "unresolved_frac")
  }
  if ("ci95_low" %in% names(out) && !bg_has_non_missing(out$ci95_low)) {
    drop_cols <- c(drop_cols, "ci95_low", "ci95_high")
  }

  out <- out[, setdiff(names(out), unique(drop_cols)), drop = FALSE]
  out
}

bg_compact_benchmark_summary <- function(summary_df, n = NULL) {
  # Compact one benchmark-study summary to the key cross-method columns.
  summary_df <- as.data.frame(summary_df, stringsAsFactors = FALSE)
  if (nrow(summary_df) == 0L) {
    return(summary_df)
  }

  keep <- intersect(
    c(
      "method",
      "total_budget",
      "probability_correct_selection",
      "mean_simple_regret",
      "mean_mse",
      "mean_runtime_seconds",
      "dice_mode",
      "crn",
      "total_cases",
      "reference_best_move_label",
      "most_selected_move_label"
    ),
    names(summary_df)
  )
  out <- summary_df[, keep, drop = FALSE]
  rename_map <- c(
    probability_correct_selection = "proxy_pcs",
    mean_simple_regret = "simple_regret",
    mean_mse = "mse",
    mean_runtime_seconds = "runtime_seconds",
    reference_best_move_label = "reference_best_action",
    most_selected_move_label = "most_selected_action"
  )
  for (old_nm in names(rename_map)) {
    if (old_nm %in% names(out)) {
      names(out)[names(out) == old_nm] <- rename_map[[old_nm]]
    }
  }
  out <- out[, intersect(
    c(
      "method",
      "total_budget",
      "proxy_pcs",
      "simple_regret",
      "mse",
      "runtime_seconds",
      "dice_mode",
      "crn",
      "total_cases",
      "reference_best_action",
      "most_selected_action"
    ),
    names(out)
  ), drop = FALSE]
  out <- out[order(out$total_budget, out$method), , drop = FALSE]
  bg_truncate_rows(out, n, "n")
}

bg_compact_budget_tradeoff_table <- function(results, n = NULL) {
  # Present one recommendation-vs-budget table with consistent naming across
  # methods and studies.
  results <- as.data.frame(results, stringsAsFactors = FALSE)
  if (nrow(results) == 0L) {
    return(results)
  }

  keep <- intersect(
    c(
      "method",
      "total_budget",
      "chosen_move_label",
      "correct_selection",
      "simple_regret",
      "mse",
      "runtime_seconds",
      "chosen_estimate",
      "chosen_truth_value",
      "truth_best_value"
    ),
    names(results)
  )
  out <- results[, keep, drop = FALSE]
  rename_map <- c(
    chosen_move_label = "recommended_action",
    correct_selection = "proxy_pcs",
    chosen_truth_value = "reference_value_recommended",
    truth_best_value = "reference_best_value"
  )
  for (old_nm in names(rename_map)) {
    if (old_nm %in% names(out)) {
      names(out)[names(out) == old_nm] <- rename_map[[old_nm]]
    }
  }
  out <- out[, intersect(
    c(
      "method",
      "total_budget",
      "recommended_action",
      "proxy_pcs",
      "simple_regret",
      "mse",
      "runtime_seconds",
      "chosen_estimate",
      "reference_value_recommended",
      "reference_best_value"
    ),
    names(out)
  ), drop = FALSE]
  out <- out[order(out$total_budget), , drop = FALSE]
  bg_truncate_rows(out, n, "n")
}

bg_compact_variance_table <- function(results, n = NULL) {
  results <- as.data.frame(results, stringsAsFactors = FALSE)
  if (nrow(results) == 0L) {
    return(results)
  }

  keep <- intersect(
    c(
      "method",
      "total_budget",
      "dice_mode",
      "crn",
      "chosen_move_label",
      "correct_selection",
      "simple_regret",
      "mse",
      "runtime_seconds"
    ),
    names(results)
  )
  out <- results[, keep, drop = FALSE]
  rename_map <- c(
    chosen_move_label = "recommended_action",
    correct_selection = "proxy_pcs"
  )
  for (old_nm in names(rename_map)) {
    if (old_nm %in% names(out)) {
      names(out)[names(out) == old_nm] <- rename_map[[old_nm]]
    }
  }
  out <- out[, intersect(
    c(
      "method",
      "total_budget",
      "dice_mode",
      "crn",
      "recommended_action",
      "proxy_pcs",
      "simple_regret",
      "mse",
      "runtime_seconds"
    ),
    names(out)
  ), drop = FALSE]
  out <- out[order(out$dice_mode, out$crn), , drop = FALSE]
  bg_truncate_rows(out, n, "n")
}

bg_compact_method_comparison_table <- function(summary_df, n = NULL) {
  summary_df <- as.data.frame(summary_df, stringsAsFactors = FALSE)
  if (nrow(summary_df) == 0L) {
    return(summary_df)
  }

  keep <- intersect(
    c(
      "method",
      "recommended_move_label",
      "recommended_allocation_count",
      "recommended_estimate",
      "recommended_posterior_sd",
      "recommended_prob_best",
      "recommended_expected_regret",
      "estimate_gap_top2",
      "runtime_seconds"
    ),
    names(summary_df)
  )
  out <- summary_df[, keep, drop = FALSE]
  rename_map <- c(
    recommended_move_label = "recommended_action",
    recommended_allocation_count = "alloc_n",
    recommended_estimate = "estimate",
    recommended_posterior_sd = "uncertainty_sd",
    recommended_prob_best = "prob_best",
    recommended_expected_regret = "exp_regret"
  )
  for (old_nm in names(rename_map)) {
    if (old_nm %in% names(out)) {
      names(out)[names(out) == old_nm] <- rename_map[[old_nm]]
    }
  }
  out <- out[, intersect(
    c(
      "method",
      "recommended_action",
      "alloc_n",
      "estimate",
      "uncertainty_sd",
      "prob_best",
      "exp_regret",
      "estimate_gap_top2",
      "runtime_seconds"
    ),
    names(out)
  ), drop = FALSE]

  order_key <- rep.int(1L, nrow(out))
  if ("method" %in% names(out)) {
    order_key <- ifelse(out$method == "thompson", 0L, 1L)
  }
  estimate_col <- if ("estimate" %in% names(out)) out$estimate else rep(NA_real_, nrow(out))
  out <- out[order(order_key, -estimate_col, out$method), , drop = FALSE]
  bg_truncate_rows(out, n, "n")
}

bg_compact_truth_table <- function(x, n = 8L) {
  n <- bg_coerce_integerish(n, "n", 1L)

  if (inherits(x, "bg_truth_battery")) {
    df <- x$summary
    keep <- intersect(
      c(
        "problem_id",
        "opening_roll",
        "n_moves",
        "best_move_label",
        "top_two_gap_estimate",
        "n_near_optimal",
        "mc_not_separated_from_best_set_size",
        "mean_reference_se",
        "difficulty_label"
      ),
      names(df)
    )
    out <- df[, keep, drop = FALSE]
    if ("best_move_label" %in% names(out)) {
      names(out)[names(out) == "best_move_label"] <- "truth_best_move"
    }
    out <- out[order(out$top_two_gap_estimate), , drop = FALSE]
    return(bg_round_display_table(bg_truncate_rows(out, n, "n")))
  }

  diag <- bg_truth_diagnostics(x, top_n = n)
  if (is.list(diag) && "move_table" %in% names(diag)) {
    return(bg_round_display_table(diag$move_table))
  }

  bg_round_display_table(bg_truncate_rows(as.data.frame(diag, stringsAsFactors = FALSE), n, "n"))
}

bg_compact_reference_aware_table <- function(
    x,
    truth = NULL,
    checkpoints = NULL,
    top_k = 3L,
    epsilon = 0.01,
    gap_tol = 0.01,
    n = NULL) {
  out <- bg_eval_reference_aware(
    x = x,
    truth = truth,
    checkpoints = checkpoints,
    top_k = top_k,
    epsilon = epsilon,
    gap_tol = gap_tol
  )

  keep <- intersect(
    c(
      "problem_id",
      "allocation_policy",
      "checkpoint",
      "seed",
      "recommended_move_label",
      "truth_best_move_label",
      "top1_match",
      "simple_regret",
      "spearman",
      "top_k_overlap",
      "pairwise_ordering_accuracy",
      "share_top_k_truth",
      "share_mc_screened_suboptimal",
      "allocation_entropy",
      "near_tie",
      "chosen_mc_not_separated_from_best"
    ),
    names(out)
  )
  out <- out[, keep, drop = FALSE]
  bg_round_display_table(bg_truncate_rows(out, n, "n"))
}

bg_compact_state_battery_table <- function(x, n = NULL) {
  if (!inherits(x, "bg_state_battery")) {
    stop("`x` must inherit from class 'bg_state_battery'.", call. = FALSE)
  }

  out <- x$state_table[, intersect(
    c(
      "problem_id",
      "sample_seed",
      "game_index",
      "turn_index",
      "state_class",
      "n_legal_moves",
      "top_two_gap_estimate",
      "n_near_optimal",
      "difficulty_score"
    ),
    names(x$state_table)
  ), drop = FALSE]
  out <- out[order(out$state_class, out$top_two_gap_estimate), , drop = FALSE]
  bg_round_display_table(bg_truncate_rows(out, n, "n"))
}

bg_has_non_missing <- function(x) {
  any(!is.na(x))
}

bg_truncate_rows <- function(df, n, arg_name = "n") {
  if (is.null(n)) {
    return(df)
  }

  n <- bg_coerce_integerish(n, arg_name, 1L)
  if (n < 1L) {
    stop(sprintf("`%s` must be at least 1.", arg_name), call. = FALSE)
  }

  utils::head(df, n)
}

bg_compact_action_table <- function(results, n = NULL, include_interval = TRUE) {
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
    estimate = if ("estimate" %in% names(results)) results$estimate else NA_real_,
    uncertainty_sd = if ("posterior_sd" %in% names(results)) results$posterior_sd else NA_real_,
    prob_best = if ("prob_best" %in% names(results)) results$prob_best else NA_real_,
    exp_regret = if ("posterior_expected_regret" %in% names(results)) {
      results$posterior_expected_regret
    } else {
      NA_real_
    },
    stringsAsFactors = FALSE
  )

  if (isTRUE(include_interval) &&
      all(c("lower_95", "upper_95") %in% names(results)) &&
      bg_has_non_missing(results$lower_95) &&
      bg_has_non_missing(results$upper_95)) {
    out$ci95_low <- results$lower_95
    out$ci95_high <- results$upper_95
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
  if ("ci95_low" %in% names(out) && !bg_has_non_missing(out$ci95_low)) {
    drop_cols <- c(drop_cols, "ci95_low", "ci95_high")
  }

  out <- out[, setdiff(names(out), unique(drop_cols)), drop = FALSE]
  out
}

bg_compact_benchmark_summary <- function(summary_df, n = NULL) {
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

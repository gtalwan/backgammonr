# S3 print/summary/plot/autoplot/as_tibble methods.
#
# This file centralizes the user-facing methods for package objects.

# -----------------------------------------------------------------------------
# Source: print.R
# -----------------------------------------------------------------------------
# Print and formatting methods for board, roll, move, and study objects.
#' Print a backgammon board
#'
#' @param x A `bg_board` object.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.bg_board <- function(x, ...) {
  info <- bg_inspect_board(x)
  occupied_n <- sum(info$points$n_checkers > 0L)
  cat("<bg_board>\n", sep = "")
  cat("occupied points: ", occupied_n, "\n", sep = "")
  cat(format(x, ...), sep = "\n")

  invisible(x)
}

#' Print a backgammon dice roll
#'
#' @param x A `bg_roll` object.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.bg_roll <- function(x, ...) {
  if (!is_bg_roll(x)) {
    stop("`x` must inherit from class 'bg_roll'.", call. = FALSE)
  }

  cat("<bg_roll>\n", sep = "")
  cat("dice:     ", paste(x$dice, collapse = " "), "\n", sep = "")
  cat("double:   ", if (isTRUE(x$is_double)) "yes" else "no", "\n", sep = "")
  cat("expanded: ", paste(x$expanded, collapse = " "), "\n", sep = "")

  invisible(x)
}

#' Print a backgammon move step
#'
#' @param x A `bg_move_step` object.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.bg_move_step <- function(x, ...) {
  if (!is_bg_move_step(x)) {
    stop("`x` must inherit from class 'bg_move_step'.", call. = FALSE)
  }

  cat("<bg_move_step>\n", sep = "")
  cat("from: ", x$from, "\n", sep = "")
  cat("to:   ", x$to, "\n", sep = "")
  cat("die:  ", x$die, "\n", sep = "")
  cat("hit:  ", if (isTRUE(x$hit)) "yes" else "no", "\n", sep = "")

  invisible(x)
}

#' Print a backgammon move sequence
#'
#' @param x A `bg_move_sequence` object.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.bg_move_sequence <- function(x, ...) {
  if (!is_bg_move_sequence(x)) {
    stop("`x` must inherit from class 'bg_move_sequence'.", call. = FALSE)
  }

  cat("<bg_move_sequence>\n", sep = "")
  cat("player:   ", if (x$player == 1L) "player_1" else "player_2", "\n", sep = "")
  cat("n_steps:  ", x$n_steps, "\n", sep = "")
  cat("dice_used:", if (length(x$dice_used) == 0L) " <none>" else paste(" ", paste(x$dice_used, collapse = " "), sep = ""), "\n", sep = "")

  if (is.null(x$roll)) {
    cat("roll:     <none>\n", sep = "")
  } else {
    cat("roll:     ", paste(x$roll$dice, collapse = " "), "\n", sep = "")
  }

  if (length(x$steps) == 0L) {
    cat("steps:    <none>\n", sep = "")
  } else {
    step_df <- data.frame(
      idx = seq_along(x$steps),
      from = vapply(x$steps, function(step) step$from, integer(1L)),
      to = vapply(x$steps, function(step) step$to, integer(1L)),
      die = vapply(x$steps, function(step) step$die, integer(1L)),
      hit = vapply(x$steps, function(step) step$hit, logical(1L)),
      stringsAsFactors = FALSE
    )
    print(step_df, row.names = FALSE)
  }

  invisible(x)
}

#' Print a turn result
#'
#' @param x A `bg_turn_result` object.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.bg_turn_result <- function(x, ...) {
  if (!is_bg_turn_result(x)) {
    stop("`x` must inherit from class 'bg_turn_result'.", call. = FALSE)
  }

  cat("<bg_turn_result>\n", sep = "")
  cat("player:       ", if (x$player == 1L) "player_1" else "player_2", "\n", sep = "")
  cat("selection:    ", x$selection, "\n", sep = "")
  cat("roll:         ", paste(x$roll$dice, collapse = " "), "\n", sep = "")
  cat("n_legal_moves:", x$n_legal_moves, "\n", sep = "")
  cat("turn_passed:  ", if (isTRUE(x$turn_passed)) "yes" else "no", "\n", sep = "")
  cat("game_over:    ", if (isTRUE(x$game_over)) "yes" else "no", "\n", sep = "")
  cat("winner:       ", x$winner, "\n", sep = "")

  invisible(x)
}

#' Print a game result
#'
#' @param x A `bg_game_result` object.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.bg_game_result <- function(x, ...) {
  if (!is_bg_game_result(x)) {
    stop("`x` must inherit from class 'bg_game_result'.", call. = FALSE)
  }

  cat("<bg_game_result>\n", sep = "")
  cat("player_1_selection:    ", x$player1_selection, "\n", sep = "")
  cat("player_2_selection:    ", x$player2_selection, "\n", sep = "")
  cat("n_turns:               ", x$n_turns, "\n", sep = "")
  cat("game_over:             ", if (isTRUE(x$game_over)) "yes" else "no", "\n", sep = "")
  cat("winner:                ", x$winner, "\n", sep = "")
  cat("turn_limit_reached:    ", if (isTRUE(x$turn_limit_reached)) "yes" else "no", "\n", sep = "")
  cat("roll_sequence_exhausted:", if (isTRUE(x$roll_sequence_exhausted)) "yes" else "no", "\n", sep = "")

  if (nrow(x$history) > 0L) {
    print(x$history, row.names = FALSE)
  }

  invisible(x)
}

#' Print a matchup-simulation result
#'
#' @param x A `bg_matchup_simulation` object.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.bg_matchup_simulation <- function(x, ...) {
  if (!is_bg_matchup_simulation(x)) {
    stop("`x` must inherit from class 'bg_matchup_simulation'.", call. = FALSE)
  }

  cat("<bg_matchup_simulation>\n", sep = "")
  cat("player_1: ", x$settings$player1_selection, "\n", sep = "")
  cat("player_2: ", x$settings$player2_selection, "\n", sep = "")
  cat("n_games:  ", x$settings$n_games, "\n", sep = "")
  cat("max_turns:", x$settings$max_turns, "\n", sep = "")
  cat("scripted: ", if (isTRUE(x$settings$used_scripted_rolls)) "yes" else "no", "\n", sep = "")

  if (bg_is_rollout_family_selection(x$settings$player1_selection) ||
      bg_is_rollout_family_selection(x$settings$player2_selection)) {
    cat("rollout_budget: ", x$settings$rollout_budget, "\n", sep = "")
    cat("rollout_policy: ", x$settings$rollout_policy, "\n", sep = "")
    cat("max_rollout_turns: ", x$settings$max_rollout_turns, "\n", sep = "")
  }

  if (!is.null(x$summary)) {
    cat("summary:\n", sep = "")
    print(x$summary, row.names = FALSE)
  } else {
    cat("games (first rows):\n", sep = "")
    print(utils::head(x$games), row.names = FALSE)
  }

  invisible(x)
}

# -----------------------------------------------------------------------------
# Source: bg_s3_methods.R
# -----------------------------------------------------------------------------
# S3 methods and object summaries for research-layer objects.
#
# These methods are intentionally presentation-oriented. They keep printing and
# plotting logic out of the statistical workflow files so those files can focus
# on data construction rather than display.
bg_gg_title <- function(title, subtitle = NULL) {
  ggplot2::labs(title = title, subtitle = subtitle)
}

bg_reference_subtitle <- function(reference) {
  if (is.null(reference)) {
    return("No proxy reference attached; all summaries are model-relative only.")
  }

  if (nrow(reference$action_table) == 0L) {
    return("No legal actions are available in this decision problem.")
  }

  if (isTRUE(reference$summary$mc_gap_excludes_zero[[1L]])) {
    "The proxy-reference top-two Monte Carlo gap interval excludes zero."
  } else {
    "The proxy-reference top-two Monte Carlo gap interval does not exclude zero; treat rank-based conclusions cautiously."
  }
}

bg_focus_action_rows <- function(action_table, top_n = 6L) {
  action_table <- as.data.frame(action_table, stringsAsFactors = FALSE)
  if (nrow(action_table) == 0L) {
    return(action_table)
  }

  top_n <- min(bg_coerce_integerish(top_n, "top_n", 1L), nrow(action_table))
  if ("reference_mean" %in% names(action_table)) {
    ord <- order(action_table$rank, action_table$candidate_index)
    return(action_table[ord[seq_len(top_n)], , drop = FALSE])
  }

  recommended <- if ("recommended" %in% names(action_table)) as.integer(action_table$recommended) else rep.int(0L, nrow(action_table))
  allocation_count <- if ("allocation_count" %in% names(action_table)) action_table$allocation_count else rep.int(0L, nrow(action_table))
  prob_best <- if ("model_relative_prob_best" %in% names(action_table)) {
    action_table$model_relative_prob_best
  } else if ("prob_best" %in% names(action_table)) {
    action_table$prob_best
  } else {
    rep(NA_real_, nrow(action_table))
  }
  estimate <- if ("estimate" %in% names(action_table)) action_table$estimate else rep(NA_real_, nrow(action_table))
  ord <- order(-recommended, -allocation_count, -prob_best, -estimate, action_table$candidate_index, na.last = TRUE)
  action_table[ord[seq_len(top_n)], , drop = FALSE]
}

bg_round_display_table <- function(df, digits = 3L) {
  out <- as.data.frame(df, stringsAsFactors = FALSE)
  numeric_cols <- vapply(out, is.numeric, logical(1L))
  out[numeric_cols] <- lapply(out[numeric_cols], round, digits = digits)
  out
}

bg_plot_problem_object <- function(x) {
  df <- x$candidate_table
  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = stats::reorder(move_label, candidate_index),
      y = n_equivalent_sequences,
      fill = n_steps
    )
  ) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::coord_flip() +
    bg_gg_title(
      sprintf("Collapsed action set: %s", x$problem_id),
      "Bars show how many legal sequences collapse to each unique post-move state."
    ) +
    ggplot2::labs(
      x = "Candidate action",
      y = "Equivalent legal sequences",
      fill = "Steps"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

bg_plot_reference_object <- function(x) {
  df <- x$action_table
  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = stats::reorder(move_label, reference_mean),
      y = reference_mean
    )
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = reference_mc_lower_95, ymax = reference_mc_upper_95),
      width = 0.15,
      color = "#6a51a3"
    ) +
    ggplot2::geom_point(size = 2.3, color = "#08519c") +
    ggplot2::coord_flip() +
    bg_gg_title(
      sprintf("Proxy-reference ranking: %s", x$problem$problem_id),
      bg_reference_subtitle(x)
    ) +
    ggplot2::labs(
      x = "Candidate action",
      y = "Proxy-reference mean"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

#' Plot budget-path diagnostics
#'
#' @param x A `bg_ts_run`, `bg_ts_profile`, or `bg_method_compare` object.
#' @param metric Metric to plot.
#'
#' @return A `ggplot` object.
#' @keywords internal
#' @noRd
plot_budget_path <- function(
    x,
    metric = c("simple_regret", "recommended_prob_best", "selected_reference_rank", "runtime_seconds")) {
  metric <- match.arg(metric)

  if (inherits(x, "bg_ts_run")) {
    df <- x$checkpoint_table
    p <- ggplot2::ggplot(df, ggplot2::aes(x = checkpoint, y = .data[[metric]])) +
      ggplot2::geom_line(color = "#234f7d", linewidth = 0.9) +
      ggplot2::geom_point(color = "#d95f02", size = 2) +
      ggplot2::scale_x_continuous(trans = "log2", breaks = unique(df$checkpoint)) +
      bg_gg_title(
        sprintf("Thompson budget path: %s", x$problem$problem_id),
        bg_reference_subtitle(x$reference)
      ) +
      ggplot2::labs(
        x = "Budget (log2 scale)",
        y = gsub("_", " ", metric)
      ) +
      ggplot2::theme_minimal(base_size = 12)
    return(p)
  }

  if (inherits(x, "bg_ts_profile")) {
    df <- x$results
    p <- ggplot2::ggplot(df, ggplot2::aes(x = checkpoint, y = .data[[metric]], group = seed)) +
      ggplot2::geom_line(alpha = 0.15, color = "#6baed6") +
      ggplot2::stat_summary(
        fun = mean,
        geom = "line",
        linewidth = 1.2,
        color = "#08306b"
      ) +
      ggplot2::stat_summary(fun = mean, geom = "point", size = 2.2, color = "#cb181d") +
      ggplot2::scale_x_continuous(trans = "log2", breaks = unique(df$checkpoint)) +
      bg_gg_title(
        sprintf("Thompson profile over budget: %s", x$problem$problem_id),
        bg_reference_subtitle(x$reference)
      ) +
      ggplot2::labs(
        x = "Budget (log2 scale)",
        y = gsub("_", " ", metric)
      ) +
      ggplot2::theme_minimal(base_size = 12)
    return(p)
  }

  if (inherits(x, "bg_method_compare")) {
    df <- x$results
    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = checkpoint, y = .data[[metric]], color = allocation_policy, group = interaction(problem_id, seed, allocation_policy))
    ) +
      ggplot2::geom_line(alpha = 0.12) +
      ggplot2::stat_summary(
        ggplot2::aes(group = allocation_policy),
        fun = mean,
        geom = "line",
        linewidth = 1.1
      ) +
      ggplot2::facet_wrap(~ problem_id, scales = "free_y") +
      ggplot2::scale_x_continuous(trans = "log2", breaks = unique(df$checkpoint)) +
      bg_gg_title(
        "Method comparison over budget",
        "Curves summarize model-relative decision quality against a proxy reference."
      ) +
      ggplot2::labs(
        x = "Budget (log2 scale)",
        y = gsub("_", " ", metric),
        color = "Allocation policy"
      ) +
      ggplot2::theme_minimal(base_size = 12)
    return(p)
  }

  stop("`plot_budget_path()` does not support this object type.", call. = FALSE)
}

#' Plot allocation flow across budget
#'
#' @param x A `bg_ts_run` object.
#'
#' @return A `ggplot` object.
#' @keywords internal
#' @noRd
plot_allocation_flow <- function(x) {
  if (!inherits(x, "bg_ts_run")) {
    stop("`x` must inherit from class 'bg_ts_run'.", call. = FALSE)
  }

  focus <- bg_focus_action_rows(x$action_table, top_n = 6L)
  df <- x$checkpoint_actions[x$checkpoint_actions$candidate_index %in% focus$candidate_index, , drop = FALSE]
  df$move_label <- factor(df$move_label, levels = focus$move_label)

  ggplot2::ggplot(df, ggplot2::aes(x = checkpoint, y = allocation_count, color = move_label)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.8) +
    bg_gg_title(
      sprintf("Allocation flow: %s", x$problem$problem_id),
      "Top trajectories are chosen by final Thompson allocation and posterior mass."
    ) +
    ggplot2::labs(
      x = "Budget checkpoint",
      y = "Allocation count",
      color = "Action"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

#' Plot model-relative probability-best trajectories
#'
#' @param x A `bg_ts_run` object.
#'
#' @return A `ggplot` object.
#' @keywords internal
#' @noRd
plot_prob_best <- function(x) {
  if (!inherits(x, "bg_ts_run")) {
    stop("`x` must inherit from class 'bg_ts_run'.", call. = FALSE)
  }

  focus <- bg_focus_action_rows(x$action_table, top_n = 6L)
  df <- x$checkpoint_actions[x$checkpoint_actions$candidate_index %in% focus$candidate_index, , drop = FALSE]
  df$move_label <- factor(df$move_label, levels = focus$move_label)

  ggplot2::ggplot(df, ggplot2::aes(x = checkpoint, y = model_relative_prob_best, color = move_label)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.7) +
    bg_gg_title(
      sprintf("Model-relative probability-best path: %s", x$problem$problem_id),
      "Only the most relevant final contenders are shown to avoid legend clutter."
    ) +
    ggplot2::labs(
      x = "Budget checkpoint",
      y = "Model-relative probability-best",
      color = "Action"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

#' Plot seed-by-budget selected-rank heatmap
#'
#' @param x A `bg_ts_profile`, `bg_method_compare`, or `bg_opening_study` object.
#'
#' @return A `ggplot` object.
#' @keywords internal
#' @noRd
plot_seed_heatmap <- function(x) {
  if (inherits(x, "bg_opening_study")) {
    df <- x$metric_panel
    df <- df[df$allocation_policy == "thompson", , drop = FALSE]
    df$panel <- if ("opening_roll" %in% names(df)) df$opening_roll else df$problem_id
  } else if (inherits(x, "bg_method_compare")) {
    df <- x$results
    df$panel <- paste(df$problem_id, df$allocation_policy)
  } else if (inherits(x, "bg_ts_profile")) {
    df <- x$results
    df$panel <- x$problem$problem_id
  } else {
    stop("`plot_seed_heatmap()` does not support this object type.", call. = FALSE)
  }

  ggplot2::ggplot(df, ggplot2::aes(x = factor(checkpoint), y = factor(seed), fill = selected_reference_rank)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::facet_wrap(~ panel, scales = "free") +
    ggplot2::scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "grey90") +
    bg_gg_title(
      "Selected-rank heatmap across seeds and budgets",
      "Ranks are relative to the proxy-reference ordering where available."
    ) +
    ggplot2::labs(
      x = "Budget checkpoint",
      y = "Seed",
      fill = "Selected reference rank"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

#' Plot opening-roll atlas
#'
#' @param x A `bg_opening_study` object.
#'
#' @return A `ggplot` object.
#' @keywords internal
#' @noRd
plot_opening_atlas <- function(x) {
  if (!inherits(x, "bg_opening_study")) {
    stop("`x` must inherit from class 'bg_opening_study'.", call. = FALSE)
  }

  summary_df <- x$leaderboard
  summary_df$panel <- summary_df$opening_roll

  ggplot2::ggplot(summary_df, ggplot2::aes(x = checkpoint, y = simple_regret, color = method)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::facet_wrap(~ panel, scales = "free_y") +
    bg_gg_title(
      "Opening-roll atlas",
      "Simple regret vs budget by opening roll; regret is measured against a proxy reference."
    ) +
    ggplot2::labs(
      x = "Budget checkpoint",
      y = "Simple regret",
      color = "Method"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

#' Plot Thompson behavior through a game
#'
#' @param x A `bg_game_trace` object.
#'
#' @return A `ggplot` object.
#' @keywords internal
#' @noRd
plot_game_trace <- function(x) {
  if (!inherits(x, "bg_game_trace")) {
    stop("`x` must inherit from class 'bg_game_trace'.", call. = FALSE)
  }

  ggplot2::ggplot(x$node_table, ggplot2::aes(x = node_index, y = recommended_prob_best, color = phase)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 2) +
    bg_gg_title(
      "Thompson decision behavior through a game trace",
      "Each point is one local decision problem along the analyzed trace."
    ) +
    ggplot2::labs(
      x = "Decision node",
      y = "Recommended action probability-best",
      color = "Phase"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

#' Plot experimental structure map
#'
#' @param x A `bg_structure_study` object.
#'
#' @return A `ggplot` object.
#' @keywords internal
#' @noRd
plot_structure_map <- function(x) {
  if (!inherits(x, "bg_structure_study")) {
    stop("`x` must inherit from class 'bg_structure_study'.", call. = FALSE)
  }

  df <- x$feature_table
  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = own_pip_count,
      y = own_contact_checkers,
      color = ts_regret_gain,
      shape = split
    )
  ) +
    ggplot2::geom_point(size = 3, alpha = 0.8) +
    bg_gg_title(
      "Experimental structure map",
      "Color shows Thompson regret gain over the baseline; this is a feature study, not ground truth."
    ) +
    ggplot2::labs(
      x = "Own pip count",
      y = "Own contact checkers",
      color = "TS regret gain",
      shape = "Split"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

#' @export
summary.bg_problem <- function(object, ...) {
  object$candidate_table
}

#' @export
print.bg_problem <- function(x, ...) {
  cat("<bg_problem>\n", sep = "")
  cat("problem_id:            ", x$problem_id, "\n", sep = "")
  cat("simulation_policy:     ", x$settings$simulation_policy, "\n", sep = "")
  cat("simulation_policy_raw: ", x$settings$simulation_policy_engine, "\n", sep = "")
  cat("n_legal_moves:         ", length(x$legal_moves), "\n", sep = "")
  cat("n_unique_candidates:   ", nrow(x$candidate_table), "\n", sep = "")
  cat("max_rollout_turns:     ", x$settings$max_rollout_turns, "\n", sep = "")
  cat("unresolved_value:      ", x$settings$unresolved_value, "\n", sep = "")
  if (!is.null(x$settings$simulation_policy_note)) {
    cat("note:                  ", x$settings$simulation_policy_note, "\n", sep = "")
  }
  invisible(x)
}

#' @export
plot.bg_problem <- function(x, ...) {
  print(bg_plot_problem_object(x))
  invisible(x)
}

#' @export
summary.bg_reference <- function(object, ...) {
  object$summary
}

#' @export
print.bg_reference <- function(x, ...) {
  cat("<bg_reference>\n", sep = "")
  cat("problem_id:                 ", x$problem$problem_id, "\n", sep = "")
  cat("reference_budget:           ", x$summary$reference_budget[[1L]], "\n", sep = "")
  cat("proxy_reference_best_move:  ", x$summary$proxy_reference_best_move_label[[1L]], "\n", sep = "")
  cat("top_two_gap_estimate:       ", format(x$summary$top_two_gap_estimate[[1L]], digits = 4), "\n", sep = "")
  cat("mc_gap_excludes_zero:       ", x$summary$mc_gap_excludes_zero[[1L]], "\n", sep = "")
  cat("difficulty_label:           ", x$summary$difficulty_label[[1L]], "\n", sep = "")
  if (nrow(x$action_table) > 0L) {
    display <- bg_compact_action_table(bg_focus_action_rows(x$action_table, top_n = 6L), n = 6L)
    display <- display[, intersect(c("rank", "action", "alloc_n", "estimate", "ci95_low", "ci95_high", "unresolved_frac"), names(display)), drop = FALSE]
    names(display)[names(display) == "action"] <- "move_label"
    names(display)[names(display) == "estimate"] <- "reference_mean"
    print(bg_round_display_table(display), row.names = FALSE)
  }
  if (length(x$warnings) > 0L) {
    cat("warnings:\n", sep = "")
    for (msg in x$warnings) {
      cat(" - ", msg, "\n", sep = "")
    }
  }
  invisible(x)
}

#' @export
plot.bg_reference <- function(x, ...) {
  print(bg_plot_reference_object(x))
  invisible(x)
}

#' @export
summary.bg_ts_run <- function(object, ...) {
  object$checkpoint_table[object$checkpoint_table$checkpoint == object$budget, , drop = FALSE]
}

#' @export
print.bg_ts_run <- function(x, ...) {
  final <- summary(x)
  cat("<bg_ts_run>\n", sep = "")
  cat("problem_id:               ", x$problem$problem_id, "\n", sep = "")
  cat("allocation_policy:        ", x$allocation_policy, "\n", sep = "")
  cat("ts_mode:                  ", x$ts_mode, "\n", sep = "")
  cat("budget:                   ", x$budget, "\n", sep = "")
  cat("recommended_move:         ", x$recommended_move_label, "\n", sep = "")
  cat("recommended_estimate:     ", format(final$recommended_estimate[[1L]], digits = 4), "\n", sep = "")
  cat("recommended_prob_best:    ", format(final$recommended_prob_best[[1L]], digits = 4), "\n", sep = "")
  if (!is.na(final$simple_regret[[1L]])) {
    cat("simple_regret:            ", format(final$simple_regret[[1L]], digits = 4), "\n", sep = "")
  }
  cat("runtime_seconds:          ", format(final$runtime_seconds[[1L]], digits = 4), "\n", sep = "")
  if (nrow(x$action_table) > 0L) {
    display <- bg_compact_action_table(bg_focus_action_rows(x$action_table, top_n = 6L), n = 6L)
    display <- display[, intersect(c("rank", "action", "recommended", "alloc_n", "estimate", "uncertainty_sd", "prob_best", "reference_rank", "simple_regret"), names(display)), drop = FALSE]
    names(display)[names(display) == "action"] <- "move_label"
    print(bg_round_display_table(display), row.names = FALSE)
  }
  if (length(x$warnings) > 0L) {
    cat("warnings:\n", sep = "")
    for (msg in x$warnings) {
      cat(" - ", msg, "\n", sep = "")
    }
  }
  invisible(x)
}

#' @export
summary.bg_ts_profile <- function(object, ...) {
  object$summary
}

#' @export
print.bg_ts_profile <- function(x, ...) {
  cat("<bg_ts_profile>\n", sep = "")
  cat("problem_id:            ", x$problem$problem_id, "\n", sep = "")
  cat("allocation_policy:     ", x$settings$allocation_policy, "\n", sep = "")
  cat("ts_mode:               ", x$settings$ts_mode, "\n", sep = "")
  cat("n_seeds:               ", length(x$settings$seeds), "\n", sep = "")
  cat("budgets:               ", paste(x$settings$budgets, collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' @export
summary.bg_method_compare <- function(object, ...) {
  object$summary
}

#' @export
print.bg_method_compare <- function(x, ...) {
  cat("<bg_method_compare>\n", sep = "")
  cat("n_problems:            ", length(x$problems), "\n", sep = "")
  cat("methods:               ", paste(x$settings$methods, collapse = ", "), "\n", sep = "")
  cat("budgets:               ", paste(x$settings$budgets, collapse = ", "), "\n", sep = "")
  cat("n_seeds:               ", length(x$settings$seeds), "\n", sep = "")
  invisible(x)
}

#' @export
summary.bg_truth_stability <- function(object, ...) {
  object$summary
}

#' @export
print.bg_truth_stability <- function(x, ...) {
  cat("<bg_truth_stability>\n", sep = "")
  cat("n_problems:            ", length(x$problems), "\n", sep = "")
  cat("budgets:               ", paste(x$settings$budgets, collapse = ", "), "\n", sep = "")
  cat("n_seeds:               ", length(x$settings$seeds), "\n", sep = "")
  stable_rate <- mean(x$summary$reference_stability_label == "reference_stable", na.rm = TRUE)
  clear_rate <- mean(x$summary$decision_screen_label == "decision_clear", na.rm = TRUE)
  cat("reference_stable_rate: ", format(stable_rate, digits = 3), "\n", sep = "")
  cat("decision_clear_rate:   ", format(clear_rate, digits = 3), "\n", sep = "")
  invisible(x)
}

#' @export
summary.bg_opening_compare_study <- function(object, ...) {
  object$opening_aggregate
}

#' @export
print.bg_opening_compare_study <- function(x, ...) {
  cat("<bg_opening_compare_study>\n", sep = "")
  cat("opening_rolls:         ", length(unique(x$opening_summary$opening_roll)), "\n", sep = "")
  cat("methods:               ", paste(unique(x$opening_summary$allocation_policy), collapse = ", "), "\n", sep = "")
  cat("budgets:               ", paste(x$settings$budgets, collapse = ", "), "\n", sep = "")
  cat("n_seeds:               ", length(x$settings$seeds), "\n", sep = "")
  invisible(x)
}

#' @export
summary.bg_opening_study <- function(object, ...) {
  object$difficulty_table
}

#' @export
print.bg_opening_study <- function(x, ...) {
  cat("<bg_opening_study>\n", sep = "")
  cat("opening_rolls:         ", length(x$problems), "\n", sep = "")
  if (!is.null(x$difficulty_table$is_double)) {
    cat("double_rolls:          ", sum(x$difficulty_table$is_double), "\n", sep = "")
    cat("non_double_rolls:      ", sum(!x$difficulty_table$is_double), "\n", sep = "")
  }
  cat("methods:               ", paste(x$settings$methods, collapse = ", "), "\n", sep = "")
  cat("budgets:               ", paste(x$settings$budgets, collapse = ", "), "\n", sep = "")
  cat("n_seeds:               ", length(x$settings$seeds), "\n", sep = "")
  invisible(x)
}

#' @export
summary.bg_game_trace <- function(object, ...) {
  object$summary
}

#' @export
print.bg_game_trace <- function(x, ...) {
  cat("<bg_game_trace>\n", sep = "")
  cat("n_nodes:               ", length(x$problems), "\n", sep = "")
  cat("local_budget:          ", x$settings$local_budget, "\n", sep = "")
  cat("n_reference_nodes:     ", x$settings$n_reference_nodes, "\n", sep = "")
  invisible(x)
}

#' @export
summary.bg_structure_study <- function(object, ...) {
  object$feature_table
}

#' @export
print.bg_structure_study <- function(x, ...) {
  cat("<bg_structure_study>\n", sep = "")
  cat("budget:                ", x$settings$budget, "\n", sep = "")
  cat("baseline:              ", x$settings$baseline, "\n", sep = "")
  cat("n_problems:            ", nrow(x$feature_table), "\n", sep = "")
  cat("warning:               ", x$warnings, "\n", sep = "")
  invisible(x)
}

#' @export
plot.bg_ts_run <- function(x, ...) {
  print(plot_budget_path(x, metric = "recommended_prob_best"))
  invisible(x)
}

#' @export
plot.bg_ts_profile <- function(x, ...) {
  print(plot_budget_path(x, metric = "simple_regret"))
  invisible(x)
}

#' @export
plot.bg_method_compare <- function(x, ...) {
  print(plot_budget_path(x, metric = "simple_regret"))
  invisible(x)
}

#' @export
plot.bg_opening_study <- function(x, ...) {
  print(plot_opening_atlas(x))
  invisible(x)
}

#' @export
plot.bg_game_trace <- function(x, ...) {
  print(plot_game_trace(x))
  invisible(x)
}

#' @export
plot.bg_structure_study <- function(x, ...) {
  print(plot_structure_map(x))
  invisible(x)
}

#' @exportS3Method tibble::as_tibble
as_tibble.bg_problem <- function(x, ...) {
  tibble::as_tibble(x$candidate_table)
}

#' @exportS3Method tibble::as_tibble
as_tibble.bg_reference <- function(x, ...) {
  tibble::as_tibble(x$action_table)
}

#' @exportS3Method tibble::as_tibble
as_tibble.bg_ts_run <- function(x, ...) {
  tibble::as_tibble(x$checkpoint_table)
}

#' @exportS3Method tibble::as_tibble
as_tibble.bg_ts_profile <- function(x, ...) {
  tibble::as_tibble(x$results)
}

#' @exportS3Method tibble::as_tibble
as_tibble.bg_method_compare <- function(x, ...) {
  tibble::as_tibble(x$results)
}

#' @exportS3Method tibble::as_tibble
as_tibble.bg_opening_study <- function(x, ...) {
  tibble::as_tibble(x$metric_panel)
}

#' @exportS3Method tibble::as_tibble
as_tibble.bg_game_trace <- function(x, ...) {
  tibble::as_tibble(x$node_table)
}

#' @exportS3Method tibble::as_tibble
as_tibble.bg_structure_study <- function(x, ...) {
  tibble::as_tibble(x$feature_table)
}

#' @exportS3Method ggplot2::autoplot
autoplot.bg_problem <- function(object, ...) {
  bg_plot_problem_object(object)
}

#' @exportS3Method ggplot2::autoplot
autoplot.bg_reference <- function(object, ...) {
  bg_plot_reference_object(object)
}

#' @exportS3Method ggplot2::autoplot
autoplot.bg_ts_run <- function(object, ...) {
  plot_budget_path(object, metric = "recommended_prob_best")
}

#' @exportS3Method ggplot2::autoplot
autoplot.bg_ts_profile <- function(object, ...) {
  plot_budget_path(object, metric = "simple_regret")
}

#' @exportS3Method ggplot2::autoplot
autoplot.bg_method_compare <- function(object, ...) {
  plot_budget_path(object, metric = "simple_regret")
}

#' @exportS3Method ggplot2::autoplot
autoplot.bg_opening_study <- function(object, ...) {
  plot_opening_atlas(object)
}

#' @exportS3Method ggplot2::autoplot
autoplot.bg_game_trace <- function(object, ...) {
  plot_game_trace(object)
}

#' @exportS3Method ggplot2::autoplot
autoplot.bg_structure_study <- function(object, ...) {
  plot_structure_map(object)
}

# Public plot builders for truth objects, TS runs, and comparison studies.
#
# The plots in this file are intentionally tied to the package's current
# research story: proxy truths, TS/TTTS allocation behavior, and compact study
# summaries. The palette only includes live policy labels.
bg_plot_theme_research <- function() {
  ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "#d6dee6", linewidth = 0.35),
      panel.grid.major.y = ggplot2::element_line(color = "#e8edf1", linewidth = 0.35),
      panel.border = ggplot2::element_rect(color = "#d9e1e8", fill = NA, linewidth = 0.6),
      plot.background = ggplot2::element_rect(fill = "#fbfaf7", color = NA),
      panel.background = ggplot2::element_rect(fill = "#fbfaf7", color = NA),
      plot.title.position = "plot",
      legend.position = "bottom",
      legend.box = "vertical",
      legend.margin = ggplot2::margin(t = 4, r = 0, b = 0, l = 0),
      legend.box.margin = ggplot2::margin(t = 2, r = 0, b = 0, l = 0),
      panel.spacing = grid::unit(0.9, "lines"),
      strip.text = ggplot2::element_text(face = "bold"),
      strip.background = ggplot2::element_rect(fill = "#eef3f7", color = "#d9e1e8"),
      plot.title = ggplot2::element_text(face = "bold", size = 16),
      plot.subtitle = ggplot2::element_text(color = "#405261", margin = ggplot2::margin(b = 8)),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 8)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 8)),
      axis.text = ggplot2::element_text(color = "#24323d"),
      legend.title = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::margin(t = 12, r = 16, b = 10, l = 10)
    )
}

bg_method_palette <- function() {
  c(
    thompson = "#0072B2",
    top_two_thompson = "#D55E00",
    multi_sample_thompson = "#009E73",
    soft_elimination_thompson = "#CC79A7",
    forced_exploration_thompson = "#E69F00",
    top_k_thompson = "#56B4E9",
    equal = "#6C757D",
    ucb = "#332288",
    ocba = "#AA4499",
    greedy = "#999933",
    elimination_thompson = "#CC79A7",
    ranking_aware_thompson = "#56B4E9"
  )
}

bg_lookup_palette_values <- function(keys) {
  palette <- bg_method_palette()
  missing <- setdiff(keys, names(palette))
  if (length(missing) > 0L) {
    extra <- stats::setNames(
      grDevices::hcl.colors(length(missing), palette = "Zissou 1"),
      missing
    )
    palette <- c(palette, extra)
  }
  palette[keys]
}

bg_metric_label <- function(metric) {
  switch(
    metric,
    simple_regret = "Simple regret",
    top1_match = "Top-1 match rate",
    selected_reference_rank = "Truth rank of selected move",
    truth_top2_hit = "Selected move is in truth top-2",
    spearman = "Spearman rank correlation",
    top_k_overlap = "Top-k overlap",
    share_top_k_truth = "Budget share on truth top-k",
    share_top2_truth = "Budget share on truth top-2",
    share_mc_screened_suboptimal = "Budget share on MC-screened suboptimal moves",
    gap_weighted_wasted_allocation = "Gap-weighted wasted allocation",
    allocation = "Allocation count",
    prob_best = "Posterior probability-best",
    estimate = "Posterior mean estimate",
    selection_score = "Selection score",
    truth_gap = "Proxy-truth top-two gap",
    class_count = "States",
    allocation_count = "Allocation count",
    runtime_seconds = "Runtime (seconds)",
    elapsed_seconds = "Elapsed time (seconds)",
    metric
  )
}

bg_plot_method_order <- function() {
  c(
    "thompson",
    "top_two_thompson",
    "multi_sample_thompson",
    "soft_elimination_thompson",
    "forced_exploration_thompson",
    "top_k_thompson",
    "equal",
    "ucb",
    "ocba",
    "greedy"
  )
}

bg_plot_method_spec <- function(methods) {
  methods <- unique(as.character(methods))
  ordered_methods <- c(
    intersect(bg_plot_method_order(), methods),
    setdiff(methods, bg_plot_method_order())
  )
  labels <- vapply(ordered_methods, bg_allocation_policy_label, character(1L))
  list(
    methods = ordered_methods,
    labels = labels,
    palette = stats::setNames(bg_lookup_palette_values(ordered_methods), labels)
  )
}

bg_plot_run_trace <- function(x, metric, top_n, title_prefix, subtitle) {
  if (!inherits(x, "bg_ts_run")) {
    stop("`x` must inherit from class 'bg_ts_run'.", call. = FALSE)
  }

  top_n <- bg_coerce_integerish(top_n, "top_n", 1L)
  final_tab <- x$action_table[order(-x$action_table$allocation_count, -x$action_table$estimate), , drop = FALSE]
  focus_ids <- utils::head(final_tab$candidate_index, top_n)
  df <- x$checkpoint_actions[x$checkpoint_actions$candidate_index %in% focus_ids, , drop = FALSE]
  df$move_label <- factor(df$move_label, levels = final_tab$move_label[match(focus_ids, final_tab$candidate_index)])

  value_col <- switch(
    metric,
    allocation = "allocation_count",
    prob_best = "model_relative_prob_best",
    estimate = "estimate",
    selection_score = "selection_score"
  )

  ggplot2::ggplot(df, ggplot2::aes(x = checkpoint, y = .data[[value_col]], color = move_label)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::labs(
      title = sprintf("%s for %s", title_prefix, x$problem$problem_id),
      subtitle = subtitle,
      x = "Budget checkpoint",
      y = bg_metric_label(metric),
      color = "Move"
    ) +
    bg_plot_theme_research()
}

bg_plot_curve_summary <- function(df, value_col) {
  split_key <- interaction(df$problem_id, df$allocation_policy, df$checkpoint, drop = TRUE)
  rows <- lapply(
    split(df, split_key),
    function(chunk) {
      data.frame(
        problem_id = chunk$problem_id[[1L]],
        allocation_policy = chunk$allocation_policy[[1L]],
        checkpoint = chunk$checkpoint[[1L]],
        mean_value = mean(chunk[[value_col]], na.rm = TRUE),
        sd_value = stats::sd(chunk[[value_col]], na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }
  )
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Plot proxy truth or truth batteries
#'
#' @param x A `bg_truth_state`, `bg_reference`, or `bg_truth_battery` object.
#' @param top_n Integer-like number of actions to show for single-state truth
#'   plots.
#'
#' @return A `ggplot` object.
#' @export
plot_bg_truth <- function(x, top_n = 8L) {
  top_n <- bg_coerce_integerish(top_n, "top_n", 1L)

  if (inherits(x, "bg_truth_battery")) {
    df <- x$summary
    label_col <- if ("opening_roll" %in% names(df)) "opening_roll" else "problem_id"
    df$label <- df[[label_col]]
    df <- df[order(df$top_two_gap_estimate), , drop = FALSE]
    df$label <- factor(df$label, levels = df$label)
    has_gap_interval <- all(c("top_two_gap_mc_lower_95", "top_two_gap_mc_upper_95") %in% names(df))

    return(
      ggplot2::ggplot(df, ggplot2::aes(x = top_two_gap_estimate, y = label, color = mc_gap_excludes_zero)) +
        if (has_gap_interval) {
          ggplot2::geom_segment(
            ggplot2::aes(
              x = top_two_gap_mc_lower_95,
              xend = top_two_gap_mc_upper_95,
              y = label,
              yend = label
            ),
            linewidth = 1.4,
            alpha = 0.45
          )
        } else {
          ggplot2::geom_blank()
        } +
        ggplot2::geom_segment(
          ggplot2::aes(x = 0, xend = top_two_gap_estimate, y = label, yend = label),
          linewidth = 0.8
        ) +
        ggplot2::geom_point(ggplot2::aes(size = n_moves), alpha = 0.9) +
        ggplot2::scale_color_manual(values = c(`TRUE` = "#0b4f6c", `FALSE` = "#c9643b")) +
        ggplot2::labs(
          title = if (identical(label_col, "opening_roll")) {
            "Opening truth gap ladder"
          } else {
            "Proxy-truth separation across states"
          },
          subtitle = "Each point is the estimated gap between the best and second-best move under the rollout model.",
          x = "Estimated top-two gap",
          y = NULL,
          color = "MC gap excludes 0",
          size = "Legal moves"
        ) +
        bg_plot_theme_research()
    )
  }

  ref <- bg_normalize_truth_reference(x, "x")
  df <- ref$action_table[order(ref$action_table$rank), , drop = FALSE]
  df <- utils::head(df, top_n)
  df$move_label <- factor(df$move_label, levels = rev(df$move_label))

  ggplot2::ggplot(df, ggplot2::aes(x = reference_mean, y = move_label)) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = reference_mc_lower_95,
        xend = reference_mc_upper_95,
        y = move_label,
        yend = move_label
      ),
      linewidth = 1.2,
      color = "#7a8ea3"
    ) +
    ggplot2::geom_point(color = "#0b4f6c", size = 2.8) +
    ggplot2::labs(
      title = sprintf("Proxy truth for %s", ref$problem$problem_id),
      subtitle = "Intervals show Monte Carlo uncertainty for the rollout-model value of each move.",
      x = "Proxy-reference mean",
      y = NULL
    ) +
    bg_plot_theme_research()
}

#' Plot Thompson allocation or posterior traces
#'
#' @param x A `bg_ts_run` object.
#' @param metric Trace metric.
#' @param top_n Integer-like number of actions to display.
#'
#' @return A `ggplot` object.
#' @export
plot_bg_ts_trace <- function(x, metric = c("allocation", "prob_best", "estimate"), top_n = 6L) {
  if (!inherits(x, "bg_ts_run")) {
    stop("`x` must inherit from class 'bg_ts_run'.", call. = FALSE)
  }

  metric <- match.arg(metric)
  bg_plot_run_trace(
    x = x,
    metric = metric,
    top_n = top_n,
    title_prefix = bg_allocation_policy_label(x$allocation_policy),
    subtitle = "Only the main contenders at the final checkpoint are shown."
  )
}

#' Plot a UCB allocation trace
#'
#' @param x A `bg_ts_run` object created by [bg_ucb_run()] or another UCB path.
#' @param metric Trace metric.
#' @param top_n Integer-like number of actions to display.
#'
#' @return A `ggplot` object.
#' @keywords internal
#' @noRd
plot_bg_ucb_trace <- function(x, metric = c("allocation", "estimate", "selection_score"), top_n = 6L) {
  if (!inherits(x, "bg_ts_run")) {
    stop("`x` must inherit from class 'bg_ts_run'.", call. = FALSE)
  }
  if (!identical(x$allocation_policy, "ucb")) {
    stop("`plot_bg_ucb_trace()` expects a run with `allocation_policy = 'ucb'`.", call. = FALSE)
  }

  metric <- match.arg(metric)
  bg_plot_run_trace(
    x = x,
    metric = metric,
    top_n = top_n,
    title_prefix = "UCB trace",
    subtitle = "UCB emphasizes optimism under uncertainty rather than posterior probability matching."
  )
}

#' Plot budget-performance curves
#'
#' @param x A supported TS/comparison object.
#' @param metric Performance metric to visualize.
#' @param truth Optional truth object used instead of embedded proxy
#'   references.
#' @param checkpoints Optional checkpoint vector.
#' @param top_k Integer-like top-k value used by rank/allocation metrics.
#'
#' @return A `ggplot` object.
#' @export
plot_bg_budget_curve <- function(
    x,
    metric = c(
      "simple_regret",
      "top1_match",
      "selected_reference_rank",
      "truth_top2_hit",
      "spearman",
      "top_k_overlap",
      "share_top_k_truth",
      "share_top2_truth",
      "share_mc_screened_suboptimal",
      "gap_weighted_wasted_allocation"
    ),
    truth = NULL,
    checkpoints = NULL,
    top_k = 3L) {
  metric <- match.arg(metric)

  raw <- switch(
    metric,
    simple_regret = bg_eval_top1(x, truth = truth, checkpoints = checkpoints),
    top1_match = bg_eval_top1(x, truth = truth, checkpoints = checkpoints),
    selected_reference_rank = bg_eval_reference_aware(x, truth = truth, checkpoints = checkpoints, top_k = top_k),
    truth_top2_hit = bg_eval_reference_aware(x, truth = truth, checkpoints = checkpoints, top_k = top_k),
    spearman = bg_eval_rank(x, truth = truth, checkpoints = checkpoints, top_k = top_k),
    top_k_overlap = bg_eval_rank(x, truth = truth, checkpoints = checkpoints, top_k = top_k),
    share_top_k_truth = bg_eval_allocation(x, truth = truth, checkpoints = checkpoints, top_k = top_k),
    share_top2_truth = bg_eval_allocation(x, truth = truth, checkpoints = checkpoints, top_k = top_k),
    share_mc_screened_suboptimal = bg_eval_allocation(x, truth = truth, checkpoints = checkpoints, top_k = top_k),
    gap_weighted_wasted_allocation = bg_eval_allocation(x, truth = truth, checkpoints = checkpoints, top_k = top_k)
  )

  value_col <- switch(
    metric,
    simple_regret = "simple_regret",
    top1_match = "top1_match",
    selected_reference_rank = "selected_reference_rank",
    truth_top2_hit = "truth_top2_hit",
    spearman = "spearman",
    top_k_overlap = "top_k_overlap",
    share_top_k_truth = "share_top_k_truth",
    share_top2_truth = "share_top2_truth",
    share_mc_screened_suboptimal = "share_mc_screened_suboptimal",
    gap_weighted_wasted_allocation = "gap_weighted_wasted_allocation"
  )

  summary_df <- bg_plot_curve_summary(raw, value_col = value_col)
  method_spec <- bg_plot_method_spec(summary_df$allocation_policy)
  summary_df$method_label <- factor(
    vapply(summary_df$allocation_policy, bg_allocation_policy_label, character(1L)),
    levels = method_spec$labels
  )

  p <- ggplot2::ggplot(
    summary_df,
    ggplot2::aes(x = checkpoint, y = mean_value, color = method_label, fill = method_label)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
      alpha = 0.12,
      linewidth = 0,
      show.legend = FALSE
    ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_color_manual(values = method_spec$palette, drop = FALSE) +
    ggplot2::scale_fill_manual(values = method_spec$palette, drop = FALSE) +
    ggplot2::labs(
      title = "Budget-performance curve",
      subtitle = "Lines show mean performance across seeds; ribbons show one standard deviation where available.",
      x = "Budget checkpoint",
      y = bg_metric_label(metric),
      color = "Method"
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 2, byrow = TRUE)) +
    bg_plot_theme_research()

  if (length(unique(summary_df$problem_id)) > 1L) {
    p <- p + ggplot2::facet_wrap(~ problem_id, scales = "free_y", ncol = min(3L, length(unique(summary_df$problem_id))))
  }

  p
}

#' Plot posterior-family budget curves
#'
#' @param x A `bg_posterior_compare` or `bg_reward_model_compare` object.
#' @param metric Performance metric to visualize.
#' @param top_k Integer-like top-k value used by rank-based metrics.
#'
#' @return A `ggplot` object.
#' @export
plot_bg_posterior_compare <- function(
    x,
    metric = c("simple_regret", "top1_match", "spearman", "top_k_overlap"),
    top_k = 3L) {
  if (!inherits(x, "bg_posterior_compare") && !inherits(x, "bg_reward_model_compare")) {
    stop("`x` must inherit from `bg_posterior_compare` or `bg_reward_model_compare`.", call. = FALSE)
  }

  metric <- match.arg(metric)
  title <- if (inherits(x, "bg_reward_model_compare")) {
    "Reward-model comparison"
  } else {
    "Posterior-family comparison"
  }
  subtitle <- if (inherits(x, "bg_reward_model_compare")) {
    "Each line compares one coherent reward-model/posterior-model stack under the same TS allocation policy."
  } else {
    "Each line is Thompson sampling under a different posterior family for the same reward definition."
  }

  plot_bg_budget_curve(x, metric = metric, top_k = top_k) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle
    )
}

#' Plot estimated values against proxy truth
#'
#' @param x A `bg_ts_run` object.
#' @param truth Optional truth object used instead of the embedded proxy
#'   reference.
#' @param checkpoint Optional checkpoint. Defaults to the final budget.
#' @param top_n Integer-like number of truth-ranked actions to display.
#'
#' @return A `ggplot` object.
#' @export
plot_bg_rank_compare <- function(x, truth = NULL, checkpoint = NULL, top_n = 8L) {
  if (!inherits(x, "bg_ts_run")) {
    stop("`x` must inherit from class 'bg_ts_run'.", call. = FALSE)
  }

  if (is.null(checkpoint)) {
    checkpoint <- x$budget
  }
  top_n <- bg_coerce_integerish(top_n, "top_n", 1L)

  reference <- bg_eval_reference_for_run(x, truth = truth)
  if (is.null(reference)) {
    stop("A proxy reference is required for `plot_bg_rank_compare()`.", call. = FALSE)
  }

  tab <- bg_eval_action_table_at_checkpoint(x, checkpoint, reference = reference)
  ref_tab <- reference$action_table[order(reference$action_table$rank), , drop = FALSE]
  df <- merge(
    ref_tab[, c("candidate_index", "move_label", "reference_mean", "rank"), drop = FALSE],
    tab[, c("candidate_index", "estimate", "recommended"), drop = FALSE],
    by = "candidate_index",
    all.x = TRUE,
    sort = FALSE
  )
  df <- utils::head(df[order(df$rank), , drop = FALSE], top_n)
  df$move_label <- factor(df$move_label, levels = rev(df$move_label))

  ggplot2::ggplot(df, ggplot2::aes(y = move_label)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = reference_mean, xend = estimate, yend = move_label),
      linewidth = 1.1,
      color = "#9aa6b2"
    ) +
    ggplot2::geom_point(ggplot2::aes(x = reference_mean), color = "#0b4f6c", size = 2.8) +
    ggplot2::geom_point(ggplot2::aes(x = estimate, fill = recommended), shape = 21, size = 3.2, color = "#222222") +
    ggplot2::scale_fill_manual(values = c(`TRUE` = "#c9643b", `FALSE` = "#f3d6c0")) +
    ggplot2::labs(
      title = sprintf("Estimated values vs proxy truth at budget %d", checkpoint),
      subtitle = "Blue points are proxy truth; filled circles show the finite-budget estimate.",
      x = "Move value under the rollout model",
      y = NULL,
      fill = "Recommended"
    ) +
    bg_plot_theme_research()
}

#' Plot runtime by posterior or reward-model stack
#'
#' @param x A comparison object with repeated-seed results.
#'
#' @return A `ggplot` object.
#' @keywords internal
#' @noRd
plot_bg_runtime_compare <- function(x) {
  if (!inherits(x, "bg_method_compare") && !inherits(x, "bg_posterior_compare") && !inherits(x, "bg_reward_model_compare")) {
    stop("`x` must contain comparison results.", call. = FALSE)
  }

  df <- x$results
  summary_df <- bg_plot_curve_summary(df, value_col = "runtime_seconds")
  palette_vals <- bg_lookup_palette_values(unique(summary_df$allocation_policy))

  ggplot2::ggplot(
    summary_df,
    ggplot2::aes(x = checkpoint, y = mean_value, color = allocation_policy, fill = allocation_policy)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = pmax(mean_value - sd_value, 0), ymax = mean_value + sd_value),
      alpha = 0.12,
      linewidth = 0,
      show.legend = FALSE
    ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_color_manual(values = palette_vals) +
    ggplot2::scale_fill_manual(values = palette_vals) +
    ggplot2::labs(
      title = "Runtime by comparison stack",
      subtitle = "Lines show mean elapsed runtime across seeds; ribbons show one standard deviation.",
      x = "Budget checkpoint",
      y = "Runtime (seconds)",
      color = "Stack"
    ) +
    bg_plot_theme_research()
}

#' Plot seed variability over budget
#'
#' @param x A supported TS/comparison object.
#' @param metric Metric to visualize across seeds.
#' @param truth Optional truth object used instead of embedded proxy
#'   references.
#' @param checkpoints Optional checkpoint vector.
#'
#' @return A `ggplot` object.
#' @keywords internal
#' @noRd
plot_bg_seed_variability <- function(
    x,
    metric = c("simple_regret", "top1_match", "spearman"),
    truth = NULL,
    checkpoints = NULL) {
  metric <- match.arg(metric)

  raw <- switch(
    metric,
    simple_regret = bg_eval_top1(x, truth = truth, checkpoints = checkpoints),
    top1_match = bg_eval_top1(x, truth = truth, checkpoints = checkpoints),
    spearman = bg_eval_rank(x, truth = truth, checkpoints = checkpoints)
  )

  value_col <- switch(
    metric,
    simple_regret = "simple_regret",
    top1_match = "top1_match",
    spearman = "spearman"
  )

  palette_vals <- bg_lookup_palette_values(unique(raw$allocation_policy))

  p <- ggplot2::ggplot(
    raw,
    ggplot2::aes(
      x = checkpoint,
      y = .data[[value_col]],
      color = allocation_policy,
      group = interaction(problem_id, allocation_policy, seed)
    )
  ) +
    ggplot2::geom_line(alpha = 0.18) +
    ggplot2::stat_summary(
      ggplot2::aes(group = interaction(problem_id, allocation_policy)),
      fun = mean,
      geom = "line",
      linewidth = 1.1
    ) +
    ggplot2::scale_color_manual(values = palette_vals) +
    ggplot2::labs(
      title = "Seed variability over budget",
      subtitle = "Thin lines show individual seeds; bold lines show the mean path.",
      x = "Budget checkpoint",
      y = bg_metric_label(metric),
      color = "Method"
    ) +
    bg_plot_theme_research()

  if (length(unique(raw$problem_id)) > 1L) {
    p <- p + ggplot2::facet_wrap(~ problem_id, scales = "free_y")
  }

  p
}

#' Plot a sampled state battery
#'
#' @param x A `bg_state_battery` object.
#' @param metric Visualization target.
#'
#' @return A `ggplot` object.
#' @keywords internal
#' @noRd
plot_bg_state_battery <- function(x, metric = c("truth_gap", "class_count")) {
  if (!inherits(x, "bg_state_battery")) {
    stop("`x` must inherit from class 'bg_state_battery'.", call. = FALSE)
  }

  metric <- match.arg(metric)
  df <- x$state_table

  if (metric == "class_count") {
    counts <- as.data.frame(table(df$state_class), stringsAsFactors = FALSE)
    names(counts) <- c("state_class", "n")

    return(
      ggplot2::ggplot(counts, ggplot2::aes(x = stats::reorder(state_class, n), y = n, fill = state_class)) +
        ggplot2::geom_col(show.legend = FALSE) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = "Sampled state battery by class",
          subtitle = "The taxonomy is heuristic and intended for statistical stratification.",
          x = NULL,
          y = bg_metric_label(metric)
        ) +
        bg_plot_theme_research()
    )
  }

  if (!"top_two_gap_estimate" %in% names(df)) {
    stop("`x` does not contain attached truth diagnostics; rerun with `reference_budget`.", call. = FALSE)
  }

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = n_legal_moves, y = top_two_gap_estimate, color = state_class)
  ) +
    ggplot2::geom_point(size = 2.8, alpha = 0.9) +
    ggplot2::labs(
      title = "State difficulty across the sampled battery",
      subtitle = "Smaller top-two gaps indicate harder best-move identification under the rollout model.",
      x = "Number of legal moves",
      y = bg_metric_label(metric),
      color = "State class"
    ) +
    bg_plot_theme_research()
}

#' Plot proxy-truth gaps
#'
#' @param x A `bg_truth_state` or `bg_reference` object.
#' @param top_n Integer-like number of actions to show.
#'
#' @return A `ggplot` object.
#' @keywords internal
#' @noRd
plot_bg_gap <- function(x, top_n = 8L) {
  top_n <- bg_coerce_integerish(top_n, "top_n", 1L)
  ref <- bg_normalize_truth_reference(x, "x")
  df <- ref$action_table[order(ref$action_table$rank), , drop = FALSE]

  if (nrow(df) == 0L) {
    stop("`x` does not contain any action rows to plot.", call. = FALSE)
  }

  best_mean <- max(df$reference_mean, na.rm = TRUE)
  df$gap_to_best <- best_mean - df$reference_mean
  df$gap_lower_95 <- pmax(0, best_mean - df$reference_mc_upper_95)
  df$gap_upper_95 <- pmax(0, best_mean - df$reference_mc_lower_95)
  df <- utils::head(df, top_n)
  df$move_label <- factor(df$move_label, levels = rev(df$move_label))

  ggplot2::ggplot(df, ggplot2::aes(x = gap_to_best, y = move_label)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = gap_lower_95, xend = gap_upper_95, yend = move_label),
      linewidth = 1.1,
      color = "#a7b3c2"
    ) +
    ggplot2::geom_point(color = "#c9643b", size = 2.8) +
    ggplot2::labs(
      title = sprintf("Proxy-truth gaps for %s", ref$problem$problem_id),
      subtitle = "Smaller gaps indicate stronger contenders under the rollout-model reference.",
      x = "Gap to proxy-reference best move",
      y = NULL
    ) +
    bg_plot_theme_research()
}

#' Plot final-budget allocation across moves
#'
#' @param x A `bg_ts_run` object.
#' @param truth Optional truth object used instead of the embedded proxy
#'   reference.
#' @param checkpoint Optional checkpoint. Defaults to the final budget.
#' @param top_n Integer-like number of actions to display.
#'
#' @return A `ggplot` object.
#' @export
plot_bg_allocation <- function(x, truth = NULL, checkpoint = NULL, top_n = 8L) {
  if (!inherits(x, "bg_ts_run")) {
    stop("`x` must inherit from class 'bg_ts_run'.", call. = FALSE)
  }

  if (is.null(checkpoint)) {
    checkpoint <- x$budget
  }
  checkpoint <- bg_coerce_integerish(checkpoint, "checkpoint", 1L)
  top_n <- bg_coerce_integerish(top_n, "top_n", 1L)

  reference <- bg_eval_reference_for_run(x, truth = truth)
  tab <- bg_eval_action_table_at_checkpoint(x, checkpoint, reference = reference)
  if (nrow(tab) == 0L) {
    stop("`x` does not contain any checkpoint action rows to plot.", call. = FALSE)
  }

  if (!is.null(reference)) {
    tab <- tab[order(tab$proxy_reference_rank, -tab$allocation_count, tab$candidate_index), , drop = FALSE]
  } else {
    tab <- tab[order(-tab$allocation_count, -tab$estimate, tab$candidate_index), , drop = FALSE]
  }

  tab <- utils::head(tab, top_n)
  tab$move_label <- factor(tab$move_label, levels = rev(tab$move_label))
  tab$truth_top_move <- if (!is.null(reference)) tab$proxy_reference_rank == 1L else FALSE

  ggplot2::ggplot(
    tab,
    ggplot2::aes(x = allocation_count, y = move_label, fill = recommended)
  ) +
    ggplot2::geom_col(width = 0.72, color = "#222222", linewidth = 0.2) +
    ggplot2::geom_point(
      ggplot2::aes(x = allocation_count, shape = truth_top_move),
      size = 2.6,
      color = "#0b4f6c",
      show.legend = any(tab$truth_top_move)
    ) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = "#c9643b", `FALSE` = "#d9dfd0")) +
    ggplot2::scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1)) +
    ggplot2::labs(
      title = sprintf("Allocation profile at budget %d", checkpoint),
      subtitle = if (!is.null(reference)) {
        "Bars show finite-budget allocation; filled blue dots mark the proxy-reference best move."
      } else {
        "Bars show finite-budget allocation counts at the selected checkpoint."
      },
      x = "Allocation count",
      y = NULL,
      fill = "Recommended",
      shape = "Truth best"
    ) +
    bg_plot_theme_research()
}

bg_runtime_profile_long <- function(x) {
  data.frame(
    component = c(
      "legal_generation_seconds",
      "move_application_seconds",
      "one_rollout_seconds",
      "batched_rollout_seconds"
    ),
    runtime_seconds = c(
      x$legal_generation_seconds[[1L]],
      x$move_application_seconds[[1L]],
      x$one_rollout_seconds[[1L]],
      x$batched_rollout_seconds[[1L]]
    ),
    stringsAsFactors = FALSE
  )
}

bg_runtime_df_spec <- function(x, runtime_col = NULL) {
  if (inherits(x, "bg_runtime_profile")) {
    df <- bg_runtime_profile_long(x)
    return(list(
      data = df,
      x_col = "component",
      runtime_col = "runtime_seconds",
      color_col = NULL,
      discrete_x = TRUE,
      title = "Runtime profile",
      subtitle = "Component timings from a fixed runtime-profile call."
    ))
  }

  if (!is.data.frame(x)) {
    stop("`x` must be a `bg_runtime_profile` object or a data frame.", call. = FALSE)
  }

  runtime_col <- if (is.null(runtime_col)) {
    bg_first_present_column(x, c("elapsed_seconds", "runtime_seconds", "seconds"))
  } else {
    runtime_col
  }
  if (is.null(runtime_col) || !runtime_col %in% names(x)) {
    stop("Could not identify a runtime column in `x`.", call. = FALSE)
  }

  if ("n_cores" %in% names(x)) {
    return(list(
      data = x,
      x_col = "n_cores",
      runtime_col = runtime_col,
      color_col = bg_first_present_column(x, c("task", "method", "component")),
      discrete_x = FALSE,
      title = "Runtime scaling by cores",
      subtitle = "Lower curves indicate better throughput at fixed workload."
    ))
  }

  if ("budget" %in% names(x) || "total_budget" %in% names(x)) {
    x_col <- if ("budget" %in% names(x)) "budget" else "total_budget"
    return(list(
      data = x,
      x_col = x_col,
      runtime_col = runtime_col,
      color_col = bg_first_present_column(x, c("task", "method", "component")),
      discrete_x = FALSE,
      title = "Runtime scaling by budget",
      subtitle = "Use this to compare cost growth across experiments or kernels."
    ))
  }

  if ("component" %in% names(x)) {
    return(list(
      data = x,
      x_col = "component",
      runtime_col = runtime_col,
      color_col = NULL,
      discrete_x = TRUE,
      title = "Runtime profile",
      subtitle = "Component timings from a supplied runtime table."
    ))
  }

  stop(
    "For data-frame input, include either `n_cores`, `budget`/`total_budget`, ",
    "or `component`, plus a runtime column such as `elapsed_seconds`.",
    call. = FALSE
  )
}

#' Plot runtime scaling or component timings
#'
#' @param x A `bg_runtime_profile` object or a data frame with runtime columns.
#' @param runtime_col Optional runtime column name for data-frame input.
#'
#' @return A `ggplot` object.
#' @keywords internal
#' @noRd
plot_bg_runtime_scaling <- function(x, runtime_col = NULL) {
  spec <- bg_runtime_df_spec(x, runtime_col = runtime_col)
  df <- spec$data

  if (isTRUE(spec$discrete_x)) {
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = .data[[spec$x_col]], y = .data[[spec$runtime_col]])) +
        ggplot2::geom_col(fill = "#0b4f6c", width = 0.72) +
        ggplot2::labs(
          title = spec$title,
          subtitle = spec$subtitle,
          x = NULL,
          y = bg_metric_label(spec$runtime_col)
        ) +
        bg_plot_theme_research()
    )
  }

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data[[spec$x_col]], y = .data[[spec$runtime_col]])
  )

  if (!is.null(spec$color_col)) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(color = .data[[spec$color_col]]), linewidth = 1) +
      ggplot2::geom_point(ggplot2::aes(color = .data[[spec$color_col]]), size = 2.2) +
      ggplot2::labs(color = tools::toTitleCase(gsub("_", " ", spec$color_col)))
  } else {
    p <- p +
      ggplot2::geom_line(linewidth = 1, color = "#0b4f6c") +
      ggplot2::geom_point(size = 2.2, color = "#0b4f6c")
  }

  p +
    ggplot2::labs(
      title = spec$title,
      subtitle = spec$subtitle,
      x = tools::toTitleCase(gsub("_", " ", spec$x_col)),
      y = bg_metric_label(spec$runtime_col)
    ) +
    bg_plot_theme_research()
}

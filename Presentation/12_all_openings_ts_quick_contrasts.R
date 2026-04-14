# 12_all_openings_ts_quick_contrasts.R
#
# Purpose:
# - quickly walk through how canonical TS compares with the other Thompson
#   variants across all 21 opening rolls;
# - reuse the main all-openings win/loss study when available; and
# - surface the most readable TS-versus-variant contrasts.
#
# Main package functions used here:
# - bg_opening_compare_study()           [R/bg_truth.R] via saved study reuse
# - bg_truth_project()                   [R/bg_truth.R]
# - plot_bg_budget_curve()               [R/bg_plots.R]
#
# Relevant native files:
# - src/model_beta_bernoulli.cpp
# - src/alloc_core.cpp
# - src/policy_ts.cpp
# - src/policy_ttts.cpp

common_path <- if (file.exists("DESCRIPTION")) {
  file.path("Presentation", "presentation_common.R")
} else {
  "presentation_common.R"
}
source(common_path)

repo_root <- presentation_repo_root()
presentation_load_package(repo_root)
library(ggplot2)

study_path <- file.path(
  presentation_output_dir(repo_root, "studies"),
  "05_all_openings_ts_family_win_loss_master_2048.rds"
)

if (!file.exists(study_path)) {
  source(file.path(repo_root, "Presentation", "05_all_openings_ts_family.R"))
}

study <- readRDS(study_path)
truths <- presentation_project_truths(
  presentation_load_opening_truths(repo_root),
  stack = "beta_bernoulli"
)

ts_variants <- c(
  "top_two_thompson",
  "multi_sample_thompson",
  "soft_elimination_thompson",
  "forced_exploration_thompson",
  "top_k_thompson"
)
final_checkpoint <- max(study$opening_summary$checkpoint)
final_opening <- subset(study$opening_summary, checkpoint == final_checkpoint)
opening_lookup <- truths$summary[, c("opening_roll", "top_two_gap_estimate", "difficulty_label")]

contrast_rows <- lapply(
  ts_variants,
  function(method) {
    ts_rows <- final_opening[final_opening$allocation_policy == "thompson", , drop = FALSE]
    variant_rows <- final_opening[final_opening$allocation_policy == method, , drop = FALSE]
    merged <- merge(
      ts_rows[, c("opening_roll", "mean_top1_match", "mean_simple_regret", "mean_share_top2_truth", "mean_gap_weighted_wasted_allocation")],
      variant_rows[, c("opening_roll", "mean_top1_match", "mean_simple_regret", "mean_share_top2_truth", "mean_gap_weighted_wasted_allocation")],
      by = "opening_roll",
      suffixes = c("_ts", "_variant"),
      all = FALSE
    )

    merged$allocation_policy <- method
    merged$method <- presentation_method_label(method)
    merged$variant_minus_ts_top1 <- merged$mean_top1_match_variant - merged$mean_top1_match_ts
    merged$ts_minus_variant_regret <- merged$mean_simple_regret_variant - merged$mean_simple_regret_ts
    merged$variant_minus_ts_focus <- merged$mean_share_top2_truth_variant - merged$mean_share_top2_truth_ts
    merged$ts_minus_variant_waste <- merged$mean_gap_weighted_wasted_allocation_variant - merged$mean_gap_weighted_wasted_allocation_ts
    merged
  }
)

contrast_table <- do.call(rbind, contrast_rows)
contrast_table <- merge(contrast_table, opening_lookup, by = "opening_roll", all.x = TRUE, sort = FALSE)

summary_table <- aggregate(
  cbind(
    variant_minus_ts_top1,
    ts_minus_variant_regret,
    variant_minus_ts_focus,
    ts_minus_variant_waste
  ) ~ method + allocation_policy,
  data = contrast_table,
  FUN = mean
)
summary_table <- summary_table[order(-summary_table$variant_minus_ts_top1, -summary_table$ts_minus_variant_regret), , drop = FALSE]

path_table <- subset(
  study$opening_aggregate,
  allocation_policy %in% c("thompson", ts_variants) &
    metric %in% c("mean_top1_match", "mean_simple_regret", "mean_share_top2_truth")
)

metric_contrast_paths <- do.call(
  rbind,
  lapply(
    ts_variants,
    function(method) {
      ts_df <- path_table[path_table$allocation_policy == "thompson", , drop = FALSE]
      variant_df <- path_table[path_table$allocation_policy == method, , drop = FALSE]
      merged <- merge(
        ts_df[, c("checkpoint", "metric", "estimate")],
        variant_df[, c("checkpoint", "metric", "estimate")],
        by = c("checkpoint", "metric"),
        suffixes = c("_ts", "_variant"),
        all = FALSE
      )

      merged$allocation_policy <- method
      merged$method <- presentation_method_label(method)
      merged$value <- ifelse(
        merged$metric == "mean_simple_regret",
        merged$estimate_ts - merged$estimate_variant,
        merged$estimate_variant - merged$estimate_ts
      )
      merged
    }
  )
)

metric_contrast_paths$metric_label <- c(
  mean_top1_match = "Variant minus TS top-1 match",
  mean_simple_regret = "TS minus variant simple regret",
  mean_share_top2_truth = "Variant minus TS top-2 focus"
)[metric_contrast_paths$metric]

# This table is the quickest summary of whether the TS variants beat canonical
# TS on the main all-opening metrics.
summary_plot <- ggplot(
  reshape(
    summary_table,
    idvar = c("method", "allocation_policy"),
    varying = c(
      "variant_minus_ts_top1",
      "ts_minus_variant_regret",
      "variant_minus_ts_focus",
      "ts_minus_variant_waste"
    ),
    v.names = "value",
    timevar = "contrast_metric",
    times = c(
      "Variant minus TS top-1 match",
      "TS minus variant simple regret",
      "Variant minus TS top-2 focus",
      "TS minus variant wasted allocation"
    ),
    direction = "long"
  ),
  aes(x = reorder(method, value), y = value, fill = value > 0)
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ contrast_metric, scales = "free_x") +
  coord_flip() +
  scale_fill_manual(values = c(`TRUE` = "#D55E00", `FALSE` = "#0072B2")) +
  labs(
    title = "How the other Thompson variants compare with canonical TS",
    subtitle = paste("Positive values favor the variant under each panel's direction at budget", final_checkpoint),
    x = NULL,
    y = "Average contrast across openings"
  ) +
  bg_plot_theme_research()

# This path plot is the fast "when does the gap show up?" view.
path_plot <- ggplot(
  metric_contrast_paths,
  aes(x = checkpoint, y = value, color = method)
) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "#6C757D") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ metric_label, scales = "free_y") +
  scale_color_manual(values = presentation_method_palette(ts_variants)) +
  scale_x_continuous(trans = "log2", breaks = sort(unique(metric_contrast_paths$checkpoint))) +
  labs(
    title = "Variant-versus-TS contrasts across budget",
    subtitle = "Positive values mean the variant is beating canonical TS under the metric direction in that panel.",
    x = "Budget",
    y = "Contrast",
    color = "Variant"
  ) +
  bg_plot_theme_research()

# This heatmap is the per-opening view. It shows which openings actually drive
# the final pooled differences.
heatmap_plot <- ggplot(
  contrast_table,
  aes(
    x = method,
    y = factor(opening_roll, levels = unique(truths$summary$opening_roll)),
    fill = variant_minus_ts_top1
  )
) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_gradient2(low = "#0072B2", mid = "#F4F1EA", high = "#D55E00", midpoint = 0) +
  labs(
    title = "Variant minus TS on final top-1 match by opening",
    subtitle = "Positive cells mean the variant beats canonical TS on that opening at the final budget.",
    x = NULL,
    y = "Opening roll",
    fill = "Variant - TS\nTop-1"
  ) +
  bg_plot_theme_research()

hardness_plot <- ggplot(
  contrast_table,
  aes(x = top_two_gap_estimate, y = variant_minus_ts_top1, color = method)
) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "#6C757D") +
  geom_point(size = 2.3, alpha = 0.9) +
  geom_smooth(se = FALSE, linewidth = 0.8, method = "lm") +
  scale_color_manual(values = presentation_method_palette(ts_variants)) +
  labs(
    title = "When do the variants beat canonical TS?",
    subtitle = "Positive values mean the variant improves top-1 match relative to TS.",
    x = "Truth top-two gap",
    y = "Variant minus TS top-1 match",
    color = "Variant"
  ) +
  bg_plot_theme_research()

presentation_save_table(summary_table, repo_root, "12_all_openings_ts_quick_contrasts_summary")
presentation_save_table(contrast_table, repo_root, "12_all_openings_ts_quick_contrasts_openings")
presentation_save_plot(summary_plot, repo_root, "12_all_openings_ts_quick_contrasts_summary", width = 12, height = 8)
presentation_save_plot(path_plot, repo_root, "12_all_openings_ts_quick_contrasts_paths", width = 12, height = 8)
presentation_save_plot(heatmap_plot, repo_root, "12_all_openings_ts_quick_contrasts_heatmap", width = 10, height = 8)
presentation_save_plot(hardness_plot, repo_root, "12_all_openings_ts_quick_contrasts_hardness", width = 10, height = 6)

print(summary_table)
print(head(contrast_table, 20L))
print(summary_plot)
print(path_plot)
print(heatmap_plot)
print(hardness_plot)

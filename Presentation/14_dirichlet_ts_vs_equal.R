# 14_dirichlet_ts_vs_equal.R
#
# Purpose:
# - compare baseline Thompson sampling against equal allocation under the
#   categorical-outcome + Dirichlet stack;
# - use the preserved master truths directly in their categorical form; and
# - keep the story focused on one stack at a time.
#
# Main package functions used here:
# - bg_truth_project()                   [R/bg_truth.R]
# - bg_opening_compare_study()           [R/bg_truth.R]
#
# Relevant native files:
# - src/model_dirichlet_categorical.cpp
# - src/alloc_core.cpp
# - src/policy_ts.cpp
# - src/policy_equal.cpp

common_path <- if (file.exists("DESCRIPTION")) {
  file.path("Presentation", "presentation_common.R")
} else {
  "presentation_common.R"
}
source(common_path)

repo_root <- presentation_repo_root()
presentation_load_package(repo_root)
library(ggplot2)

stack_id <- "dirichlet"
stack_spec <- presentation_stack_spec(stack_id)
methods <- c("thompson", "equal")
budgets <- presentation_checkpoint_grid(presentation_run_budget())
seeds <- 1L
bootstrap_reps <- 200L

workers <- presentation_detect_workers(max_cores = 4L)
master_truths <- presentation_load_opening_truths(repo_root)
master_reference_budget <- presentation_master_reference_budget(master_truths)
truths <- presentation_project_truths(master_truths, stack = stack_id)

study <- bg_opening_compare_study(
  proxy_truths = truths,
  methods = methods,
  budgets = budgets,
  seeds = seeds,
  n_cores = workers$n_cores,
  parallel = workers$parallel,
  progress = TRUE,
  bootstrap_reps = bootstrap_reps,
  save_path = file.path(
    presentation_output_dir(repo_root, "studies"),
    "14_dirichlet_ts_vs_equal_master_2048.rds"
  ),
  overwrite = TRUE,
  seed = 1L
)

final_checkpoint <- max(study$opening_summary$checkpoint)
final_opening <- subset(study$opening_summary, checkpoint == final_checkpoint)
final_aggregate <- subset(study$opening_aggregate, checkpoint == final_checkpoint)

final_table <- Reduce(
  function(left, right) merge(left, right, by = "allocation_policy", all = TRUE, sort = FALSE),
  list(
    aggregate(
      cbind(mean_selected_reference_rank, mean_runtime_seconds) ~ allocation_policy,
      data = final_opening,
      FUN = mean
    ),
    reshape(
      final_aggregate[
        final_aggregate$metric %in% c(
          "mean_top1_match",
          "mean_simple_regret",
          "mean_truth_top2_hit",
          "mean_share_top2_truth",
          "mean_gap_weighted_wasted_allocation",
          "high_confidence_wrong_rate"
        ),
        c("allocation_policy", "metric", "estimate")
      ],
      idvar = "allocation_policy",
      timevar = "metric",
      direction = "wide"
    )
  )
)

names(final_table) <- sub("^estimate\\.", "", names(final_table))
final_table$method <- presentation_method_label(final_table$allocation_policy)
final_table <- final_table[
  order(-final_table$mean_top1_match, final_table$mean_simple_regret),
  c(
    "method",
    "allocation_policy",
    "mean_top1_match",
    "mean_simple_regret",
    "mean_selected_reference_rank",
    "mean_truth_top2_hit",
    "mean_share_top2_truth",
    "mean_gap_weighted_wasted_allocation",
    "high_confidence_wrong_rate",
    "mean_runtime_seconds"
  )
]

curve_data <- study$opening_aggregate
curve_data$method <- factor(
  presentation_method_label(curve_data$allocation_policy),
  levels = presentation_method_label(methods)
)
method_palette <- presentation_method_palette(methods)

# This is the headline performance curve for the Dirichlet stack.
plot_top1 <- ggplot(
  subset(curve_data, metric == "mean_top1_match"),
  aes(x = checkpoint, y = estimate, color = method, ymin = lower_95, ymax = upper_95)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(fill = method), alpha = 0.12, linewidth = 0, color = NA) +
  scale_color_manual(values = method_palette) +
  scale_fill_manual(values = method_palette) +
  scale_x_continuous(trans = "log2", breaks = budgets) +
  labs(
    title = "Dirichlet stack: TS versus equal",
    subtitle = paste(
      "Both methods are evaluated against the same preserved master truths at reference budget",
      format(master_reference_budget, big.mark = ","),
      "."
    ),
    x = "Budget",
    y = "Mean top-1 match",
    color = "Method",
    fill = "Method"
  ) +
  bg_plot_theme_research()

plot_regret <- ggplot(
  subset(curve_data, metric == "mean_simple_regret"),
  aes(x = checkpoint, y = estimate, color = method, ymin = lower_95, ymax = upper_95)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(fill = method), alpha = 0.12, linewidth = 0, color = NA) +
  scale_color_manual(values = method_palette) +
  scale_fill_manual(values = method_palette) +
  scale_x_continuous(trans = "log2", breaks = budgets) +
  labs(
    title = "Dirichlet stack: simple regret",
    subtitle = "Lower is better.",
    x = "Budget",
    y = "Mean simple regret",
    color = "Method",
    fill = "Method"
  ) +
  bg_plot_theme_research()

plot_focus <- ggplot(
  subset(curve_data, metric == "mean_share_top2_truth"),
  aes(x = checkpoint, y = estimate, color = method, ymin = lower_95, ymax = upper_95)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(fill = method), alpha = 0.12, linewidth = 0, color = NA) +
  scale_color_manual(values = method_palette) +
  scale_fill_manual(values = method_palette) +
  scale_x_continuous(trans = "log2", breaks = budgets) +
  labs(
    title = "Dirichlet stack: budget share on the truth top-2",
    subtitle = "Higher is better.",
    x = "Budget",
    y = "Mean share on truth top-2",
    color = "Method",
    fill = "Method"
  ) +
  bg_plot_theme_research()

plot_conf_wrong <- ggplot(
  subset(curve_data, metric == "high_confidence_wrong_rate"),
  aes(x = checkpoint, y = estimate, color = method, ymin = lower_95, ymax = upper_95)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(fill = method), alpha = 0.12, linewidth = 0, color = NA) +
  scale_color_manual(values = method_palette) +
  scale_fill_manual(values = method_palette) +
  scale_x_continuous(trans = "log2", breaks = budgets) +
  labs(
    title = "Dirichlet stack: high-confidence wrong rate",
    subtitle = "Lower is better.",
    x = "Budget",
    y = "High-confidence wrong rate",
    color = "Method",
    fill = "Method"
  ) +
  bg_plot_theme_research()

opening_contrast <- merge(
  final_opening[final_opening$allocation_policy == "thompson", c("opening_roll", "mean_top1_match", "mean_simple_regret")],
  final_opening[final_opening$allocation_policy == "equal", c("opening_roll", "mean_top1_match", "mean_simple_regret")],
  by = "opening_roll",
  suffixes = c("_ts", "_equal"),
  all = FALSE
)
opening_contrast <- merge(
  opening_contrast,
  truths$summary[, c("opening_roll", "top_two_gap_estimate", "difficulty_label")],
  by = "opening_roll",
  all.x = TRUE,
  sort = FALSE
)
opening_contrast$ts_minus_equal_top1 <- opening_contrast$mean_top1_match_ts - opening_contrast$mean_top1_match_equal
opening_contrast$equal_minus_ts_regret <- opening_contrast$mean_simple_regret_equal - opening_contrast$mean_simple_regret_ts

# This heatmap shows exactly which openings drive the Dirichlet head-to-head.
heatmap_plot <- ggplot(
  opening_contrast,
  aes(x = "Dirichlet", y = factor(opening_roll, levels = unique(truths$summary$opening_roll)), fill = ts_minus_equal_top1)
) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_gradient2(low = "#0072B2", mid = "#F4F1EA", high = "#D55E00", midpoint = 0) +
  labs(
    title = "Dirichlet stack: TS minus equal by opening",
    subtitle = "Positive cells mean TS beats equal on final top-1 match.",
    x = NULL,
    y = "Opening roll",
    fill = "TS - Equal\nTop-1"
  ) +
  bg_plot_theme_research()

hardness_plot <- ggplot(
  opening_contrast,
  aes(x = top_two_gap_estimate, y = ts_minus_equal_top1, color = difficulty_label)
) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "#6C757D") +
  geom_point(size = 2.3, alpha = 0.9) +
  geom_smooth(se = FALSE, linewidth = 0.8, method = "lm") +
  labs(
    title = "Dirichlet stack: where TS gains appear",
    subtitle = "Positive values mean TS improves top-1 match over equal.",
    x = "Truth top-two gap",
    y = "TS minus equal top-1 match",
    color = "Difficulty"
  ) +
  bg_plot_theme_research()

presentation_save_table(final_table, repo_root, "14_dirichlet_ts_vs_equal_final_table")
presentation_save_table(opening_contrast, repo_root, "14_dirichlet_ts_vs_equal_opening_contrast")
presentation_save_plot(plot_top1, repo_root, "14_dirichlet_ts_vs_equal_top1", width = 10, height = 6)
presentation_save_plot(plot_regret, repo_root, "14_dirichlet_ts_vs_equal_regret", width = 10, height = 6)
presentation_save_plot(plot_focus, repo_root, "14_dirichlet_ts_vs_equal_focus", width = 10, height = 6)
presentation_save_plot(plot_conf_wrong, repo_root, "14_dirichlet_ts_vs_equal_conf_wrong", width = 10, height = 6)
presentation_save_plot(heatmap_plot, repo_root, "14_dirichlet_ts_vs_equal_heatmap", width = 8, height = 8)
presentation_save_plot(hardness_plot, repo_root, "14_dirichlet_ts_vs_equal_hardness", width = 10, height = 6)

print(final_table)
print(head(opening_contrast, 20L))
print(plot_top1)
print(plot_regret)
print(plot_focus)
print(plot_conf_wrong)
print(heatmap_plot)
print(hardness_plot)

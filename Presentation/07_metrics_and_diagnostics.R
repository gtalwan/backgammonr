# 07_metrics_and_diagnostics.R
#
# Purpose:
# - explain the primary metrics used throughout the presentation;
# - show compact examples of how to read them; and
# - separate headline metrics from supporting diagnostics.
#
# Main package functions used here:
# - bg_opening_truth_load_one()          [R/bg_truth.R]
# - bg_truth_project()                   [R/bg_truth.R]
# - bg_ts_run()                          [R/bg_algorithms.R]
# - bg_ttts_run()                        [R/bg_algorithms.R]
# - bg_equal_run()                       [R/bg_algorithms.R]
# - bg_eval_reference_aware()            [R/bg_metrics.R]
# - bg_ts_diagnostics()                  [R/bg_metrics.R]
# - plot_bg_truth()                      [R/bg_plots.R]
#
# Relevant native files:
# - src/model_beta_bernoulli.cpp
# - src/metrics_summary.cpp

common_path <- if (file.exists("DESCRIPTION")) {
  file.path("Presentation", "presentation_common.R")
} else {
  "presentation_common.R"
}
source(common_path)

repo_root <- presentation_repo_root()
presentation_load_package(repo_root)
library(ggplot2)

roll_label <- "1-6"
methods <- c("thompson", "top_two_thompson", "equal")
budget <- presentation_run_budget()
checkpoints <- presentation_checkpoint_grid(budget)
seeds <- 1L

truth <- presentation_project_truths(
  presentation_load_opening_truth(repo_root, roll_label),
  stack = "beta_bernoulli"
)

study <- presentation_run_panel(
  problem = truth$problem,
  truth = truth,
  methods = methods,
  seeds = seeds,
  budget = budget,
  checkpoints = checkpoints
)
diag_ts <- bg_ts_diagnostics(study$runs[["thompson::1"]], truth = truth)
final_diag_checkpoint <- max(checkpoints)

metric_glossary <- data.frame(
  metric = c(
    "top1_match",
    "simple_regret",
    "selected_reference_rank",
    "truth_top2_hit",
    "share_top2_truth",
    "share_mc_screened_suboptimal",
    "gap_weighted_wasted_allocation",
    "high_confidence_wrong_rate",
    "recommendation_instability",
    "top_two_gap_estimate"
  ),
  interpretation = c(
    "Did the method choose the truth-best move?",
    "How much value was lost relative to the truth-best move?",
    "What truth rank did the selected move have?",
    "Did the method at least land inside the truth top-2?",
    "How much of the budget went to the truly competitive moves?",
    "How much budget was spent on moves the Monte Carlo truth already screens out?",
    "How much budget was wasted, weighted by how bad those moves really are?",
    "How often was the method confidently wrong?",
    "How much do different seeds disagree on the recommendation?",
    "How hard is the opening itself?"
  ),
  direction = c(
    "Higher is better",
    "Lower is better",
    "Lower is better",
    "Higher is better",
    "Higher is better",
    "Lower is better",
    "Lower is better",
    "Lower is better",
    "Lower is better",
    "Smaller means harder"
  ),
  main_or_supporting = c(
    "Primary",
    "Primary",
    "Primary",
    "Primary",
    "Primary",
    "Supporting",
    "Supporting",
    "Supporting",
    "Supporting",
    "Context"
  ),
  stringsAsFactors = FALSE
)

curve_panel <- study$checkpoint_summary
curve_panel$method <- factor(
  presentation_method_label(curve_panel$allocation_policy),
  levels = presentation_method_label(methods)
)

# This faceted plot is the core "how to read the metrics" visual for the deck.
curve_long <- rbind(
  data.frame(method = curve_panel$method, checkpoint = curve_panel$checkpoint, metric = "Top-1 match", value = curve_panel$top1_match),
  data.frame(method = curve_panel$method, checkpoint = curve_panel$checkpoint, metric = "Simple regret", value = curve_panel$simple_regret),
  data.frame(method = curve_panel$method, checkpoint = curve_panel$checkpoint, metric = "Selected truth rank", value = curve_panel$selected_reference_rank),
  data.frame(method = curve_panel$method, checkpoint = curve_panel$checkpoint, metric = "Share on truth top-2", value = curve_panel$share_top2_truth)
)

curve_plot <- ggplot(curve_long, aes(x = checkpoint, y = value, color = method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ metric, scales = "free_y") +
  scale_color_manual(values = presentation_method_palette(methods)) +
  scale_x_continuous(trans = "log2", breaks = checkpoints) +
  labs(
    title = "How to read the main path metrics",
    subtitle = "The same direct method paths can be read as correctness, regret, rank quality, or budget focus.",
    x = "Budget",
    y = NULL,
    color = "Method"
  ) +
  bg_plot_theme_research()

diag_allocation_final <- subset(diag_ts$allocation, checkpoint == final_diag_checkpoint)
diag_accuracy_final <- subset(diag_ts$accuracy, checkpoint == final_diag_checkpoint)
diag_efficiency <- diag_ts$efficiency

diag_table <- rbind(
  data.frame(
    section = "Allocation diagnostics",
    metric = c(
      "n_allocated_actions",
      "allocation_entropy",
      "allocation_max_share",
      "share_top2_truth",
      "share_mc_screened_suboptimal",
      "gap_weighted_wasted_allocation"
    ),
    value = c(
      diag_allocation_final$n_allocated_actions,
      diag_allocation_final$allocation_entropy,
      diag_allocation_final$allocation_max_share,
      diag_allocation_final$share_top2_truth,
      diag_allocation_final$share_mc_screened_suboptimal,
      diag_allocation_final$gap_weighted_wasted_allocation
    )
  ),
  data.frame(
    section = "Accuracy diagnostics",
    metric = c(
      "top1_match",
      "simple_regret",
      "selected_reference_rank",
      "truth_top2_hit",
      "spearman",
      "restricted_pairwise_ordering_accuracy"
    ),
    value = c(
      diag_accuracy_final$top1_match,
      diag_accuracy_final$simple_regret,
      diag_accuracy_final$selected_reference_rank,
      diag_accuracy_final$truth_top2_hit,
      diag_accuracy_final$spearman,
      diag_accuracy_final$restricted_pairwise_ordering_accuracy
    )
  ),
  data.frame(
    section = "Efficiency diagnostics",
    metric = c(
      "first_budget_top1_match",
      "auc_top1_match",
      "auc_simple_regret",
      "mean_brier_top1",
      "final_top1_match",
      "final_simple_regret"
    ),
    value = c(
      diag_efficiency$first_budget_top1_match,
      diag_efficiency$auc_top1_match,
      diag_efficiency$auc_simple_regret,
      diag_efficiency$mean_brier_top1,
      diag_efficiency$final_top1_match,
      diag_efficiency$final_simple_regret
    )
  )
)

diag_plot <- ggplot(diag_table, aes(x = reorder(metric, value), y = value, fill = section)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ section, scales = "free_y") +
  labs(
    title = "TS diagnostic components for one run",
    subtitle = "Diagnostics are secondary; they explain why a run looks good or bad.",
    x = NULL,
    y = "Value"
  ) +
  bg_plot_theme_research()

# These supporting metrics explain why two methods with similar top-1 match can
# still be meaningfully different in how they spend the budget.
support_long <- rbind(
  data.frame(method = curve_panel$method, checkpoint = curve_panel$checkpoint, metric = "Share MC-screened suboptimal", value = curve_panel$share_mc_screened_suboptimal),
  data.frame(method = curve_panel$method, checkpoint = curve_panel$checkpoint, metric = "Gap-weighted wasted allocation", value = curve_panel$gap_weighted_wasted_allocation)
)

support_plot <- ggplot(support_long, aes(x = checkpoint, y = value, color = method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ metric, scales = "free_y") +
  scale_color_manual(values = presentation_method_palette(methods)) +
  scale_x_continuous(trans = "log2", breaks = checkpoints) +
  labs(
    title = "Supporting metrics explain why a method looks good or bad",
    subtitle = "These are not the headline metrics, but they help interpret allocation quality.",
    x = "Budget",
    y = NULL,
    color = "Method"
  ) +
  bg_plot_theme_research()

# This truth plot belongs here because the hardest metrics only make sense in
# the context of small truth gaps.
truth_plot <- plot_bg_truth(
  presentation_load_opening_truths(repo_root)
) +
  labs(
    title = "Truth gap context for the primary metrics",
    subtitle = "Small top-two gaps are exactly the settings where top-1 and regret become hard to stabilize."
  )

presentation_save_table(metric_glossary, repo_root, "07_metric_glossary")
presentation_save_table(diag_table, repo_root, "07_one_run_diagnostics_table")
presentation_save_plot(curve_plot, repo_root, "07_metric_path_examples", width = 12, height = 8)
presentation_save_plot(diag_plot, repo_root, "07_diagnostic_components", width = 12, height = 7)
presentation_save_plot(support_plot, repo_root, "07_supporting_metric_paths", width = 12, height = 7)
presentation_save_plot(truth_plot, repo_root, "07_truth_gap_context", width = 10, height = 7)

# This glossary is the best compact reference to keep next to the rest of the
# presentation scripts while deciding which metrics to emphasize.
print(metric_glossary)

# This one-run diagnostics table is intentionally compact and presentation-safe.
print(diag_table)
print(curve_plot)
print(diag_plot)
print(support_plot)
print(truth_plot)

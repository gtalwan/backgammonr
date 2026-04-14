# 04_one_opening_ts_vs_equal.R
#
# Purpose:
# - give one clean TS-versus-equal head-to-head example;
# - focus on the most interpretable metrics; and
# - make the allocation difference visible at the same budget.
#
# Main package functions used here:
# - bg_opening_truth_load_one()          [R/bg_truth.R]
# - bg_truth_project()                   [R/bg_truth.R]
# - bg_ts_run()                          [R/bg_algorithms.R]
# - bg_equal_run()                       [R/bg_algorithms.R]
# - bg_eval_reference_aware()            [R/bg_metrics.R]
# - plot_bg_allocation()                 [R/bg_plots.R]
#
# Relevant native files:
# - src/alloc_core.cpp
# - src/policy_ts.cpp
# - src/policy_equal.cpp
# - src/model_beta_bernoulli.cpp

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
methods <- c("thompson", "equal")
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

saveRDS(study, file = file.path(presentation_output_dir(repo_root, "studies"), "04_one_opening_ts_vs_equal_1_6.rds"))
presentation_save_table(study$panel, repo_root, "04_one_opening_ts_vs_equal_panel_1_6")
presentation_save_table(study$final_summary, repo_root, "04_one_opening_ts_vs_equal_final_1_6")

curve_data <- study$checkpoint_summary
curve_data$method <- factor(
  presentation_method_label(curve_data$allocation_policy),
  levels = presentation_method_label(methods)
)
method_palette <- presentation_method_palette(methods)

# Top-1 match is the headline "who gets the best move?" contrast.
plot_top1 <- ggplot(curve_data, aes(x = checkpoint, y = top1_match, color = method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = method_palette) +
  scale_x_continuous(trans = "log2", breaks = checkpoints) +
  labs(
    title = paste("TS versus equal on", roll_label),
    subtitle = "Direct checkpoint paths for one TS run and one equal-allocation run.",
    x = "Budget",
    y = "Top-1 match",
    color = "Method"
  ) +
  bg_plot_theme_research()

# Regret is often easier to read when both methods get close to the same move.
plot_regret <- ggplot(curve_data, aes(x = checkpoint, y = simple_regret, color = method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = method_palette) +
  scale_x_continuous(trans = "log2", breaks = checkpoints) +
  labs(
    title = paste("TS versus equal regret on", roll_label),
    subtitle = "Lower is better.",
    x = "Budget",
    y = "Simple regret",
    color = "Method"
  ) +
  bg_plot_theme_research()

# Focus on the truth top-2 is the clearest mechanism plot in this head-to-head.
plot_focus <- ggplot(curve_data, aes(x = checkpoint, y = share_top2_truth, color = method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = method_palette) +
  scale_x_continuous(trans = "log2", breaks = checkpoints) +
  labs(
    title = paste("TS versus equal budget focus on", roll_label),
    subtitle = "Higher means more rollout effort on the truth top-2 moves.",
    x = "Budget",
    y = "Share of budget on the truth top-2",
    color = "Method"
  ) +
  bg_plot_theme_research()

# Wasted allocation flips the focus plot around and asks how much budget is
# still leaking onto moves that should have been screened out.
plot_waste <- ggplot(curve_data, aes(x = checkpoint, y = gap_weighted_wasted_allocation, color = method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = method_palette) +
  scale_x_continuous(trans = "log2", breaks = checkpoints) +
  labs(
    title = paste("TS versus equal wasted allocation on", roll_label),
    subtitle = "Lower means less budget spent on clearly inferior moves.",
    x = "Budget",
    y = "Gap-weighted wasted allocation",
    color = "Method"
  ) +
  bg_plot_theme_research()

plot_rank <- ggplot(curve_data, aes(x = checkpoint, y = selected_reference_rank, color = method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = method_palette) +
  scale_x_continuous(trans = "log2", breaks = checkpoints) +
  labs(
    title = paste("TS versus equal selected truth rank on", roll_label),
    subtitle = "Lower is better; rank 1 means the method selected the truth-best move.",
    x = "Budget",
    y = "Selected truth rank",
    color = "Method"
  ) +
  bg_plot_theme_research()

plot_probbest <- ggplot(curve_data, aes(x = checkpoint, y = recommended_prob_best, color = method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = method_palette) +
  scale_x_continuous(trans = "log2", breaks = checkpoints) +
  labs(
    title = paste("TS versus equal posterior confidence on", roll_label),
    subtitle = "This is a within-run confidence measure, not a cross-seed success rate.",
    x = "Budget",
    y = "Recommended probability-best",
    color = "Method"
  ) +
  bg_plot_theme_research()

seed1_ts <- study$runs[["thompson::1"]]
seed1_equal <- study$runs[["equal::1"]]

# These final allocation plots are the most visual summary of the head-to-head.
allocation_ts <- plot_bg_allocation(seed1_ts, truth = truth, checkpoint = budget, top_n = 8L) +
  labs(title = paste("TS allocation on", roll_label, "(single direct path)"))
allocation_equal <- plot_bg_allocation(seed1_equal, truth = truth, checkpoint = budget, top_n = 8L) +
  labs(title = paste("Equal allocation on", roll_label, "(single direct path)"))

contrast <- reshape(
  curve_data[, c("checkpoint", "allocation_policy", "top1_match", "simple_regret", "share_top2_truth", "gap_weighted_wasted_allocation")],
  idvar = "checkpoint",
  timevar = "allocation_policy",
  direction = "wide"
)
contrast$ts_minus_equal_top1 <- contrast$top1_match.thompson - contrast$top1_match.equal
contrast$equal_minus_ts_regret <- contrast$simple_regret.equal - contrast$simple_regret.thompson
contrast$ts_minus_equal_focus <- contrast$share_top2_truth.thompson - contrast$share_top2_truth.equal

contrast_long <- rbind(
  data.frame(checkpoint = contrast$checkpoint, metric = "TS minus equal top-1 match", value = contrast$ts_minus_equal_top1),
  data.frame(checkpoint = contrast$checkpoint, metric = "Equal minus TS regret", value = contrast$equal_minus_ts_regret),
  data.frame(checkpoint = contrast$checkpoint, metric = "TS minus equal top-2 focus", value = contrast$ts_minus_equal_focus)
)

contrast_plot <- ggplot(contrast_long, aes(x = checkpoint, y = value)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "#6C757D") +
  geom_line(linewidth = 1, color = "#D55E00") +
  geom_point(size = 2, color = "#D55E00") +
  facet_wrap(~ metric, scales = "free_y") +
  scale_x_continuous(trans = "log2", breaks = checkpoints) +
  labs(
    title = paste("Direct TS-minus-equal contrasts on", roll_label),
    subtitle = "Positive values favor TS in each panel's direction.",
    x = "Budget",
    y = NULL
  ) +
  bg_plot_theme_research()

final_contrast <- contrast[contrast$checkpoint == max(contrast$checkpoint), , drop = FALSE]
summary_table <- data.frame(
  opening_roll = roll_label,
  run_budget = budget,
  master_reference_budget = truth$reference$summary$reference_budget[[1L]],
  ts_final_move = seed1_ts$recommended_move_label,
  equal_final_move = seed1_equal$recommended_move_label,
  truth_best_move = truth$reference$summary$proxy_reference_best_move_label[[1L]],
  final_ts_minus_equal_top1 = final_contrast$ts_minus_equal_top1,
  final_equal_minus_ts_regret = final_contrast$equal_minus_ts_regret,
  final_ts_minus_equal_focus = final_contrast$ts_minus_equal_focus,
  stringsAsFactors = FALSE
)

presentation_save_table(contrast, repo_root, "04_one_opening_ts_vs_equal_contrast_1_6")
presentation_save_table(summary_table, repo_root, "04_one_opening_ts_vs_equal_summary_1_6")
presentation_save_plot(plot_top1, repo_root, "04_ts_vs_equal_top1_1_6", width = 9, height = 6)
presentation_save_plot(plot_regret, repo_root, "04_ts_vs_equal_regret_1_6", width = 9, height = 6)
presentation_save_plot(plot_focus, repo_root, "04_ts_vs_equal_focus_1_6", width = 9, height = 6)
presentation_save_plot(plot_waste, repo_root, "04_ts_vs_equal_waste_1_6", width = 9, height = 6)
presentation_save_plot(plot_rank, repo_root, "04_ts_vs_equal_rank_1_6", width = 9, height = 6)
presentation_save_plot(plot_probbest, repo_root, "04_ts_vs_equal_prob_best_1_6", width = 9, height = 6)
presentation_save_plot(contrast_plot, repo_root, "04_ts_vs_equal_contrast_paths_1_6", width = 12, height = 8)
presentation_save_plot(allocation_ts, repo_root, "04_ts_allocation_seed1_1_6", width = 9, height = 6)
presentation_save_plot(allocation_equal, repo_root, "04_equal_allocation_seed1_1_6", width = 9, height = 6)

# This is the one-row head-to-head summary for slides.
print(summary_table)

# This table keeps the direct method comparison visible at the final budget.
print(study$final_summary)

# These checkpoint contrasts make the TS-minus-equal story explicit.
print(contrast)
print(plot_top1)
print(plot_regret)
print(plot_focus)
print(plot_waste)
print(plot_rank)
print(plot_probbest)
print(contrast_plot)
print(allocation_ts)
print(allocation_equal)

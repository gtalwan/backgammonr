# 08_plot_gallery.R
#
# Purpose:
# - create a small final gallery of presentation-ready plots;
# - reuse saved study outputs when available; and
# - export the figures into Presentation/output/plots.
#
# Main package functions used here:
# - bg_opening_truth_load_all()          [R/bg_truth.R]
# - plot_bg_truth()                      [R/bg_plots.R]
# - bg_opening_compare_study()           [R/bg_truth.R] via saved studies
#
# Relevant native files:
# - src/truth_proxy.cpp
# - src/model_beta_bernoulli.cpp
# - src/model_student_t.cpp
# - src/model_dirichlet_categorical.cpp

common_path <- if (file.exists("DESCRIPTION")) {
  file.path("Presentation", "presentation_common.R")
} else {
  "presentation_common.R"
}
source(common_path)

repo_root <- presentation_repo_root()
presentation_load_package(repo_root)
library(ggplot2)

study_05_path <- file.path(
  presentation_output_dir(repo_root, "studies"),
  "05_all_openings_ts_family_win_loss_master_2048.rds"
)
study_06_path <- file.path(
  presentation_output_dir(repo_root, "studies"),
  "06_model_sensitivity_summary_master_2048.rds"
)

if (!file.exists(study_05_path)) {
  source(file.path(repo_root, "Presentation", "05_all_openings_ts_family.R"))
}
if (!file.exists(study_06_path)) {
  source(file.path(repo_root, "Presentation", "06_model_sensitivity_baseline_ts.R"))
}

truths <- presentation_load_opening_truths(repo_root)
study_05 <- readRDS(study_05_path)
study_06 <- readRDS(study_06_path)

gap_plot <- plot_bg_truth(truths) +
  labs(
    title = "Opening truth gap ladder",
    subtitle = "The package's 21-opening battery, ordered from hardest to easiest by top-two gap."
  )

# This is the main pooled performance figure for the opening battery.
agg_top1 <- subset(study_05$opening_aggregate, metric == "mean_top1_match")
agg_top1$method <- factor(
  presentation_method_label(agg_top1$allocation_policy),
  levels = presentation_method_label(unique(agg_top1$allocation_policy))
)
top1_plot <- ggplot(agg_top1, aes(x = checkpoint, y = estimate, color = method, ymin = lower_95, ymax = upper_95)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(fill = method), alpha = 0.12, linewidth = 0, color = NA) +
  scale_color_manual(values = presentation_method_palette(unique(agg_top1$allocation_policy))) +
  scale_fill_manual(values = presentation_method_palette(unique(agg_top1$allocation_policy))) +
  scale_x_continuous(trans = "log2", breaks = sort(unique(agg_top1$checkpoint))) +
  labs(
    title = "All openings: top-1 match",
    subtitle = "Main headline result across the TS family and equal allocation.",
    x = "Budget",
    y = "Mean top-1 match",
    color = "Method",
    fill = "Method"
  ) +
  bg_plot_theme_research()

# This focus curve is the most direct "why do these methods differ?" plot.
agg_focus <- subset(study_05$opening_aggregate, metric == "mean_share_top2_truth")
agg_focus$method <- factor(
  presentation_method_label(agg_focus$allocation_policy),
  levels = presentation_method_label(unique(agg_focus$allocation_policy))
)
focus_plot <- ggplot(agg_focus, aes(x = checkpoint, y = estimate, color = method, ymin = lower_95, ymax = upper_95)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(fill = method), alpha = 0.12, linewidth = 0, color = NA) +
  scale_color_manual(values = presentation_method_palette(unique(agg_focus$allocation_policy))) +
  scale_fill_manual(values = presentation_method_palette(unique(agg_focus$allocation_policy))) +
  scale_x_continuous(trans = "log2", breaks = sort(unique(agg_focus$checkpoint))) +
  labs(
    title = "All openings: budget share on the truth top-2",
    subtitle = "This is the cleanest explanation for why some methods outperform equal allocation.",
    x = "Budget",
    y = "Mean share on truth top-2",
    color = "Method",
    fill = "Method"
  ) +
  bg_plot_theme_research()

final_opening <- subset(study_05$opening_summary, checkpoint == max(study_05$opening_summary$checkpoint))
contrast_ts_ttts <- merge(
  final_opening[final_opening$allocation_policy == "thompson", c("opening_roll", "mean_top1_match")],
  final_opening[final_opening$allocation_policy == "top_two_thompson", c("opening_roll", "mean_top1_match")],
  by = "opening_roll",
  suffixes = c("_ts", "_ttts"),
  all = FALSE
)
contrast_ts_ttts$ttts_minus_ts <- contrast_ts_ttts$mean_top1_match_ttts - contrast_ts_ttts$mean_top1_match_ts
contrast_ts_ttts <- contrast_ts_ttts[order(contrast_ts_ttts$ttts_minus_ts), , drop = FALSE]

contrast_plot <- ggplot(
  contrast_ts_ttts,
  aes(x = reorder(opening_roll, ttts_minus_ts), y = ttts_minus_ts, fill = ttts_minus_ts > 0)
) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = c(`TRUE` = "#D55E00", `FALSE` = "#0072B2")) +
  labs(
    title = "TTTS minus TS on final top-1 match by opening",
    subtitle = "Positive values mean TTTS beats canonical TS on that opening.",
    x = NULL,
    y = "TTTS minus TS"
  ) +
  bg_plot_theme_research()

# This heatmap is the most compact model-sensitivity figure in the gallery.
stack_modal <- study_06$modal_recommendations
stack_heatmap <- ggplot(
  stack_modal,
  aes(x = stack_label, y = factor(opening_roll, levels = unique(truths$summary$opening_roll)), fill = mean_selected_reference_rank)
) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "#0B4F6C", high = "#E6CCB2") +
  labs(
    title = "Model-stack sensitivity for TS",
    subtitle = "Average selected truth rank by opening and model stack.",
    x = NULL,
    y = "Opening roll",
    fill = "Mean selected\ntruth rank"
  ) +
  bg_plot_theme_research()

presentation_save_plot(gap_plot, repo_root, "08_gallery_truth_gap_ladder", width = 10, height = 7)
presentation_save_plot(top1_plot, repo_root, "08_gallery_top1_curve", width = 10, height = 6)
presentation_save_plot(focus_plot, repo_root, "08_gallery_focus_curve", width = 10, height = 6)
presentation_save_plot(contrast_plot, repo_root, "08_gallery_ttts_minus_ts", width = 9, height = 7)
presentation_save_plot(stack_heatmap, repo_root, "08_gallery_model_stack_heatmap", width = 10, height = 8)

print(gap_plot)
print(top1_plot)
print(focus_plot)
print(contrast_plot)
print(stack_heatmap)


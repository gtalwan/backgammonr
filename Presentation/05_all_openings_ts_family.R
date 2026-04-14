# 05_all_openings_ts_family.R
#
# Purpose:
# - compare the Thompson-family methods plus equal across all 21 openings;
# - use the preserved master opening truths projected to the win/loss stack; and
# - summarize by opening first, then across openings.
#
# Main package functions used here:
# - bg_opening_truth_load_all()          [R/bg_truth.R]
# - bg_truth_project()                   [R/bg_truth.R]
# - bg_opening_compare_study()           [R/bg_truth.R]
#
# Relevant native files:
# - src/truth_proxy.cpp
# - src/model_beta_bernoulli.cpp
# - src/alloc_core.cpp
# - src/policy_ts.cpp
# - src/policy_ttts.cpp
# - src/policy_equal.cpp
#
# Note:
# `bg_opening_compare_study()` is used here as the repeated-study orchestrator.
# The direct method front doors remain the conceptual center; this helper just
# runs them repeatedly over openings on one shared direct path per method.
#
# These settings are intentionally presentation-oriented rather than research-
# batch sized: they are larger than the minimal smoke-test grid, but still
# small enough to run without rebuilding any truths.

common_path <- if (file.exists("DESCRIPTION")) {
  file.path("Presentation", "presentation_common.R")
} else {
  "presentation_common.R"
}
source(common_path)

repo_root <- presentation_repo_root()
presentation_load_package(repo_root)
library(ggplot2)

methods <- c(
  "thompson",
  "top_two_thompson",
  "multi_sample_thompson",
  "soft_elimination_thompson",
  "forced_exploration_thompson",
  "top_k_thompson",
  "equal"
)
budgets <- presentation_checkpoint_grid(presentation_run_budget())
seeds <- 1L
bootstrap_reps <- 200L

workers <- presentation_detect_workers(max_cores = 4L)
truths <- presentation_project_truths(
  presentation_load_opening_truths(repo_root),
  stack = "beta_bernoulli"
)
master_reference_budget <- presentation_master_reference_budget(truths)

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
    "05_all_openings_ts_family_win_loss_master_2048.rds"
  ),
  overwrite = TRUE,
  seed = 1L
)

final_checkpoint <- max(study$opening_summary$checkpoint)
final_opening <- subset(study$opening_summary, checkpoint == final_checkpoint)
final_aggregate <- subset(study$opening_aggregate, checkpoint == final_checkpoint)

final_method_table <- Reduce(
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
          "mean_share_mc_screened_suboptimal",
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

names(final_method_table) <- sub("^estimate\\.", "", names(final_method_table))
final_method_table$method <- presentation_method_label(final_method_table$allocation_policy)
final_method_table <- final_method_table[
  order(-final_method_table$mean_top1_match, final_method_table$mean_simple_regret),
  c(
    "method",
    "allocation_policy",
    "mean_top1_match",
    "mean_simple_regret",
    "mean_selected_reference_rank",
    "mean_truth_top2_hit",
    "mean_share_top2_truth",
    "mean_share_mc_screened_suboptimal",
    "mean_gap_weighted_wasted_allocation",
    "high_confidence_wrong_rate",
    "mean_runtime_seconds"
  )
]

method_levels <- presentation_method_label(methods)
method_palette <- presentation_method_palette(methods)

agg_top1 <- subset(study$opening_aggregate, metric == "mean_top1_match")
agg_regret <- subset(study$opening_aggregate, metric == "mean_simple_regret")
agg_focus <- subset(study$opening_aggregate, metric == "mean_share_top2_truth")
agg_waste <- subset(study$opening_aggregate, metric == "mean_gap_weighted_wasted_allocation")
agg_top2_hit <- subset(study$opening_aggregate, metric == "mean_truth_top2_hit")
agg_conf_wrong <- subset(study$opening_aggregate, metric == "high_confidence_wrong_rate")
agg_screened <- subset(study$opening_aggregate, metric == "mean_share_mc_screened_suboptimal")

agg_rank <- aggregate(
  mean_selected_reference_rank ~ allocation_policy + checkpoint,
  data = study$opening_summary,
  FUN = mean,
  na.rm = TRUE
)
names(agg_rank)[names(agg_rank) == "mean_selected_reference_rank"] <- "estimate"

for_plot <- function(df) {
  df$method <- factor(presentation_method_label(df$allocation_policy), levels = method_levels)
  df
}

agg_top1 <- for_plot(agg_top1)
agg_regret <- for_plot(agg_regret)
agg_focus <- for_plot(agg_focus)
agg_waste <- for_plot(agg_waste)
agg_top2_hit <- for_plot(agg_top2_hit)
agg_conf_wrong <- for_plot(agg_conf_wrong)
agg_rank <- for_plot(agg_rank)
agg_screened <- for_plot(agg_screened)

# This is the main all-opening headline plot. It answers which method most
# often finds the truth-best move across the entire opening battery.
plot_top1 <- ggplot(agg_top1, aes(x = checkpoint, y = estimate, color = method, ymin = lower_95, ymax = upper_95)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(fill = method), alpha = 0.12, linewidth = 0, color = NA) +
  scale_color_manual(values = method_palette) +
  scale_fill_manual(values = method_palette) +
  scale_x_continuous(trans = "log2", breaks = budgets) +
  labs(
    title = "All openings: mean top-1 match",
    subtitle = paste(
      "Each point is an opening-level mean; intervals bootstrap over openings.",
      "The underlying master truth cache carries a reference budget of",
      format(master_reference_budget, big.mark = ","),
      "rollouts per opening."
    ),
    x = "Budget",
    y = "Mean top-1 match",
    color = "Method",
    fill = "Method"
  ) +
  bg_plot_theme_research()

# Regret complements the binary top-1 view by showing how costly the misses are.
plot_regret <- ggplot(agg_regret, aes(x = checkpoint, y = estimate, color = method, ymin = lower_95, ymax = upper_95)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(fill = method), alpha = 0.12, linewidth = 0, color = NA) +
  scale_color_manual(values = method_palette) +
  scale_fill_manual(values = method_palette) +
  scale_x_continuous(trans = "log2", breaks = budgets) +
  labs(
    title = "All openings: mean simple regret",
    subtitle = "Lower is better.",
    x = "Budget",
    y = "Mean simple regret",
    color = "Method",
    fill = "Method"
  ) +
  bg_plot_theme_research()

# Focus on the truth top-2 is the primary allocation-mechanism explanation.
plot_focus <- ggplot(agg_focus, aes(x = checkpoint, y = estimate, color = method, ymin = lower_95, ymax = upper_95)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(fill = method), alpha = 0.12, linewidth = 0, color = NA) +
  scale_color_manual(values = method_palette) +
  scale_fill_manual(values = method_palette) +
  scale_x_continuous(trans = "log2", breaks = budgets) +
  labs(
    title = "All openings: budget share on the truth top-2",
    subtitle = "Higher is better.",
    x = "Budget",
    y = "Mean share on truth top-2",
    color = "Method",
    fill = "Method"
  ) +
  bg_plot_theme_research()

# Wasted allocation tells the inverse story: how much budget still lands in
# places that the truth would screen out as clearly inferior.
plot_waste <- ggplot(agg_waste, aes(x = checkpoint, y = estimate, color = method, ymin = lower_95, ymax = upper_95)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(fill = method), alpha = 0.12, linewidth = 0, color = NA) +
  scale_color_manual(values = method_palette) +
  scale_fill_manual(values = method_palette) +
  scale_x_continuous(trans = "log2", breaks = budgets) +
  labs(
    title = "All openings: gap-weighted wasted allocation",
    subtitle = "Lower is better.",
    x = "Budget",
    y = "Mean gap-weighted wasted allocation",
    color = "Method",
    fill = "Method"
  ) +
  bg_plot_theme_research()

# Top-2 hit is a forgiving accuracy measure. It is useful when the best move
# and second-best move are extremely close in the harder openings.
plot_top2_hit <- ggplot(agg_top2_hit, aes(x = checkpoint, y = estimate, color = method, ymin = lower_95, ymax = upper_95)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(fill = method), alpha = 0.12, linewidth = 0, color = NA) +
  scale_color_manual(values = method_palette) +
  scale_fill_manual(values = method_palette) +
  scale_x_continuous(trans = "log2", breaks = budgets) +
  labs(
    title = "All openings: truth top-2 hit rate",
    subtitle = "Higher is better; this is a forgiving but interpretable accuracy measure.",
    x = "Budget",
    y = "Mean truth top-2 hit",
    color = "Method",
    fill = "Method"
  ) +
  bg_plot_theme_research()

# This plot punishes a method for being both wrong and overconfident.
plot_conf_wrong <- ggplot(agg_conf_wrong, aes(x = checkpoint, y = estimate, color = method, ymin = lower_95, ymax = upper_95)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(fill = method), alpha = 0.12, linewidth = 0, color = NA) +
  scale_color_manual(values = method_palette) +
  scale_fill_manual(values = method_palette) +
  scale_x_continuous(trans = "log2", breaks = budgets) +
  labs(
    title = "All openings: high-confidence wrong rate",
    subtitle = "Lower is better; this is a strict error metric that combines confidence with being wrong.",
    x = "Budget",
    y = "High-confidence wrong rate",
    color = "Method",
    fill = "Method"
  ) +
  bg_plot_theme_research()

plot_rank <- ggplot(agg_rank, aes(x = checkpoint, y = estimate, color = method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = method_palette) +
  scale_x_continuous(trans = "log2", breaks = budgets) +
  labs(
    title = "All openings: mean selected truth rank",
    subtitle = "Lower is better; this gives more gradation than top-1 match alone. This line is a pooled opening mean without a bootstrap ribbon.",
    x = "Budget",
    y = "Mean selected truth rank",
    color = "Method"
  ) +
  bg_plot_theme_research()

plot_screened <- ggplot(agg_screened, aes(x = checkpoint, y = estimate, color = method, ymin = lower_95, ymax = upper_95)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(fill = method), alpha = 0.12, linewidth = 0, color = NA) +
  scale_color_manual(values = method_palette) +
  scale_fill_manual(values = method_palette) +
  scale_x_continuous(trans = "log2", breaks = budgets) +
  labs(
    title = "All openings: budget share on MC-screened suboptimal moves",
    subtitle = "Lower is better; strong methods stop spending budget on clearly inferior moves.",
    x = "Budget",
    y = "Mean share on screened-out moves",
    color = "Method",
    fill = "Method"
  ) +
  bg_plot_theme_research()

# The heatmap is the per-opening view. It shows where the pooled averages are
# hiding meaningful opening-to-opening variation.
heatmap_data <- final_opening[, c("opening_roll", "allocation_policy", "mean_top1_match")]
heatmap_data$method <- factor(presentation_method_label(heatmap_data$allocation_policy), levels = method_levels)
heatmap_data$opening_roll <- factor(heatmap_data$opening_roll, levels = unique(truths$summary$opening_roll))

heatmap_plot <- ggplot(heatmap_data, aes(x = method, y = opening_roll, fill = mean_top1_match)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "#F4F1EA", high = "#0B4F6C") +
  labs(
    title = "Final top-1 match by opening and method",
    subtitle = paste("Final checkpoint =", final_checkpoint),
    x = NULL,
    y = "Opening roll",
    fill = "Top-1\nmatch"
  ) +
  bg_plot_theme_research()

winner_counts <- aggregate(
  mean_top1_match ~ allocation_policy,
  data = final_opening,
  FUN = function(x) 0
)
winner_by_opening <- do.call(
  rbind,
  lapply(
    split(final_opening, final_opening$opening_roll),
    function(df) df[which.max(df$mean_top1_match), c("opening_roll", "allocation_policy"), drop = FALSE]
  )
)
winner_counts <- as.data.frame(table(winner_by_opening$allocation_policy), stringsAsFactors = FALSE)
names(winner_counts) <- c("allocation_policy", "n_openings_won")
winner_counts$method <- presentation_method_label(winner_counts$allocation_policy)

final_ts_equal <- merge(
  final_opening[final_opening$allocation_policy == "thompson", c("opening_roll", "mean_top1_match")],
  final_opening[final_opening$allocation_policy == "equal", c("opening_roll", "mean_top1_match")],
  by = "opening_roll",
  suffixes = c("_ts", "_equal"),
  all = FALSE
)
final_ts_equal <- merge(
  final_ts_equal,
  truths$summary[, c("opening_roll", "top_two_gap_estimate", "difficulty_label")],
  by = "opening_roll",
  all.x = TRUE,
  sort = FALSE
)
final_ts_equal$ts_minus_equal_top1 <- final_ts_equal$mean_top1_match_ts - final_ts_equal$mean_top1_match_equal

gain_plot <- ggplot(
  final_ts_equal,
  aes(x = top_two_gap_estimate, y = ts_minus_equal_top1, color = difficulty_label)
) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "#6C757D") +
  geom_point(size = 2.5, alpha = 0.9) +
  geom_smooth(se = FALSE, linewidth = 0.8, method = "lm") +
  labs(
    title = "TS gain over equal versus opening hardness",
    subtitle = "Positive values mean TS beats equal on final top-1 match.",
    x = "Truth top-two gap",
    y = "TS minus equal top-1 match",
    color = "Difficulty"
  ) +
  bg_plot_theme_research()

winner_plot <- ggplot(
  winner_counts,
  aes(x = reorder(method, n_openings_won), y = n_openings_won, fill = method)
) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = method_palette) +
  labs(
    title = "Number of openings won on final top-1 match",
    subtitle = paste("Winner defined at checkpoint", final_checkpoint),
    x = NULL,
    y = "Openings won"
  ) +
  bg_plot_theme_research()

presentation_save_table(final_method_table, repo_root, "05_all_openings_ts_family_final_table")
presentation_save_table(final_opening, repo_root, "05_all_openings_ts_family_opening_summary_final")
presentation_save_table(winner_counts, repo_root, "05_all_openings_ts_family_opening_wins")
presentation_save_plot(plot_top1, repo_root, "05_all_openings_top1", width = 10, height = 6)
presentation_save_plot(plot_regret, repo_root, "05_all_openings_regret", width = 10, height = 6)
presentation_save_plot(plot_focus, repo_root, "05_all_openings_focus", width = 10, height = 6)
presentation_save_plot(plot_waste, repo_root, "05_all_openings_waste", width = 10, height = 6)
presentation_save_plot(plot_top2_hit, repo_root, "05_all_openings_top2_hit", width = 10, height = 6)
presentation_save_plot(plot_conf_wrong, repo_root, "05_all_openings_high_conf_wrong", width = 10, height = 6)
presentation_save_plot(plot_rank, repo_root, "05_all_openings_rank", width = 10, height = 6)
presentation_save_plot(plot_screened, repo_root, "05_all_openings_screened_share", width = 10, height = 6)
presentation_save_plot(heatmap_plot, repo_root, "05_all_openings_top1_heatmap", width = 10, height = 8)
presentation_save_plot(winner_plot, repo_root, "05_all_openings_winner_counts", width = 8, height = 5.5)
presentation_save_plot(gain_plot, repo_root, "05_all_openings_ts_gain_vs_hardness", width = 9, height = 6)

# This is the final pooled leaderboard across openings.
print(final_method_table)

# This table answers a different question: which method actually wins the most
# openings at the final checkpoint, not just on average?
print(winner_counts)
print(plot_top1)
print(plot_regret)
print(plot_focus)
print(plot_waste)
print(plot_top2_hit)
print(plot_conf_wrong)
print(plot_rank)
print(plot_screened)
print(heatmap_plot)
print(winner_plot)
print(gain_plot)

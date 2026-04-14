# 03_one_opening_ts_family.R
#
# Purpose:
# - compare the Thompson-family methods plus equal on one opening roll;
# - use the direct run functions as the main story; and
# - show where the methods differ in allocation and regret.
#
# Main package functions used here:
# - bg_opening_truth_load_one()          [R/bg_truth.R]
# - bg_truth_project()                   [R/bg_truth.R]
# - bg_ts_run()                          [R/bg_algorithms.R]
# - bg_ttts_run()                        [R/bg_algorithms.R]
# - bg_multi_sample_ts_run()             [R/bg_algorithms.R]
# - bg_soft_elimination_ts_run()         [R/bg_algorithms.R]
# - bg_forced_exploration_ts_run()       [R/bg_algorithms.R]
# - bg_top_k_ts_run()                    [R/bg_algorithms.R]
# - bg_equal_run()                       [R/bg_algorithms.R]
# - bg_eval_reference_aware()            [R/bg_metrics.R]
#
# Relevant native files:
# - src/alloc_core.cpp
# - src/policy_ts.cpp
# - src/policy_ttts.cpp
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
methods <- c(
  "thompson",
  "top_two_thompson",
  "multi_sample_thompson",
  "soft_elimination_thompson",
  "forced_exploration_thompson",
  "top_k_thompson",
  "equal"
)
budget <- presentation_run_budget()
checkpoints <- presentation_checkpoint_grid(budget)
seeds <- 1L

# This script still uses the direct method functions as the main story. The
# helper below simply runs those front doors on one shared budget grid.
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

saveRDS(study, file = file.path(presentation_output_dir(repo_root, "studies"), "03_one_opening_ts_family_1_6.rds"))
presentation_save_table(study$panel, repo_root, "03_one_opening_ts_family_panel_1_6")
presentation_save_table(study$final_summary, repo_root, "03_one_opening_ts_family_final_1_6")

method_levels <- presentation_method_label(methods)
method_palette <- presentation_method_palette(methods)

curve_data <- study$checkpoint_summary
curve_data$method <- factor(
  presentation_method_label(curve_data$allocation_policy),
  levels = method_levels
)

# Top-1 match is the cleanest headline score when the audience only wants to
# know whether the method gets the best move.
plot_top1 <- ggplot(curve_data, aes(x = checkpoint, y = top1_match, color = method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = method_palette) +
  scale_x_continuous(trans = "log2", breaks = checkpoints) +
  labs(
    title = paste("One opening TS-family comparison on", roll_label),
    subtitle = "Direct method paths on one shared checkpoint grid.",
    x = "Budget",
    y = "Top-1 match",
    color = "Method"
  ) +
  bg_plot_theme_research()

# Regret gives a smoother view than top-1 match because it still rewards
# methods that are close even when they miss the exact best move.
plot_regret <- ggplot(curve_data, aes(x = checkpoint, y = simple_regret, color = method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = method_palette) +
  scale_x_continuous(trans = "log2", breaks = checkpoints) +
  labs(
    title = paste("Simple regret on", roll_label),
    subtitle = "Lower is better.",
    x = "Budget",
    y = "Simple regret",
    color = "Method"
  ) +
  bg_plot_theme_research()

# Budget focus is the mechanistic explanation for why some methods look better:
# better methods move budget toward the truly competitive moves faster.
plot_focus <- ggplot(curve_data, aes(x = checkpoint, y = share_top2_truth, color = method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = method_palette) +
  scale_x_continuous(trans = "log2", breaks = checkpoints) +
  labs(
    title = paste("Allocation focus on the truth top-2 for", roll_label),
    subtitle = "Higher is better.",
    x = "Budget",
    y = "Share of budget on the truth top-2",
    color = "Method"
  ) +
  bg_plot_theme_research()

# Wasted allocation is the complementary view: how much effort still goes to
# moves that the truth would already screen out.
plot_waste <- ggplot(curve_data, aes(x = checkpoint, y = gap_weighted_wasted_allocation, color = method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = method_palette) +
  scale_x_continuous(trans = "log2", breaks = checkpoints) +
  labs(
    title = paste("Gap-weighted wasted allocation on", roll_label),
    subtitle = "Lower is better.",
    x = "Budget",
    y = "Gap-weighted wasted allocation",
    color = "Method"
  ) +
  bg_plot_theme_research()

# Selected truth rank is useful when top-1 match is too coarse. A method with
# rank 2 is often behaving much better than one bouncing around rank 5 or 6.
plot_rank <- ggplot(curve_data, aes(x = checkpoint, y = selected_reference_rank, color = method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = method_palette) +
  scale_x_continuous(trans = "log2", breaks = checkpoints) +
  labs(
    title = paste("Selected truth rank on", roll_label),
    subtitle = "Lower is better; rank 1 means the method selected the truth-best move.",
    x = "Budget",
    y = "Selected truth rank",
    color = "Method"
  ) +
  bg_plot_theme_research()

# Posterior confidence is only interpretable next to the truth-aware metrics
# above; it shows how quickly each method becomes convinced of its own choice.
plot_probbest <- ggplot(curve_data, aes(x = checkpoint, y = recommended_prob_best, color = method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = method_palette) +
  scale_x_continuous(trans = "log2", breaks = checkpoints) +
  labs(
    title = paste("Posterior confidence by method on", roll_label),
    subtitle = "Higher means the method is more confident that its current recommendation is best.",
    x = "Budget",
    y = "Recommended probability-best",
    color = "Method"
  ) +
  bg_plot_theme_research()

plot_top2_hit <- ggplot(curve_data, aes(x = checkpoint, y = truth_top2_hit, color = method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = method_palette) +
  scale_x_continuous(trans = "log2", breaks = checkpoints) +
  labs(
    title = paste("Truth top-2 hit rate on", roll_label),
    subtitle = "This is a forgiving accuracy view that still tracks whether a method stays near the top.",
    x = "Budget",
    y = "Truth top-2 hit",
    color = "Method"
  ) +
  bg_plot_theme_research()

seed1_rows <- lapply(
  methods,
  function(method) {
    run_i <- study$runs[[paste(method, 1L, sep = "::")]]
    tab <- run_i$action_table[
      order(run_i$action_table$proxy_reference_rank),
      c("move_label", "allocation_count", "proxy_reference_rank", "recommended"),
      drop = FALSE
    ]
    tab <- utils::head(tab, 6L)
    tab$method <- presentation_method_label(method)
    tab
  }
)
seed1_alloc <- do.call(rbind, seed1_rows)
seed1_alloc$move_label <- factor(seed1_alloc$move_label, levels = rev(unique(seed1_alloc$move_label)))

allocation_small_multiples <- ggplot(
  seed1_alloc,
  aes(x = allocation_count, y = move_label, fill = recommended)
) +
  geom_col(color = "#1f1f1f", linewidth = 0.2) +
  facet_wrap(~ method, scales = "free_x") +
  scale_fill_manual(values = c(`TRUE` = "#D55E00", `FALSE` = "#C9D2D8")) +
  labs(
    title = paste("Seed-1 allocation profile by method on", roll_label),
    subtitle = "Each panel shows which moves received budget at the final checkpoint.",
    x = "Allocation count",
    y = NULL,
    fill = "Recommended"
  ) +
  bg_plot_theme_research()

winner_table <- data.frame(
  metric = c(
    "top1_match",
    "simple_regret",
    "selected_reference_rank",
    "share_top2_truth",
    "gap_weighted_wasted_allocation"
  ),
  winner = c(
    study$final_summary$method[[which.max(study$final_summary$top1_match)]],
    study$final_summary$method[[which.min(study$final_summary$simple_regret)]],
    study$final_summary$method[[which.min(study$final_summary$selected_reference_rank)]],
    study$final_summary$method[[which.max(study$final_summary$share_top2_truth)]],
    study$final_summary$method[[which.min(study$final_summary$gap_weighted_wasted_allocation)]]
  ),
  stringsAsFactors = FALSE
)

presentation_save_table(winner_table, repo_root, "03_one_opening_ts_family_winners_1_6")
presentation_save_plot(plot_top1, repo_root, "03_one_opening_top1_1_6", width = 9, height = 6)
presentation_save_plot(plot_regret, repo_root, "03_one_opening_regret_1_6", width = 9, height = 6)
presentation_save_plot(plot_focus, repo_root, "03_one_opening_focus_1_6", width = 9, height = 6)
presentation_save_plot(plot_waste, repo_root, "03_one_opening_waste_1_6", width = 9, height = 6)
presentation_save_plot(plot_rank, repo_root, "03_one_opening_rank_1_6", width = 9, height = 6)
presentation_save_plot(plot_top2_hit, repo_root, "03_one_opening_top2_hit_1_6", width = 9, height = 6)
presentation_save_plot(plot_probbest, repo_root, "03_one_opening_prob_best_1_6", width = 9, height = 6)
presentation_save_plot(allocation_small_multiples, repo_root, "03_one_opening_allocation_small_multiples_1_6", width = 12, height = 8)

# The final summary is the easiest side-by-side table to put on a slide.
print(study$final_summary)

# The winner table makes the conclusion explicit instead of asking the audience
# to infer it from several curves at once.
print(winner_table)
print(plot_top1)
print(plot_regret)
print(plot_focus)
print(plot_waste)
print(plot_rank)
print(plot_top2_hit)
print(plot_probbest)
print(allocation_small_multiples)

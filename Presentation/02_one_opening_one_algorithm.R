# 02_one_opening_one_algorithm.R
#
# Purpose:
# - take one cached opening truth;
# - project it to the win/loss + Beta-Bernoulli stack;
# - run one Thompson-sampling path; and
# - show exactly what the main run outputs mean.
#
# Main package functions used here:
# - bg_opening_truth_load_one()          [R/bg_truth.R]
# - bg_truth_project()                   [R/bg_truth.R]
# - bg_ts_run()                          [R/bg_algorithms.R]
# - bg_eval_reference_aware()            [R/bg_metrics.R]
# - bg_ts_diagnostics()                  [R/bg_metrics.R]
# - plot_bg_truth()                      [R/bg_plots.R]
# - plot_bg_ts_trace()                   [R/bg_plots.R]
# - plot_bg_rank_compare()               [R/bg_plots.R]
# - plot_bg_allocation()                 [R/bg_plots.R]
#
# Relevant native files:
# - src/truth_proxy.cpp
# - src/model_beta_bernoulli.cpp
# - src/alloc_core.cpp
# - src/policy_ts.cpp

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
budget <- presentation_run_budget()
checkpoints <- presentation_checkpoint_grid(budget)

# `presentation_load_opening_truth()` reads one preserved master truth from
# cache/opening_truths_master, and `presentation_project_truths()` re-expresses
# that same truth under the Beta-Bernoulli win/loss stack.
truth <- presentation_project_truths(
  presentation_load_opening_truth(repo_root, roll_label),
  stack = "beta_bernoulli"
)

candidate_table <- bg_move_features(truth$problem)[, c("candidate_index", "move_label", "n_equivalent_sequences")]
truth_table <- truth$reference$action_table[
  order(truth$reference$action_table$rank),
  c("rank", "move_label", "reference_mean", "reference_mc_lower_95", "reference_mc_upper_95", "wins", "losses", "unresolved")
]

fit_ts <- bg_ts_run(
  problem = truth$problem,
  budget = budget,
  checkpoints = checkpoints,
  proxy_reference = truth$reference,
  seed = 1L
)

checkpoint_panel <- bg_eval_reference_aware(
  fit_ts,
  truth = truth,
  checkpoints = checkpoints,
  top_k = 3L
)

diagnostics <- bg_ts_diagnostics(fit_ts, truth = truth)
final_checkpoint <- checkpoint_panel[checkpoint_panel$checkpoint == max(checkpoints), , drop = FALSE]
run_summary <- data.frame(
  opening_roll = roll_label,
  run_budget = budget,
  master_reference_budget = truth$reference$summary$reference_budget[[1L]],
  recommended_move_label = fit_ts$recommended_move_label,
  truth_best_move_label = truth$reference$summary$proxy_reference_best_move_label[[1L]],
  stringsAsFactors = FALSE
)

# The truth plot is the fixed target for everything that follows. It answers
# which move is actually best under the projected win/loss payoff definition.
truth_plot <- plot_bg_truth(truth) +
  labs(
    title = paste("Truth table for opening", roll_label),
    subtitle = "This is the target ranking under the win/loss rollout model."
  )

# The trace plot shows where the TS budget actually goes over time.
trace_plot <- plot_bg_ts_trace(fit_ts, metric = "allocation", top_n = 6L) +
  labs(
    title = paste("Allocation trace for TS on", roll_label),
    subtitle = "The run only tracks the main contenders at the final budget."
  )

# The rank comparison plot lets you compare the model's final ordering against
# the projected truth ordering at the same budget.
rank_plot <- plot_bg_rank_compare(fit_ts, truth = truth, checkpoint = budget, top_n = 8L) +
  labs(
    title = paste("Estimated values versus truth on", roll_label)
  )

# The allocation plot answers the practical question: which moves actually
# received the simulation budget once TS started separating contenders?
allocation_plot <- plot_bg_allocation(fit_ts, truth = truth, checkpoint = budget, top_n = 8L) +
  labs(
    title = paste("Final allocation profile on", roll_label)
  )

# These path metrics are the main way to read one direct run. Together they
# show correctness, regret, ranking quality, and focus on the true contenders.
path_metric_long <- rbind(
  data.frame(checkpoint = checkpoint_panel$checkpoint, metric = "Top-1 match", value = as.numeric(checkpoint_panel$top1_match)),
  data.frame(checkpoint = checkpoint_panel$checkpoint, metric = "Simple regret", value = checkpoint_panel$simple_regret),
  data.frame(checkpoint = checkpoint_panel$checkpoint, metric = "Selected truth rank", value = checkpoint_panel$selected_reference_rank),
  data.frame(checkpoint = checkpoint_panel$checkpoint, metric = "Share on truth top-2", value = checkpoint_panel$share_top2_truth)
)

path_metric_plot <- ggplot(path_metric_long, aes(x = checkpoint, y = value)) +
  geom_line(linewidth = 1, color = "#0072B2") +
  geom_point(size = 2, color = "#0072B2") +
  facet_wrap(~ metric, scales = "free_y") +
  scale_x_continuous(trans = "log2", breaks = checkpoints) +
  labs(
    title = paste("How the TS run evolves on", roll_label),
    subtitle = "The same run can be read as correctness, regret, rank quality, and budget focus.",
    x = "Budget",
    y = NULL
  ) +
  bg_plot_theme_research()

# This plot is purely within-run posterior confidence. It should be interpreted
# alongside, not instead of, the actual truth-aware metrics above.
probbest_plot <- ggplot(
  checkpoint_panel,
  aes(x = checkpoint, y = recommended_prob_best)
) +
  geom_line(linewidth = 1, color = "#D55E00") +
  geom_point(size = 2, color = "#D55E00") +
  scale_x_continuous(trans = "log2", breaks = checkpoints) +
  labs(
    title = paste("Model confidence in the current recommendation on", roll_label),
    subtitle = "This is posterior confidence within the run, not an empirical success rate across repeated runs.",
    x = "Budget",
    y = "Recommended probability-best"
  ) +
  bg_plot_theme_research()

allocation_quality_long <- rbind(
  data.frame(checkpoint = checkpoint_panel$checkpoint, metric = "Share MC-screened suboptimal", value = checkpoint_panel$share_mc_screened_suboptimal),
  data.frame(checkpoint = checkpoint_panel$checkpoint, metric = "Gap-weighted wasted allocation", value = checkpoint_panel$gap_weighted_wasted_allocation)
)

# These supporting diagnostics explain why the run looks good or bad. They are
# especially useful when top-1 and regret tell slightly different stories.
allocation_quality_plot <- ggplot(allocation_quality_long, aes(x = checkpoint, y = value)) +
  geom_line(linewidth = 1, color = "#6C757D") +
  geom_point(size = 2, color = "#6C757D") +
  facet_wrap(~ metric, scales = "free_y") +
  scale_x_continuous(trans = "log2", breaks = checkpoints) +
  labs(
    title = paste("Supporting allocation diagnostics on", roll_label),
    subtitle = "Lower wasted allocation and less budget on screened-out moves are both good signs.",
    x = "Budget",
    y = NULL
  ) +
  bg_plot_theme_research()

presentation_save_table(candidate_table, repo_root, "02_candidate_table_1_6")
presentation_save_table(truth_table, repo_root, "02_truth_table_1_6")
presentation_save_table(checkpoint_panel, repo_root, "02_checkpoint_panel_1_6_ts")
presentation_save_table(diagnostics$accuracy, repo_root, "02_ts_accuracy_diagnostics_1_6")
presentation_save_table(run_summary, repo_root, "02_run_summary_1_6")
saveRDS(fit_ts, file = file.path(presentation_output_dir(repo_root, "studies"), "02_one_opening_ts_run_1_6.rds"))

presentation_save_plot(truth_plot, repo_root, "02_truth_plot_1_6", width = 10, height = 6)
presentation_save_plot(trace_plot, repo_root, "02_ts_trace_1_6", width = 10, height = 6)
presentation_save_plot(rank_plot, repo_root, "02_rank_compare_1_6", width = 10, height = 6)
presentation_save_plot(allocation_plot, repo_root, "02_allocation_1_6", width = 10, height = 6)
presentation_save_plot(path_metric_plot, repo_root, "02_path_metrics_1_6", width = 12, height = 8)
presentation_save_plot(probbest_plot, repo_root, "02_prob_best_1_6", width = 10, height = 6)
presentation_save_plot(allocation_quality_plot, repo_root, "02_allocation_quality_1_6", width = 12, height = 7)

# This summary table tells the whole one-run story in one row.
print(run_summary)

# The candidate table is intentionally simple. It anchors the opening decision
# problem before any truth or TS diagnostics are shown.
print(candidate_table)

# The truth table is the reference ranking, not an algorithm output.
print(utils::head(truth_table, 8L))

# The checkpoint panel is the main truth-aware path summary for this script.
print(checkpoint_panel[, c(
  "checkpoint",
  "recommended_move_label",
  "top1_match",
  "simple_regret",
  "selected_reference_rank",
  "truth_top2_hit",
  "share_top2_truth",
  "recommended_prob_best"
)])

# The final checkpoint row is the shortest table to put next to the end-state
# allocation plot when presenting this example.
print(final_checkpoint[, c(
  "recommended_move_label",
  "top1_match",
  "simple_regret",
  "selected_reference_rank",
  "truth_top2_hit",
  "share_top2_truth",
  "recommended_prob_best"
)])

# These diagnostics support the main path metrics but do not replace them.
print(diagnostics$accuracy)

print(truth_plot)
print(trace_plot)
print(rank_plot)
print(allocation_plot)
print(path_metric_plot)
print(probbest_plot)
print(allocation_quality_plot)


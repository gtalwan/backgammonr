# 06_forced_exploration_ts_vs_equal.R
#
# Purpose:
# - compare forced-exploration TS with equal allocation on one opening; and
# - keep the same output structure as the other one-roll method files.
#
# Relevant implementation files:
# - R/bg_algorithms.R
# - R/bg_metrics.R
# - R/bg_plots.R
# - src/policy_forced_exploration.cpp
# - src/policy_equal.cpp
# - src/model_beta_bernoulli.cpp
# - src/alloc_core.cpp

source(if (file.exists("DESCRIPTION")) "results/00_results_common.R" else "00_results_common.R")

repo_root <- results_repo_root()
results_load_package(repo_root)

# Change these settings to switch the opening or budget ladder.
roll <- "1-6"
stack <- "beta_bernoulli"
budget <- results_run_budget()
checkpoints <- results_checkpoint_grid(budget)

master_truth <- results_load_master_truth(repo_root, roll = roll)
stack_spec <- results_stack_spec(stack)
truth <- results_project_truth(master_truth, stack = stack)

truth_overview <- results_truth_overview(master_truth, truth, roll, stack_spec$label)
truth_table <- results_truth_action_table(truth, top_n = 10L)

fit_forced <- bg_forced_exploration_ts_run(
  problem = truth$problem,
  budget = budget,
  checkpoints = checkpoints,
  proxy_reference = truth$reference,
  seed = 1L
)

fit_equal <- bg_equal_run(
  problem = truth$problem,
  budget = budget,
  checkpoints = checkpoints,
  proxy_reference = truth$reference,
  seed = 1L
)

forced_panels <- results_collect_run_panels(fit_forced, truth, checkpoints)
equal_panels <- results_collect_run_panels(fit_equal, truth, checkpoints)

checkpoint_summary <- rbind(forced_panels$checkpoint_metrics, equal_panels$checkpoint_metrics)
final_summary <- rbind(
  results_final_summary("forced_exploration_thompson", forced_panels$checkpoint_metrics),
  results_final_summary("equal", equal_panels$checkpoint_metrics)
)

truth_plot <- plot_bg_truth(truth) + ggplot2::labs(title = paste(stack_spec$label, "truth for opening", roll))
forced_trace_plot <- plot_bg_ts_trace(fit_forced) + ggplot2::labs(title = paste("Forced-Exploration TS trace on opening", roll))
equal_trace_plot <- plot_bg_ts_trace(fit_equal) + ggplot2::labs(title = paste("Equal trace on opening", roll))
forced_rank_plot <- plot_bg_rank_compare(fit_forced, truth = truth) + ggplot2::labs(title = paste("Forced-Exploration TS ranking versus truth on opening", roll))
equal_rank_plot <- plot_bg_rank_compare(fit_equal, truth = truth) + ggplot2::labs(title = paste("Equal ranking versus truth on opening", roll))
forced_allocation_plot <- plot_bg_allocation(fit_forced, truth = truth) + ggplot2::labs(title = paste("Forced-Exploration TS allocation versus truth on opening", roll))
equal_allocation_plot <- plot_bg_allocation(fit_equal, truth = truth) + ggplot2::labs(title = paste("Equal allocation versus truth on opening", roll))

top1_plot <- results_plot_top1_heatmap(checkpoint_summary, paste("Forced-Exploration TS versus Equal on opening", roll), "Checkpoint heatmap: did the current recommendation match the truth-best move?", methods = c("forced_exploration_thompson", "equal"))
top2_hit_plot <- results_plot_truth_top2_heatmap(checkpoint_summary, paste("Forced-Exploration TS versus Equal on opening", roll), "Checkpoint heatmap: was the recommendation at least in the truth top-2?", methods = c("forced_exploration_thompson", "equal"))
rank_plot <- results_plot_rank_confidence(checkpoint_summary, paste("Forced-Exploration TS versus Equal on opening", roll), "Truth rank of the recommended move over budget. Lower is better, and point fill shows posterior confidence.", methods = c("forced_exploration_thompson", "equal"))
regret_plot <- results_plot_checkpoint_lines(checkpoint_summary, "simple_regret", paste("Forced-Exploration TS versus Equal on opening", roll), "Simple regret over budget. Lower is better.", "simple_regret", methods = c("forced_exploration_thompson", "equal"))
spearman_plot <- results_plot_checkpoint_lines(checkpoint_summary, "spearman", paste("Forced-Exploration TS versus Equal on opening", roll), "Spearman rank correlation over budget.", "spearman", methods = c("forced_exploration_thompson", "equal"))
share_best_plot <- results_plot_checkpoint_lines(checkpoint_summary, "share_best_truth", paste("Forced-Exploration TS versus Equal on opening", roll), "Share of budget placed on the truth-best move.", "share_best_truth", methods = c("forced_exploration_thompson", "equal"))
focus_plot <- results_plot_checkpoint_lines(checkpoint_summary, "share_top2_truth", paste("Forced-Exploration TS versus Equal on opening", roll), "Share of budget placed on the truth top-2 moves.", "share_top2_truth", methods = c("forced_exploration_thompson", "equal"))
screened_plot <- results_plot_checkpoint_lines(checkpoint_summary, "share_mc_screened_suboptimal", paste("Forced-Exploration TS versus Equal on opening", roll), "Share of budget still going to MC-screened suboptimal moves. Lower is better.", "share_mc_screened_suboptimal", methods = c("forced_exploration_thompson", "equal"))
waste_plot <- results_plot_checkpoint_lines(checkpoint_summary, "gap_weighted_wasted_allocation", paste("Forced-Exploration TS versus Equal on opening", roll), "Gap-weighted wasted allocation over budget. Lower is better.", "gap_weighted_wasted_allocation", methods = c("forced_exploration_thompson", "equal"))
confidence_plot <- results_plot_confidence_heatmap(checkpoint_summary, paste("Forced-Exploration TS versus Equal on opening", roll), "Posterior confidence at each checkpoint. Higher values mean the model believes the current recommendation is more likely to be best.", methods = c("forced_exploration_thompson", "equal"))
confidence_regret_plot <- results_plot_confidence_regret_scatter(checkpoint_summary, paste("Confidence versus regret on opening", roll), "Selected checkpoints show whether higher posterior confidence coincides with lower regret.", methods = c("forced_exploration_thompson", "equal"))
timeline_plot <- results_plot_recommended_timeline(checkpoint_summary, paste("Recommended-move timeline on opening", roll), "Tiles show which move each method recommends at each checkpoint; darker tiles have better truth rank.", methods = c("forced_exploration_thompson", "equal"))

results_save_table(truth_overview, repo_root, paste0("06_forced_exploration_ts_vs_equal_truth_overview_", results_roll_tag(roll), "_", stack))
results_save_table(truth_table, repo_root, paste0("06_forced_exploration_ts_vs_equal_truth_table_", results_roll_tag(roll), "_", stack))
results_save_table(checkpoint_summary, repo_root, paste0("06_forced_exploration_ts_vs_equal_checkpoint_summary_", results_roll_tag(roll), "_", stack))
results_save_table(final_summary, repo_root, paste0("06_forced_exploration_ts_vs_equal_final_summary_", results_roll_tag(roll), "_", stack))

results_save_plot(truth_plot, repo_root, paste0("06_forced_exploration_ts_vs_equal_truth_", results_roll_tag(roll), "_", stack), width = 10, height = 7)
results_save_plot(forced_trace_plot, repo_root, paste0("06_forced_exploration_ts_vs_equal_trace_forced_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(equal_trace_plot, repo_root, paste0("06_forced_exploration_ts_vs_equal_trace_equal_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(forced_rank_plot, repo_root, paste0("06_forced_exploration_ts_vs_equal_rank_forced_", results_roll_tag(roll), "_", stack), width = 9, height = 6)
results_save_plot(equal_rank_plot, repo_root, paste0("06_forced_exploration_ts_vs_equal_rank_equal_", results_roll_tag(roll), "_", stack), width = 9, height = 6)
results_save_plot(forced_allocation_plot, repo_root, paste0("06_forced_exploration_ts_vs_equal_allocation_forced_", results_roll_tag(roll), "_", stack), width = 9, height = 6)
results_save_plot(equal_allocation_plot, repo_root, paste0("06_forced_exploration_ts_vs_equal_allocation_equal_", results_roll_tag(roll), "_", stack), width = 9, height = 6)
results_save_plot(top1_plot, repo_root, paste0("06_forced_exploration_ts_vs_equal_top1_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(top2_hit_plot, repo_root, paste0("06_forced_exploration_ts_vs_equal_top2_hit_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(rank_plot, repo_root, paste0("06_forced_exploration_ts_vs_equal_rank_path_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(regret_plot, repo_root, paste0("06_forced_exploration_ts_vs_equal_regret_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(spearman_plot, repo_root, paste0("06_forced_exploration_ts_vs_equal_spearman_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(share_best_plot, repo_root, paste0("06_forced_exploration_ts_vs_equal_share_best_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(focus_plot, repo_root, paste0("06_forced_exploration_ts_vs_equal_focus_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(screened_plot, repo_root, paste0("06_forced_exploration_ts_vs_equal_screened_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(waste_plot, repo_root, paste0("06_forced_exploration_ts_vs_equal_waste_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(confidence_plot, repo_root, paste0("06_forced_exploration_ts_vs_equal_confidence_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(confidence_regret_plot, repo_root, paste0("06_forced_exploration_ts_vs_equal_confidence_regret_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(timeline_plot, repo_root, paste0("06_forced_exploration_ts_vs_equal_timeline_", results_roll_tag(roll), "_", stack), width = 11, height = 6)

# Which opening and truth object are we studying?
print(truth_overview)
print(truth_table)
print(final_summary)

# What is the proxy truth and how do the direct run traces look?
# Question: what does the truth ranking itself look like?
print(truth_plot)
# Question: how does the direct Forced-Exploration TS run evolve over the checkpoint ladder?
print(forced_trace_plot)
# Question: how does the equal-allocation baseline evolve over the same ladder?
print(equal_trace_plot)

# How do the methods rank and allocate rollouts across moves?
# Question: what ranking does Forced-Exploration TS recover relative to the proxy truth?
print(forced_rank_plot)
# Question: what ranking does Equal recover relative to the proxy truth?
print(equal_rank_plot)
# Question: where does Forced-Exploration TS actually spend its rollout budget?
print(forced_allocation_plot)
# Question: where does Equal spend its rollout budget?
print(equal_allocation_plot)

# Does the recommendation match the truth best move or at least stay in the top-2?
# Question: does the current recommendation exactly match the truth-best move?
print(top1_plot)
# Question: even when it misses top-1, does it at least stay in the true contender set?
print(top2_hit_plot)

# How costly are mistakes, how close is the recommendation to rank 1, and how well is the full ranking recovered?
# The rank path also carries posterior confidence through point fill, so it
# shows recommendation quality and model certainty in one graph.
# Question: how close is the current recommendation to truth rank 1, and how confident is the model at the same checkpoint?
print(rank_plot)
# Question: how costly is the current recommendation if it is wrong?
print(regret_plot)
# Question: is the full ordering of moves being recovered, not just the top move?
print(spearman_plot)
# Question: is budget concentrating on the single truth-best move?
print(share_best_plot)
# Question: is budget concentrating on the two moves that matter most?
print(focus_plot)
# Question: how much budget is still going to moves that Monte Carlo evidence already screens out?
print(screened_plot)
# Question: after weighting by how bad the moves really are, how much budget is being wasted?
print(waste_plot)

# Does posterior confidence line up with lower regret, and when does the recommendation itself change?
# Question: how confident is the posterior in the current recommendation at each checkpoint?
print(confidence_plot)
# Question: when confidence rises, is regret actually falling?
print(confidence_regret_plot)
# Question: when does each method switch its recommended move over the run?
print(timeline_plot)

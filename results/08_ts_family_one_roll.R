# 08_ts_family_one_roll.R
#
# Purpose:
# - compare the full Thompson family plus equal allocation on one opening;
# - show every direct run function explicitly in the file; and
# - save both high-level budget plots and per-method trace/allocation plots.
#
# Package functions demonstrated here:
# - bg_ts_run()
# - bg_ttts_run()
# - bg_multi_sample_ts_run()
# - bg_soft_elimination_ts_run()
# - bg_forced_exploration_ts_run()
# - bg_top_k_ts_run()
# - bg_equal_run()
#
# Relevant implementation files:
# - R/bg_algorithms.R
# - R/bg_metrics.R
# - R/bg_plots.R
# - src/policy_ts.cpp
# - src/policy_ttts.cpp
# - src/policy_multi_sample.cpp
# - src/policy_soft_elimination.cpp
# - src/policy_forced_exploration.cpp
# - src/policy_top_k.cpp
# - src/policy_equal.cpp
# - src/model_beta_bernoulli.cpp
# - src/alloc_core.cpp

source(if (file.exists("DESCRIPTION")) "results/00_results_common.R" else "00_results_common.R")

repo_root <- results_repo_root()
results_load_package(repo_root)

# Change these settings to rerun the same analysis on another opening or stack.
roll <- "1-6"
stack <- "beta_bernoulli"
budget <- 3072L
checkpoints <- c(
  16L, 24L, 32L, 48L, 64L, 80L, 96L,
  128L, 160L, 192L, 256L, 320L, 384L, 512L,
  640L, 768L, 1024L, 1280L, 1536L, 2048L,
  2560L, 3072L
)

master_truth <- results_load_master_truth(repo_root, roll = roll)
stack_spec <- results_stack_spec(stack)
truth <- results_project_truth(master_truth, stack = stack)

truth_overview <- results_truth_overview(master_truth, truth, roll, stack_spec$label)
truth_table <- results_truth_action_table(truth, top_n = 10L)

# Every direct method call is visible here so the script itself demonstrates the
# package API.
fit_ts <- bg_ts_run(
  problem = truth$problem,
  budget = budget,
  checkpoints = checkpoints,
  proxy_reference = truth$reference,
  seed = 1L
)

fit_ttts <- bg_ttts_run(
  problem = truth$problem,
  budget = budget,
  checkpoints = checkpoints,
  proxy_reference = truth$reference,
  seed = 1L
)

fit_multi <- bg_multi_sample_ts_run(
  problem = truth$problem,
  budget = budget,
  checkpoints = checkpoints,
  proxy_reference = truth$reference,
  seed = 1L
)

fit_soft <- bg_soft_elimination_ts_run(
  problem = truth$problem,
  budget = budget,
  checkpoints = checkpoints,
  proxy_reference = truth$reference,
  seed = 1L
)

fit_forced <- bg_forced_exploration_ts_run(
  problem = truth$problem,
  budget = budget,
  checkpoints = checkpoints,
  proxy_reference = truth$reference,
  seed = 1L
)

fit_top_k <- bg_top_k_ts_run(
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

runs <- list(
  thompson = fit_ts,
  top_two_thompson = fit_ttts,
  multi_sample_thompson = fit_multi,
  soft_elimination_thompson = fit_soft,
  forced_exploration_thompson = fit_forced,
  top_k_thompson = fit_top_k,
  equal = fit_equal
)
methods <- names(runs)

checkpoint_list <- lapply(runs, results_collect_run_panels, truth = truth, checkpoints = checkpoints)
checkpoint_summary <- do.call(rbind, lapply(checkpoint_list, `[[`, "checkpoint_metrics"))
rownames(checkpoint_summary) <- NULL

final_summary <- do.call(
  rbind,
  Map(results_final_summary, names(checkpoint_list), lapply(checkpoint_list, `[[`, "checkpoint_metrics"))
)
rownames(final_summary) <- NULL
final_summary <- final_summary[order(-final_summary$top1_match, final_summary$simple_regret), , drop = FALSE]

recommendation_path_df <- checkpoint_summary[, c(
  "allocation_policy",
  "checkpoint",
  "selected_reference_rank",
  "recommended_move_label"
)]
recommendation_path_df$method <- factor(
  results_method_label(recommendation_path_df$allocation_policy),
  levels = results_method_label(methods)
)

truth_rank_lookup <- truth$reference$action_table[, c("move_label", "rank"), drop = FALSE]
names(truth_rank_lookup)[[2L]] <- "truth_rank"

allocation_heatmap_df <- do.call(
  rbind,
  lapply(
    names(runs),
    function(method) {
      action_table <- runs[[method]]$action_table
      out <- merge(
        action_table[, c("move_label", "allocation_count"), drop = FALSE],
        truth_rank_lookup,
        by = "move_label",
        all.x = TRUE,
        sort = FALSE
      )
      out$allocation_policy <- method
      out$allocation_share <- out$allocation_count / budget
      out
    }
  )
)
allocation_heatmap_df <- allocation_heatmap_df[allocation_heatmap_df$truth_rank <= min(8L, nrow(truth_rank_lookup)), , drop = FALSE]
allocation_heatmap_df$method <- factor(
  results_method_label(allocation_heatmap_df$allocation_policy),
  levels = results_method_label(methods)
)
allocation_heatmap_df$move_label <- factor(
  allocation_heatmap_df$move_label,
  levels = truth_rank_lookup$move_label[order(truth_rank_lookup$truth_rank)][seq_len(min(8L, nrow(truth_rank_lookup)))]
)

truth_plot <- plot_bg_truth(truth) +
  ggplot2::labs(
    title = paste(stack_spec$label, "truth for opening", roll),
    subtitle = "All methods in this file are judged against this same proxy truth."
  )

# These plots are the high-level story:
# - top1_match: shown as a checkpoint heatmap so method differences stay legible
#   when many algorithms are overlaid
# - truth_top2_hit: a softer correctness view that asks whether the method is at
#   least staying inside the real contender set
# - selected_reference_rank: how close is each method to the truth-best move,
#   with posterior confidence folded into the path points
# - simple_regret: cost of mistakes
# - spearman: ranking recovery
# - share_best_truth / share_top2_truth: whether the budget goes to the moves
#   that matter most
# - share_mc_screened_suboptimal: how much budget is still going to moves that
#   Monte Carlo evidence already screens out
# - gap_weighted_wasted_allocation: how much budget was wasted on weak moves

top1_plot <- results_plot_top1_heatmap(checkpoint_summary, paste("TS family on opening", roll), "Checkpoint heatmap: did the current recommendation match the truth-best move?", methods = methods)
top2_hit_plot <- results_plot_truth_top2_heatmap(checkpoint_summary, paste("TS family on opening", roll), "Checkpoint heatmap: was the recommendation at least in the truth top-2?", methods = methods)
rank_path_plot <- results_plot_rank_confidence(checkpoint_summary, paste("TS family on opening", roll), "Truth rank of the recommended move over budget. Lower is better, and point fill shows posterior confidence.", methods = methods)
regret_plot <- results_plot_checkpoint_lines(checkpoint_summary, "simple_regret", paste("TS family on opening", roll), "Simple regret over budget. Lower is better.", "simple_regret", methods = methods)
spearman_plot <- results_plot_checkpoint_lines(checkpoint_summary, "spearman", paste("TS family on opening", roll), "Spearman rank correlation over budget.", "spearman", methods = methods)
share_best_plot <- results_plot_checkpoint_lines(checkpoint_summary, "share_best_truth", paste("TS family on opening", roll), "Share of budget placed on the single truth-best move.", "share_best_truth", methods = methods)
focus_plot <- results_plot_checkpoint_lines(checkpoint_summary, "share_top2_truth", paste("TS family on opening", roll), "Share of budget placed on the truth top-2 moves.", "share_top2_truth", methods = methods)
screened_plot <- results_plot_checkpoint_lines(checkpoint_summary, "share_mc_screened_suboptimal", paste("TS family on opening", roll), "Share of budget still going to MC-screened suboptimal moves. Lower is better.", "share_mc_screened_suboptimal", methods = methods)
waste_plot <- results_plot_checkpoint_lines(checkpoint_summary, "gap_weighted_wasted_allocation", paste("TS family on opening", roll), "Gap-weighted wasted allocation over budget. Lower is better.", "gap_weighted_wasted_allocation", methods = methods)
confidence_plot <- results_plot_confidence_heatmap(checkpoint_summary, paste("TS family on opening", roll), "Posterior confidence at each checkpoint. Higher values mean the model believes the current recommendation is more likely to be best.", methods = methods)
confidence_regret_plot <- results_plot_confidence_regret_scatter(checkpoint_summary, paste("Confidence versus regret on opening", roll), "Selected checkpoints show whether higher posterior confidence coincides with lower regret.", methods = methods)
timeline_plot <- results_plot_recommended_timeline(checkpoint_summary, paste("Recommended-move timeline on opening", roll), "Tiles show which move each method recommends at each checkpoint; darker tiles have better truth rank.", methods = methods)

final_regret_plot <- results_plot_final_metric_bars(
  final_summary,
  metric = "simple_regret",
  title = paste("Final simple regret by method on opening", roll),
  subtitle = "Lower bars are better.",
  methods = methods,
  y_label = "simple_regret"
)

# This heatmap shows how each method distributes its final budget across the
# top truth-ranked moves. It is one of the best "how are they working?"
# views because it makes the allocation pattern concrete.
allocation_heatmap_plot <- ggplot2::ggplot(
  allocation_heatmap_df,
  ggplot2::aes(x = move_label, y = method, fill = allocation_share)
) +
  ggplot2::geom_tile(color = "white", linewidth = 0.25) +
  ggplot2::scale_fill_gradient(low = "#F4F1EA", high = "#0B4F6C") +
  ggplot2::labs(
    title = paste("Final allocation share by method on opening", roll),
    subtitle = "Rows are methods; columns are the truth top-ranked moves.",
    x = "Move label",
    y = NULL,
    fill = "Allocation\nshare"
  ) +
  results_plot_theme() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

# This plot summarizes the strategic tradeoff:
# higher focus on the truth top-2 is good; lower wasted allocation is good.
tradeoff_plot <- ggplot2::ggplot(
  final_summary,
  ggplot2::aes(
    x = share_top2_truth,
    y = gap_weighted_wasted_allocation,
    label = method,
    color = method
  )
) +
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_text(nudge_y = 0.0015, size = 3, show.legend = FALSE) +
  ggplot2::scale_color_manual(values = results_method_palette(methods)) +
  ggplot2::labs(
    title = paste("Focus versus waste tradeoff on opening", roll),
    subtitle = "Better methods move toward the lower-right: more budget on good moves, less wasted on weak moves.",
    x = "share_top2_truth",
    y = "gap_weighted_wasted_allocation",
    color = "Method"
  ) +
  results_plot_theme()

results_save_table(truth_overview, repo_root, paste0("08_ts_family_one_roll_truth_overview_", results_roll_tag(roll), "_", stack))
results_save_table(truth_table, repo_root, paste0("08_ts_family_one_roll_truth_table_", results_roll_tag(roll), "_", stack))
results_save_table(checkpoint_summary, repo_root, paste0("08_ts_family_one_roll_checkpoint_summary_", results_roll_tag(roll), "_", stack))
results_save_table(final_summary, repo_root, paste0("08_ts_family_one_roll_final_summary_", results_roll_tag(roll), "_", stack))
results_save_table(recommendation_path_df, repo_root, paste0("08_ts_family_one_roll_recommendation_path_", results_roll_tag(roll), "_", stack))
results_save_table(allocation_heatmap_df, repo_root, paste0("08_ts_family_one_roll_allocation_heatmap_", results_roll_tag(roll), "_", stack))

results_save_plot(truth_plot, repo_root, paste0("08_ts_family_one_roll_truth_", results_roll_tag(roll), "_", stack), width = 10, height = 7)
results_save_plot(top1_plot, repo_root, paste0("08_ts_family_one_roll_top1_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(top2_hit_plot, repo_root, paste0("08_ts_family_one_roll_top2_hit_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(rank_path_plot, repo_root, paste0("08_ts_family_one_roll_rank_path_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(regret_plot, repo_root, paste0("08_ts_family_one_roll_regret_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(spearman_plot, repo_root, paste0("08_ts_family_one_roll_spearman_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(share_best_plot, repo_root, paste0("08_ts_family_one_roll_share_best_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(focus_plot, repo_root, paste0("08_ts_family_one_roll_focus_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(screened_plot, repo_root, paste0("08_ts_family_one_roll_screened_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(waste_plot, repo_root, paste0("08_ts_family_one_roll_waste_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(confidence_plot, repo_root, paste0("08_ts_family_one_roll_confidence_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(confidence_regret_plot, repo_root, paste0("08_ts_family_one_roll_confidence_regret_", results_roll_tag(roll), "_", stack), width = 12, height = 8)
results_save_plot(timeline_plot, repo_root, paste0("08_ts_family_one_roll_timeline_", results_roll_tag(roll), "_", stack), width = 13, height = 6.5)
results_save_plot(final_regret_plot, repo_root, paste0("08_ts_family_one_roll_final_regret_", results_roll_tag(roll), "_", stack), width = 9, height = 6)
results_save_plot(allocation_heatmap_plot, repo_root, paste0("08_ts_family_one_roll_allocation_heatmap_", results_roll_tag(roll), "_", stack), width = 11, height = 6)
results_save_plot(tradeoff_plot, repo_root, paste0("08_ts_family_one_roll_tradeoff_", results_roll_tag(roll), "_", stack), width = 10, height = 6)

# Save one trace plot and one allocation plot per method so the visual
# differences between methods are easy to inspect after the script runs.
for (method in names(runs)) {
  trace_plot <- plot_bg_ts_trace(runs[[method]]) +
    ggplot2::labs(
      title = paste(results_method_label(method), "trace on opening", roll),
      subtitle = "Direct run output for this one method."
    )

  allocation_plot <- plot_bg_allocation(runs[[method]], truth = truth) +
    ggplot2::labs(
      title = paste(results_method_label(method), "allocation versus truth on opening", roll),
      subtitle = "Final allocation pattern against the same proxy truth."
    )

  results_save_plot(trace_plot, repo_root, paste0("08_ts_family_one_roll_trace_", method, "_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
  results_save_plot(allocation_plot, repo_root, paste0("08_ts_family_one_roll_allocation_", method, "_", results_roll_tag(roll), "_", stack), width = 9, height = 6)
}

# Which opening and truth object are we studying?
# These first tables anchor every later plot: they identify the opening,
# the truth-best move, the hardness of the decision, and the final method table.
print(truth_overview)
print(truth_table)
print(final_summary)

# What is the proxy truth for this opening?
# This plot shows the truth ranking itself, so every later method plot can be
# interpreted relative to the same reference ordering and gap structure.
# Question: what does the truth ranking itself look like?
print(truth_plot)

# Do the methods match the truth best move, or at least stay inside the truth top-2?
# These heatmaps answer the most direct recommendation questions first:
# did the method hit the winner, and if not, did it at least stay inside the
# real contender set?
# Question: does the current recommendation exactly match the truth-best move?
print(top1_plot)
# Question: even when it misses top-1, does it at least stay inside the true contender set?
print(top2_hit_plot)

# How costly are mistakes, how well is the ranking recovered, and where does the budget go?
# This group is the main algorithm-comparison block:
# - rank path: how close the recommendation stays to truth rank 1, while point
#   fill shows how confident the method is at the same checkpoints
# - regret: how costly the current recommendation is
# - Spearman: whether the overall ordering is being learned
# - share_best / share_top2: whether budget concentrates on the right moves
# - screened: how much budget still goes to moves that Monte Carlo evidence
#   already screens out
# - waste: whether budget is being thrown at clearly weak moves
# - allocation heatmap: the final allocation pattern across truth-ranked moves
# - tradeoff: a one-chart summary of focus versus waste
# Question: how close is each method's recommendation to truth rank 1, and how confident is it at the same checkpoints?
print(rank_path_plot)
# Question: how costly is the current recommendation if it is wrong?
print(regret_plot)
# Question: is the full ordering of moves being learned, not just the top move?
print(spearman_plot)
# Question: is budget concentrating on the single truth-best move?
print(share_best_plot)
# Question: is budget concentrating on the two moves that matter most?
print(focus_plot)
# Question: how much budget is still going to moves that Monte Carlo evidence already screens out?
print(screened_plot)
# Question: after weighting by how bad the moves really are, how much budget is being wasted?
print(waste_plot)
# Question: what does the final budget split across the top truth-ranked moves look like?
print(allocation_heatmap_plot)
# Question: which methods end up with the best focus-versus-waste tradeoff?
print(tradeoff_plot)

# Are the methods confident for good reasons, and when do their recommendations change?
# The confidence heatmap is kept as a diagnostic rather than a headline result.
# The scatter checks whether higher confidence is actually paired with lower
# regret, and the timeline makes recommendation switching concrete.
# Question: how confident is each method in its current recommendation at each checkpoint?
print(confidence_plot)
# Question: when confidence rises, is regret actually falling?
print(confidence_regret_plot)
# Question: when do the methods switch recommended moves over the run?
print(timeline_plot)

# What is the clean final-budget comparison?
# This bar chart is the simplest final answer if we only want one method
# comparison figure from the full run.
# Question: what is the simplest final-budget comparison if we only keep one chart?
print(final_regret_plot)


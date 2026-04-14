# 02_ts_vs_equal.R
#
# Purpose:
# - compare canonical Thompson sampling with equal allocation on one opening;
# - show the direct public method calls clearly in the file; and
# - save a compact truth table, run summaries, and the main high-level graphs.
#
# Package functions demonstrated here:
# - bg_truth_load()
# - bg_truth_project()
# - bg_ts_run()
# - bg_equal_run()
# - bg_eval_reference_aware()
# - bg_eval_rank()
# - bg_eval_allocation()
# - plot_bg_truth()
# - plot_bg_ts_trace()
# - plot_bg_rank_compare()
# - plot_bg_allocation()
#
# Relevant implementation files:
# - R/bg_algorithms.R
# - R/bg_metrics.R
# - R/bg_plots.R
# - src/policy_ts.cpp
# - src/policy_equal.cpp
# - src/model_beta_bernoulli.cpp
# - src/alloc_core.cpp

source(if (file.exists("DESCRIPTION")) "results/00_results_common.R" else "00_results_common.R")

repo_root <- results_repo_root()
results_load_package(repo_root)

# -------------------------------------------------------------------
# Configuration
# Change these four objects to rerun the same script on another opening
# or with another budget ladder.
# -------------------------------------------------------------------

roll <- "1-6"
stack <- "beta_bernoulli"
budget <- 3072L
checkpoints <- c(
  16L, 24L, 32L, 48L, 64L, 80L, 96L,
  128L, 160L, 192L, 256L, 320L, 384L, 512L,
  640L, 768L, 1024L, 1280L, 1536L, 2048L,
  2560L, 3072L
)

# -------------------------------------------------------------------
# Load one preserved master truth and project it into the chosen stack.
# The truth table below is the object against which both methods are judged.
# -------------------------------------------------------------------

master_truth <- results_load_master_truth(repo_root, roll = roll)
stack_spec <- results_stack_spec(stack)
truth <- results_project_truth(master_truth, stack = stack)

truth_overview <- results_truth_overview(master_truth, truth, roll, stack_spec$label)
truth_table <- results_truth_action_table(truth, top_n = 10L)

# -------------------------------------------------------------------
# Direct algorithm calls.
# These are the public front doors the package wants users to understand.
# -------------------------------------------------------------------

fit_ts <- bg_ts_run(
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

# -------------------------------------------------------------------
# Collect the main metric panels from each run.
# The checkpoint table is the high-level "what happened over budget?" object.
# -------------------------------------------------------------------

ts_panels <- results_collect_run_panels(fit_ts, truth, checkpoints)
equal_panels <- results_collect_run_panels(fit_equal, truth, checkpoints)

ts_checkpoint <- ts_panels$checkpoint_metrics
equal_checkpoint <- equal_panels$checkpoint_metrics

checkpoint_summary <- rbind(ts_checkpoint, equal_checkpoint)
rownames(checkpoint_summary) <- NULL

final_summary <- rbind(
  results_final_summary("thompson", ts_checkpoint),
  results_final_summary("equal", equal_checkpoint)
)

# -------------------------------------------------------------------
# Truth and run plots.
# These are the most important visual summaries for a one-opening comparison.
# -------------------------------------------------------------------

truth_plot <- plot_bg_truth(truth) +
  ggplot2::labs(
    title = paste(stack_spec$label, "truth for opening", roll),
    subtitle = "This is the proxy truth both methods are being judged against."
  )

ts_trace_plot <- plot_bg_ts_trace(fit_ts) +
  ggplot2::labs(
    title = paste("TS trace on opening", roll),
    subtitle = "Checkpoint-by-checkpoint view of the direct TS run."
  )

equal_trace_plot <- plot_bg_ts_trace(fit_equal) +
  ggplot2::labs(
    title = paste("Equal trace on opening", roll),
    subtitle = "Checkpoint-by-checkpoint view of the equal-allocation baseline."
  )

ts_rank_plot <- plot_bg_rank_compare(fit_ts, truth = truth) +
  ggplot2::labs(
    title = paste("TS ranking versus truth on opening", roll),
    subtitle = "Closer to the truth ordering is better."
  )

equal_rank_plot <- plot_bg_rank_compare(fit_equal, truth = truth) +
  ggplot2::labs(
    title = paste("Equal ranking versus truth on opening", roll),
    subtitle = "This shows how the equal baseline orders the same moves."
  )

ts_allocation_plot <- plot_bg_allocation(fit_ts, truth = truth) +
  ggplot2::labs(
    title = paste("TS allocation versus truth on opening", roll),
    subtitle = "This shows where TS placed the rollout budget."
  )

equal_allocation_plot <- plot_bg_allocation(fit_equal, truth = truth) +
  ggplot2::labs(
    title = paste("Equal allocation versus truth on opening", roll),
    subtitle = "Equal allocation spreads the budget by construction."
  )

# These checkpoint plots summarize the main concepts:
# - top1_match: shown as a checkpoint heatmap so the binary match/miss path is
#   readable without overlapping lines
# - truth_top2_hit: did the method at least stay inside the true contender set?
# - simple_regret: how costly was the mistake?
# - selected_reference_rank: how close is the recommendation to rank 1?
# - share_best_truth / share_top2_truth: did the budget move toward the truly good moves?
# - share_mc_screened_suboptimal: how much budget is still going to moves that
#   Monte Carlo evidence already screens out?
# - gap_weighted_wasted_allocation: how much effort was wasted on weak moves?
# - spearman: how well did the method recover the full ranking?

top1_plot <- results_plot_top1_heatmap(
  checkpoint_summary,
  title = paste("TS versus Equal on opening", roll),
  subtitle = "Checkpoint heatmap: did the current recommendation match the truth-best move?",
  methods = c("thompson", "equal")
)

top2_hit_plot <- results_plot_truth_top2_heatmap(
  checkpoint_summary,
  title = paste("TS versus Equal on opening", roll),
  subtitle = "Checkpoint heatmap: was the recommendation at least in the truth top-2?",
  methods = c("thompson", "equal")
)

rank_plot <- results_plot_rank_confidence(
  checkpoint_summary,
  title = paste("TS versus Equal on opening", roll),
  subtitle = "Truth rank of the recommended move over budget. Lower is better, and point fill shows posterior confidence.",
  methods = c("thompson", "equal")
)

regret_plot <- results_plot_checkpoint_lines(
  checkpoint_summary,
  metric = "simple_regret",
  title = paste("TS versus Equal on opening", roll),
  subtitle = "Simple regret over budget. Lower is better.",
  y_label = "simple_regret",
  methods = c("thompson", "equal")
)

spearman_plot <- results_plot_checkpoint_lines(
  checkpoint_summary,
  metric = "spearman",
  title = paste("TS versus Equal on opening", roll),
  subtitle = "Spearman rank correlation over budget. Closer to 1 is better.",
  y_label = "spearman",
  methods = c("thompson", "equal")
)

share_best_plot <- results_plot_checkpoint_lines(
  checkpoint_summary,
  metric = "share_best_truth",
  title = paste("TS versus Equal on opening", roll),
  subtitle = "Share of budget placed on the truth-best move.",
  y_label = "share_best_truth",
  methods = c("thompson", "equal")
)

focus_plot <- results_plot_checkpoint_lines(
  checkpoint_summary,
  metric = "share_top2_truth",
  title = paste("TS versus Equal on opening", roll),
  subtitle = "Share of budget placed on the truth top-2 moves.",
  y_label = "share_top2_truth",
  methods = c("thompson", "equal")
)

screened_plot <- results_plot_checkpoint_lines(
  checkpoint_summary,
  metric = "share_mc_screened_suboptimal",
  title = paste("TS versus Equal on opening", roll),
  subtitle = "Share of budget still going to MC-screened suboptimal moves. Lower is better.",
  y_label = "share_mc_screened_suboptimal",
  methods = c("thompson", "equal")
)

waste_plot <- results_plot_checkpoint_lines(
  checkpoint_summary,
  metric = "gap_weighted_wasted_allocation",
  title = paste("TS versus Equal on opening", roll),
  subtitle = "Gap-weighted wasted allocation over budget. Lower is better.",
  y_label = "gap_weighted_wasted_allocation",
  methods = c("thompson", "equal")
)

confidence_plot <- results_plot_confidence_heatmap(
  checkpoint_summary,
  title = paste("TS versus Equal on opening", roll),
  subtitle = "Posterior confidence at each checkpoint. Higher values mean the model believes the current recommendation is more likely to be best.",
  methods = c("thompson", "equal")
)

confidence_regret_plot <- results_plot_confidence_regret_scatter(
  checkpoint_summary,
  title = paste("Confidence versus regret on opening", roll),
  subtitle = "Selected checkpoints show whether higher posterior confidence coincides with lower regret.",
  methods = c("thompson", "equal")
)

timeline_plot <- results_plot_recommended_timeline(
  checkpoint_summary,
  title = paste("Recommended-move timeline on opening", roll),
  subtitle = "Tiles show which move each method recommends at each checkpoint; darker tiles have better truth rank.",
  methods = c("thompson", "equal")
)

final_regret_plot <- results_plot_final_metric_bars(
  final_summary,
  metric = "simple_regret",
  title = paste("Final simple regret on opening", roll),
  subtitle = "This is the final-budget comparison in one bar chart.",
  methods = c("thompson", "equal"),
  y_label = "simple_regret"
)

results_save_table(truth_overview, repo_root, paste0("02_ts_vs_equal_truth_overview_", results_roll_tag(roll), "_", stack))
results_save_table(truth_table, repo_root, paste0("02_ts_vs_equal_truth_table_", results_roll_tag(roll), "_", stack))
results_save_table(checkpoint_summary, repo_root, paste0("02_ts_vs_equal_checkpoint_summary_", results_roll_tag(roll), "_", stack))
results_save_table(final_summary, repo_root, paste0("02_ts_vs_equal_final_summary_", results_roll_tag(roll), "_", stack))
results_save_study(
  list(
    truth = truth,
    fit_ts = fit_ts,
    fit_equal = fit_equal,
    checkpoint_summary = checkpoint_summary,
    final_summary = final_summary
  ),
  repo_root,
  paste0("02_ts_vs_equal_study_", results_roll_tag(roll), "_", stack)
)

results_save_plot(truth_plot, repo_root, paste0("02_ts_vs_equal_truth_", results_roll_tag(roll), "_", stack), width = 10, height = 7)
results_save_plot(ts_trace_plot, repo_root, paste0("02_ts_vs_equal_trace_ts_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(equal_trace_plot, repo_root, paste0("02_ts_vs_equal_trace_equal_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(ts_rank_plot, repo_root, paste0("02_ts_vs_equal_rank_ts_", results_roll_tag(roll), "_", stack), width = 9, height = 6)
results_save_plot(equal_rank_plot, repo_root, paste0("02_ts_vs_equal_rank_equal_", results_roll_tag(roll), "_", stack), width = 9, height = 6)
results_save_plot(ts_allocation_plot, repo_root, paste0("02_ts_vs_equal_allocation_ts_", results_roll_tag(roll), "_", stack), width = 9, height = 6)
results_save_plot(equal_allocation_plot, repo_root, paste0("02_ts_vs_equal_allocation_equal_", results_roll_tag(roll), "_", stack), width = 9, height = 6)
results_save_plot(top1_plot, repo_root, paste0("02_ts_vs_equal_top1_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(top2_hit_plot, repo_root, paste0("02_ts_vs_equal_top2_hit_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(rank_plot, repo_root, paste0("02_ts_vs_equal_rank_path_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(regret_plot, repo_root, paste0("02_ts_vs_equal_regret_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(spearman_plot, repo_root, paste0("02_ts_vs_equal_spearman_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(share_best_plot, repo_root, paste0("02_ts_vs_equal_share_best_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(focus_plot, repo_root, paste0("02_ts_vs_equal_focus_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(screened_plot, repo_root, paste0("02_ts_vs_equal_screened_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(waste_plot, repo_root, paste0("02_ts_vs_equal_waste_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(confidence_plot, repo_root, paste0("02_ts_vs_equal_confidence_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(confidence_regret_plot, repo_root, paste0("02_ts_vs_equal_confidence_regret_", results_roll_tag(roll), "_", stack), width = 10, height = 6)
results_save_plot(timeline_plot, repo_root, paste0("02_ts_vs_equal_timeline_", results_roll_tag(roll), "_", stack), width = 11, height = 6)
results_save_plot(final_regret_plot, repo_root, paste0("02_ts_vs_equal_final_regret_", results_roll_tag(roll), "_", stack), width = 8, height = 5.5)

# Which opening and truth object are we studying?
print(truth_overview)
print(truth_table)
print(final_summary)

# What is the proxy truth and how do the direct run traces look?
#
# Question: what does the truth ranking itself look like?
print(truth_plot)
#
# Question: how does the direct TS run evolve over the checkpoint ladder?
print(ts_trace_plot)
#
# Question: how does the equal-allocation baseline evolve over the same ladder?
print(equal_trace_plot)

# How do the methods rank and allocate rollouts across moves?
#
# Question: what ranking does TS recover relative to the proxy truth?
print(ts_rank_plot)
#
# Question: what ranking does Equal recover relative to the proxy truth?
print(equal_rank_plot)
#
# Question: where does TS actually spend its rollout budget?
print(ts_allocation_plot)
#
# Question: where does Equal spend its rollout budget?
print(equal_allocation_plot)

# Does the recommendation match the truth best move or at least stay in the top-2?
#
# Question: does the current recommendation exactly match the truth-best move?
print(top1_plot)
#
# Question: even when it misses top-1, does it at least stay in the true contender set?
print(top2_hit_plot)

# How costly are mistakes, how close is the recommendation to rank 1, and how well is the full ranking recovered?
# The rank path also carries posterior confidence through point fill, so it
# shows recommendation quality and model certainty in one graph.
#
# Question: how close is the current recommendation to truth rank 1, and how confident is the model at the same checkpoint?
print(rank_plot)
#
# Question: how costly is the current recommendation if it is wrong?
print(regret_plot)
#
# Question: is the full ordering of moves being recovered, not just the top move?
print(spearman_plot)
#
# Question: is budget concentrating on the single truth-best move?
print(share_best_plot)
#
# Question: is budget concentrating on the two moves that matter most?
print(focus_plot)
#
# Question: how much budget is still going to moves that Monte Carlo evidence already screens out?
print(screened_plot)
#
# Question: after weighting by how bad the moves really are, how much budget is being wasted?
print(waste_plot)

# Does posterior confidence line up with lower regret, and when does the recommendation itself change?
#
# Question: how confident is the posterior in the current recommendation at each checkpoint?
print(confidence_plot)
#
# Question: when confidence rises, is regret actually falling?
print(confidence_regret_plot)
#
# Question: when does each method switch its recommended move over the run?
print(timeline_plot)

# What is the clean final-budget comparison?
#
# Question: what is the simplest final-budget head-to-head comparison?
print(final_regret_plot)


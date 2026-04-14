# 12_in_game_board_ts.R
#
# Purpose:
# - run a direct Thompson-sampling study on one custom in-game board;
# - show the full workflow outside the opening-roll cache path; and
# - make it easy to change the board, roll, truth budget, and run budget.
#
# Package functions demonstrated here:
# - bg_board()
# - bg_roll()
# - bg_problem()
# - bg_truth_state()
# - bg_ts_run()
# - bg_eval_reference_aware()
# - bg_eval_rank()
# - bg_eval_allocation()
# - plot_bg_truth()
# - plot_bg_ts_trace()
# - plot_bg_rank_compare()
# - plot_bg_allocation()
#
# Relevant implementation files:
# - R/bg_problem.R
# - R/bg_truth.R
# - R/bg_algorithms.R
# - R/bg_metrics.R
# - R/bg_plots.R
# - src/truth_proxy.cpp
# - src/policy_ts.cpp
# - src/model_beta_bernoulli.cpp
# - src/alloc_core.cpp

source(if (file.exists("DESCRIPTION")) "results/00_results_common.R" else "00_results_common.R")

repo_root <- results_repo_root()
results_load_package(repo_root)
library(ggplot2)

# -------------------------------------------------------------------
# Configuration
# Change this board constructor, the realized roll, or the two budgets to run
# the same workflow on another in-game state.
# -------------------------------------------------------------------

build_demo_midgame_board <- function() {
  points <- integer(24)
  points[c(7, 8, 10, 13)] <- c(2L, 2L, 1L, 2L)
  points[c(18, 17, 15, 12)] <- -c(2L, 2L, 1L, 2L)

  bg_board(
    points = points,
    off = c(8L, 8L),
    turn = 1L
  )
}

board <- build_demo_midgame_board()
roll <- bg_roll(5, 3)
roll_label <- results_roll_label(roll)

# `truth_budget` controls how much Monte Carlo effort is spent building the
# proxy truth for this custom board. This file does not use the opening cache,
# because the board is not one of the preserved opening states.
truth_budget <- 4096L

# `budget` and `checkpoints` control the direct Thompson run on that same board.
budget <- results_run_budget()
checkpoints <- results_checkpoint_grid(budget)

# -------------------------------------------------------------------
# Build one local decision problem and one local proxy truth.
# The statistical stack here matches the main win/loss opening scripts.
# -------------------------------------------------------------------

problem <- bg_problem(
  state = board,
  roll = roll,
  reward_model = "win_loss",
  posterior_model = "beta_bernoulli",
  unresolved_value = 0,
  problem_id = "midgame_demo_5_3"
)

truth <- bg_truth_state(
  problem = problem,
  budget = truth_budget,
  parallel = FALSE,
  cache = FALSE,
  seed = 1L
)

truth_summary <- truth$reference$summary[1, , drop = FALSE]
truth_table <- results_truth_action_table(truth, top_n = min(14L, nrow(truth$reference$action_table)))
candidate_table <- truth$problem$candidate_table[
  ,
  intersect(
    c(
      "candidate_index",
      "move_label",
      "n_equivalent_sequences",
      "n_steps",
      "n_hits",
      "n_bar_entries",
      "n_bear_off",
      "total_step_distance"
    ),
    names(truth$problem$candidate_table)
  ),
  drop = FALSE
]

board_summary <- data.frame(
  board_label = "custom_midgame_demo",
  roll = roll_label,
  truth_budget = truth_budget,
  run_budget = budget,
  n_checkpoints = length(checkpoints),
  n_legal_sequences = length(problem$legal_moves),
  n_collapsed_candidates = nrow(problem$candidate_table),
  truth_best_move = truth$reference$summary$proxy_reference_best_move_label[[1L]],
  top_two_gap_estimate = truth_summary$top_two_gap_estimate[[1L]],
  difficulty_label = truth_summary$difficulty_label[[1L]],
  stringsAsFactors = FALSE
)

# -------------------------------------------------------------------
# Direct Thompson call.
# This is the main public algorithm front door for the in-game-board example.
# -------------------------------------------------------------------

fit_ts <- bg_ts_run(
  problem = truth$problem,
  budget = budget,
  checkpoints = checkpoints,
  proxy_reference = truth$reference,
  seed = 1L
)

ts_panels <- results_collect_run_panels(fit_ts, truth, checkpoints)
checkpoint_summary <- ts_panels$checkpoint_metrics
final_summary <- results_final_summary("thompson", checkpoint_summary)

# -------------------------------------------------------------------
# High-level plots for the custom in-game board.
# -------------------------------------------------------------------

truth_plot <- plot_bg_truth(truth) +
  ggplot2::labs(
    title = paste("Proxy truth for custom in-game board under roll", roll_label),
    subtitle = "This truth is freshly built for the custom board rather than loaded from the opening cache."
  )

ts_trace_plot <- plot_bg_ts_trace(fit_ts) +
  ggplot2::labs(
    title = paste("TS trace on custom in-game board under roll", roll_label),
    subtitle = "Checkpoint-by-checkpoint view of the Thompson run on this board."
  )

ts_rank_plot <- plot_bg_rank_compare(fit_ts, truth = truth) +
  ggplot2::labs(
    title = paste("TS ranking versus truth on custom in-game board under roll", roll_label),
    subtitle = "Closer to the truth ordering is better."
  )

ts_allocation_plot <- plot_bg_allocation(fit_ts, truth = truth) +
  ggplot2::labs(
    title = paste("TS allocation versus truth on custom in-game board under roll", roll_label),
    subtitle = "This shows where Thompson placed its rollout budget."
  )

top1_plot <- results_plot_top1_heatmap(
  checkpoint_summary,
  title = paste("TS top-1 match on custom in-game board under roll", roll_label),
  subtitle = "Checkpoint heatmap: did the current recommendation match the truth-best move?",
  methods = "thompson"
)

top2_hit_plot <- results_plot_truth_top2_heatmap(
  checkpoint_summary,
  title = paste("TS truth top-2 hit on custom in-game board under roll", roll_label),
  subtitle = "Checkpoint heatmap: was the recommendation at least in the truth top-2?",
  methods = "thompson"
)

rank_plot <- results_plot_rank_confidence(
  checkpoint_summary,
  title = paste("TS selected truth rank on custom in-game board under roll", roll_label),
  subtitle = "Truth rank of the recommended move over budget. Lower is better, and point fill shows posterior confidence.",
  methods = "thompson"
)

regret_plot <- results_plot_checkpoint_lines(
  checkpoint_summary,
  metric = "simple_regret",
  title = paste("TS simple regret on custom in-game board under roll", roll_label),
  subtitle = "Simple regret over budget. Lower is better.",
  y_label = "simple_regret",
  methods = "thompson"
)

spearman_plot <- results_plot_checkpoint_lines(
  checkpoint_summary,
  metric = "spearman",
  title = paste("TS Spearman rank recovery on custom in-game board under roll", roll_label),
  subtitle = "Spearman rank correlation over budget. Closer to 1 is better.",
  y_label = "spearman",
  methods = "thompson"
)

share_best_plot <- results_plot_checkpoint_lines(
  checkpoint_summary,
  metric = "share_best_truth",
  title = paste("TS share on truth-best move under roll", roll_label),
  subtitle = "Share of budget placed on the single truth-best move.",
  y_label = "share_best_truth",
  methods = "thompson"
)

focus_plot <- results_plot_checkpoint_lines(
  checkpoint_summary,
  metric = "share_top2_truth",
  title = paste("TS focus on truth top-2 moves under roll", roll_label),
  subtitle = "Share of budget placed on the truth top-2 moves.",
  y_label = "share_top2_truth",
  methods = "thompson"
)

screened_plot <- results_plot_checkpoint_lines(
  checkpoint_summary,
  metric = "share_mc_screened_suboptimal",
  title = paste("TS budget on MC-screened weak moves under roll", roll_label),
  subtitle = "Share of budget still going to MC-screened suboptimal moves. Lower is better.",
  y_label = "share_mc_screened_suboptimal",
  methods = "thompson"
)

waste_plot <- results_plot_checkpoint_lines(
  checkpoint_summary,
  metric = "gap_weighted_wasted_allocation",
  title = paste("TS wasted allocation under roll", roll_label),
  subtitle = "Gap-weighted wasted allocation over budget. Lower is better.",
  y_label = "gap_weighted_wasted_allocation",
  methods = "thompson"
)

confidence_plot <- results_plot_confidence_heatmap(
  checkpoint_summary,
  title = paste("TS posterior confidence under roll", roll_label),
  subtitle = "Posterior confidence at each checkpoint. Higher values mean the model believes the current recommendation is more likely to be best.",
  methods = "thompson"
)

confidence_regret_plot <- results_plot_confidence_regret_scatter(
  checkpoint_summary,
  title = paste("TS confidence versus regret under roll", roll_label),
  subtitle = "Selected checkpoints show whether higher posterior confidence coincides with lower regret.",
  methods = "thompson"
)

timeline_plot <- results_plot_recommended_timeline(
  checkpoint_summary,
  title = paste("TS recommended-move timeline under roll", roll_label),
  subtitle = "Tiles show which move TS recommends at each checkpoint; darker tiles have better truth rank.",
  methods = "thompson"
)

# -------------------------------------------------------------------
# Save tables, plots, and the full study object.
# -------------------------------------------------------------------

results_save_table(board_summary, repo_root, "12_in_game_board_ts_board_summary")
results_save_table(candidate_table, repo_root, "12_in_game_board_ts_candidate_table")
results_save_table(truth_table, repo_root, "12_in_game_board_ts_truth_table")
results_save_table(checkpoint_summary, repo_root, "12_in_game_board_ts_checkpoint_summary")
results_save_table(final_summary, repo_root, "12_in_game_board_ts_final_summary")

results_save_study(
  list(
    board = board,
    roll = roll,
    problem = problem,
    truth = truth,
    fit_ts = fit_ts,
    checkpoint_summary = checkpoint_summary,
    final_summary = final_summary
  ),
  repo_root,
  "12_in_game_board_ts_study"
)

results_save_plot(truth_plot, repo_root, "12_in_game_board_ts_truth", width = 10, height = 7)
results_save_plot(ts_trace_plot, repo_root, "12_in_game_board_ts_trace", width = 10, height = 6)
results_save_plot(ts_rank_plot, repo_root, "12_in_game_board_ts_rank", width = 9, height = 6)
results_save_plot(ts_allocation_plot, repo_root, "12_in_game_board_ts_allocation", width = 9, height = 6)
results_save_plot(top1_plot, repo_root, "12_in_game_board_ts_top1", width = 10, height = 5.5)
results_save_plot(top2_hit_plot, repo_root, "12_in_game_board_ts_top2_hit", width = 10, height = 5.5)
results_save_plot(rank_plot, repo_root, "12_in_game_board_ts_rank_path", width = 10, height = 6)
results_save_plot(regret_plot, repo_root, "12_in_game_board_ts_regret", width = 10, height = 6)
results_save_plot(spearman_plot, repo_root, "12_in_game_board_ts_spearman", width = 10, height = 6)
results_save_plot(share_best_plot, repo_root, "12_in_game_board_ts_share_best", width = 10, height = 6)
results_save_plot(focus_plot, repo_root, "12_in_game_board_ts_focus", width = 10, height = 6)
results_save_plot(screened_plot, repo_root, "12_in_game_board_ts_screened", width = 10, height = 6)
results_save_plot(waste_plot, repo_root, "12_in_game_board_ts_waste", width = 10, height = 6)
results_save_plot(confidence_plot, repo_root, "12_in_game_board_ts_confidence", width = 10, height = 6)
results_save_plot(confidence_regret_plot, repo_root, "12_in_game_board_ts_confidence_regret", width = 10, height = 6)
results_save_plot(timeline_plot, repo_root, "12_in_game_board_ts_timeline", width = 12, height = 4.8)

# Which board, roll, and truth object are we studying?
# These tables identify the custom position, how many candidate actions it has,
# what the truth-best move is, and how hard the board looks under the local
# proxy truth.
print(board_summary)
print(candidate_table)
print(truth_table)
print(final_summary)

# What is the local proxy truth and how does the direct TS run evolve?
# The truth plot shows the target ordering and gap structure, while the trace
# plot shows how the direct TS run behaves over the chosen checkpoint ladder.
# Question: what does the proxy truth itself look like on this custom board?
print(truth_plot)
# Question: how does the direct TS run evolve over the checkpoint ladder?
print(ts_trace_plot)

# How well does TS rank and allocate its budget across the candidate moves?
# These plots make the one-run behavior concrete: estimated ranking, final
# allocation pattern, and whether budget is concentrating on the right moves.
# Question: what ranking does TS recover relative to the proxy truth?
print(ts_rank_plot)
# Question: where does TS actually spend its rollout budget?
print(ts_allocation_plot)

# Does the recommendation hit the truth-best move or at least stay in the top-2?
# These heatmaps give the clean discrete-correctness view across checkpoints.
# Question: does the current recommendation exactly match the truth-best move?
print(top1_plot)
# Question: even when it misses top-1, does it at least stay in the true contender set?
print(top2_hit_plot)

# How costly are mistakes, how close is the recommendation to rank 1, and how intelligently is budget being spent?
# This is the main metric block for the custom board: regret, ranking recovery,
# focus on the best moves, wasted allocation on weak moves, and posterior
# confidence directly inside the rank-path points.
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

# Is the model becoming confident for good reasons, and when does its recommendation change?
# Confidence is shown as a diagnostic, not as the main result. The scatter
# checks whether higher confidence lines up with lower regret, and the timeline
# shows when the recommended move itself changes.
# Question: how confident is the posterior in the current recommendation at each checkpoint?
print(confidence_plot)
# Question: when confidence rises, is regret actually falling?
print(confidence_regret_plot)
# Question: when does the method switch its recommended move over the run?
print(timeline_plot)

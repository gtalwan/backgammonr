# 00_start_here.R
#
# Purpose:
# - introduce the package story;
# - show where the main public functions live;
# - show where the preserved opening truth cache lives; and
# - write the package/presentation map used by the rest of the walkthrough.
#
# Main package functions used here:
# - bg_truth_load()                       [R/bg_truth.R] via Presentation helper
# - bg_truth_project()                    [R/bg_truth.R]
# - plot_bg_truth()                       [R/bg_plots.R]
#
# Relevant native files:
# - src/truth_proxy.cpp
# - src/metrics_summary.cpp

common_path <- if (file.exists("DESCRIPTION")) {
  file.path("Presentation", "presentation_common.R")
} else {
  "presentation_common.R"
}
source(common_path)

repo_root <- presentation_repo_root()
presentation_load_package(repo_root)

truth_cache_dir <- presentation_truth_cache_dir(repo_root)
opening_truths <- presentation_load_opening_truths(repo_root)
truth_index <- opening_truths$summary
master_reference_budget <- presentation_master_reference_budget(opening_truths)

package_map <- data.frame(
  layer = c(
    "Engine mechanics",
    "Problem and cached truth",
    "Algorithms",
    "Evaluation",
    "Plots",
    "Repeated studies"
  ),
  priority = c(
    "Optional",
    "Core",
    "Core",
    "Core",
    "Core",
    "Optional"
  ),
  functions = c(
    "bg_initial_board(), bg_roll(), bg_legal_moves(), bg_apply_move_sequence()",
    "bg_opening_problem(), bg_truth_load(), bg_truth_project()",
    "bg_ts_run(), bg_ttts_run(), bg_multi_sample_ts_run(), bg_soft_elimination_ts_run(), bg_forced_exploration_ts_run(), bg_top_k_ts_run(), bg_equal_run()",
    "bg_eval_reference_aware(), bg_ts_diagnostics()",
    "plot_bg_truth(), plot_bg_ts_trace(), plot_bg_rank_compare(), plot_bg_allocation(), plot_bg_budget_curve()",
    "bg_compare_algorithms()"
  ),
  r_files = c(
    "R/bg_engine_api.R",
    "R/bg_problem.R, R/bg_truth.R",
    "R/bg_algorithms.R",
    "R/bg_metrics.R",
    "R/bg_plots.R",
    "R/bg_studies.R"
  ),
  native_files = c(
    "src/bg_board.cpp, src/bg_movegen.cpp, src/bg_game.cpp",
    "src/truth_proxy.cpp, src/metrics_summary.cpp",
    "src/alloc_core.cpp, src/policy_ts.cpp, src/policy_ttts.cpp, src/policy_equal.cpp",
    "src/metrics_summary.cpp, src/rcpp_entrypoints.cpp",
    "R-only ggplot layer",
    "R-only wrapper around direct method runs"
  ),
  stringsAsFactors = FALSE
)

presentation_scripts <- data.frame(
  script = sprintf("%02d_%s.R", 0:14, c(
    "start_here",
    "truth_overview_openings",
    "one_opening_one_algorithm",
    "one_opening_ts_family",
    "one_opening_ts_vs_equal",
    "all_openings_ts_family",
    "model_sensitivity_baseline_ts",
    "metrics_and_diagnostics",
    "plot_gallery",
    "game_mechanics",
    "all_opening_rolls",
    "user_function_reference",
    "all_openings_ts_quick_contrasts",
    "student_t_ts_vs_equal",
    "dirichlet_ts_vs_equal"
  )),
  purpose = c(
    "Package map and cache map.",
    "Opening-truth cache overview and hardness visuals.",
    "One opening, one TS run, step by step.",
    "One opening, all Thompson-family methods via direct run functions.",
    "Clean TS versus equal allocation head-to-head.",
    "All 21 openings under the win/loss stack.",
    "Sensitivity of baseline TS to the three headline model stacks.",
    "Primary metrics and how to interpret them.",
    "Curated final plots saved for a talk or slide deck.",
    "Backgammon engine mechanics from board to legal moves to turn/game results.",
    "A clean pass through all 21 opening rolls and their best-move structure.",
    "A small starter API plus an explicit advanced appendix.",
    "A fast all-openings TS-versus-other-TS contrast script.",
    "Student-t TS versus equal allocation across the opening battery.",
    "Dirichlet TS versus equal allocation across the opening battery."
  ),
  stringsAsFactors = FALSE
)

cache_table <- truth_index[
  order(truth_index$die1, truth_index$die2),
  c(
    "opening_roll",
    "reference_budget",
    "top_two_gap_estimate",
    "mc_gap_excludes_zero",
    "difficulty_label",
    "reward_model",
    "posterior_model"
  )
]

truth_plot <- plot_bg_truth(opening_truths) +
  ggplot2::labs(
    title = "Opening truth gap ladder from the preserved cache",
    subtitle = paste(
      "These are the current cached opening master truths in cache/opening_truths_master.",
      "Each opening currently carries a reference budget of",
      format(master_reference_budget, big.mark = ","),
      "rollouts."
    )
  )

presentation_save_table(package_map, repo_root, "00_package_map")
presentation_save_table(presentation_scripts, repo_root, "00_presentation_scripts")
presentation_save_table(cache_table, repo_root, "00_truth_cache_index")
presentation_save_plot(truth_plot, repo_root, "00_truth_gap_ladder", width = 10, height = 7)

# This table is the package map for the talk. It is the quickest way to see
# which user-facing functions live in which R/native files.
print(package_map)

# This table is the script index for the walkthrough. It is meant to be read
# top to bottom as a coherent presentation path.
print(presentation_scripts)

# This cache table confirms that the walkthrough is using the preserved master
# truths rather than rebuilding or switching caches behind the scenes.
print(cache_table)

# This plot is the opening-truth "elevator view": hardness, separation, and
# cache coverage are all visible in one place.
print(truth_plot)

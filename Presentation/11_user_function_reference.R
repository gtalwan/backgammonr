# 11_user_function_reference.R
#
# Purpose:
# - give the user one small starter API instead of a flat list of exports;
# - show the real call signatures for those starter functions;
# - separate the optional/advanced functions into an appendix; and
# - end the presentation with a practical "what should I call first?" file.
#
# Main package functions used here:
# - the script looks up exported functions from the installed namespace so the
#   signatures match the current code rather than a hand-written approximation.
#
# Relevant R files:
# - R/bg_engine_api.R
# - R/bg_problem.R
# - R/bg_truth.R
# - R/bg_algorithms.R
# - R/bg_metrics.R
# - R/bg_plots.R
# - R/bg_studies.R

common_path <- if (file.exists("DESCRIPTION")) {
  file.path("Presentation", "presentation_common.R")
} else {
  "presentation_common.R"
}
source(common_path)

repo_root <- presentation_repo_root()
presentation_load_package(repo_root)
library(ggplot2)

collapse_formals <- function(fn_name) {
  fn <- getExportedValue("backgammonr", fn_name)
  sig_lines <- capture.output(args(fn))
  sig_lines <- sig_lines[nzchar(sig_lines) & sig_lines != "NULL"]
  sig_text <- paste(sig_lines, collapse = " ")
  sig_text <- sub("^function\\s*", paste0(fn_name), sig_text)
  sig_text
}

make_reference_row <- function(layer, tier, function_name, primary_use, r_file, native_files) {
  data.frame(
    layer = layer,
    tier = tier,
    function_name = function_name,
    primary_use = primary_use,
    r_file = r_file,
    native_files = native_files,
    stringsAsFactors = FALSE
  )
}

core_function_reference <- do.call(
  rbind,
  list(
    make_reference_row(
      "Problem and cached truth",
      "Learn first",
      "bg_opening_problem",
      "Build the standard opening-roll local decision problem.",
      "R/bg_problem.R",
      "R-only constructor"
    ),
    make_reference_row(
      "Problem and cached truth",
      "Learn first",
      "bg_truth_load",
      "Load one preserved master truth file from disk.",
      "R/bg_truth.R",
      "R-only loader"
    ),
    make_reference_row(
      "Problem and cached truth",
      "Learn first",
      "bg_truth_project",
      "Project one master truth into the Beta-Bernoulli, Student-t, or Dirichlet stack.",
      "R/bg_truth.R",
      "src/model_beta_bernoulli.cpp / src/model_student_t.cpp / src/model_dirichlet_categorical.cpp"
    ),
    make_reference_row(
      "Algorithms",
      "Learn first",
      "bg_ts_run",
      "Run canonical Thompson sampling.",
      "R/bg_algorithms.R",
      "src/alloc_core.cpp / src/policy_ts.cpp"
    ),
    make_reference_row(
      "Algorithms",
      "Learn first",
      "bg_ttts_run",
      "Run top-two Thompson sampling.",
      "R/bg_algorithms.R",
      "src/alloc_core.cpp / src/policy_ttts.cpp"
    ),
    make_reference_row(
      "Algorithms",
      "Learn first",
      "bg_multi_sample_ts_run",
      "Run the multi-sample Thompson variant.",
      "R/bg_algorithms.R",
      "src/alloc_core.cpp / src/policy_ts.cpp"
    ),
    make_reference_row(
      "Algorithms",
      "Learn first",
      "bg_soft_elimination_ts_run",
      "Run the soft-elimination Thompson variant.",
      "R/bg_algorithms.R",
      "src/alloc_core.cpp / src/policy_ts.cpp"
    ),
    make_reference_row(
      "Algorithms",
      "Learn first",
      "bg_forced_exploration_ts_run",
      "Run the forced-exploration Thompson variant.",
      "R/bg_algorithms.R",
      "src/alloc_core.cpp / src/policy_ts.cpp"
    ),
    make_reference_row(
      "Algorithms",
      "Learn first",
      "bg_top_k_ts_run",
      "Run the top-k Thompson variant.",
      "R/bg_algorithms.R",
      "src/alloc_core.cpp / src/policy_ts.cpp"
    ),
    make_reference_row(
      "Algorithms",
      "Learn first",
      "bg_equal_run",
      "Run equal allocation as the baseline.",
      "R/bg_algorithms.R",
      "src/alloc_core.cpp / src/policy_equal.cpp"
    ),
    make_reference_row(
      "Evaluation",
      "Learn first",
      "bg_eval_reference_aware",
      "Build the main truth-aware evaluation panel for a run.",
      "R/bg_metrics.R",
      "src/metrics_summary.cpp"
    ),
    make_reference_row(
      "Evaluation",
      "Learn first",
      "bg_ts_diagnostics",
      "Bundle the main one-run diagnostics without dropping into many smaller helpers.",
      "R/bg_metrics.R",
      "src/metrics_summary.cpp"
    ),
    make_reference_row(
      "Plots",
      "Learn first",
      "plot_bg_truth",
      "Plot one truth object or one truth battery.",
      "R/bg_plots.R",
      "R-only ggplot layer"
    ),
    make_reference_row(
      "Plots",
      "Learn first",
      "plot_bg_ts_trace",
      "Plot the path of one method run over checkpoints.",
      "R/bg_plots.R",
      "R-only ggplot layer"
    ),
    make_reference_row(
      "Plots",
      "Learn first",
      "plot_bg_rank_compare",
      "Plot estimated ranking versus truth ranking.",
      "R/bg_plots.R",
      "R-only ggplot layer"
    ),
    make_reference_row(
      "Plots",
      "Learn first",
      "plot_bg_allocation",
      "Plot final allocation against the truth.",
      "R/bg_plots.R",
      "R-only ggplot layer"
    ),
    make_reference_row(
      "Plots",
      "Learn first",
      "plot_bg_budget_curve",
      "Plot method performance against budget in repeated studies.",
      "R/bg_plots.R",
      "R-only ggplot layer"
    ),
    make_reference_row(
      "Repeated studies",
      "Learn after the direct runs",
      "bg_compare_algorithms",
      "Convenience wrapper that repeatedly calls the direct run functions.",
      "R/bg_studies.R",
      "R-only wrapper over direct method runs"
    )
  )
)

advanced_function_reference <- do.call(
  rbind,
  list(
    make_reference_row(
      "Engine mechanics",
      "Optional / advanced",
      "bg_board",
      "Construct a board object directly.",
      "R/bg_engine_api.R",
      "src/bg_board.cpp"
    ),
    make_reference_row(
      "Engine mechanics",
      "Optional / advanced",
      "bg_initial_board",
      "Create the standard starting position.",
      "R/bg_engine_api.R",
      "src/bg_board.cpp"
    ),
    make_reference_row(
      "Engine mechanics",
      "Optional / advanced",
      "bg_roll",
      "Create one realized dice roll.",
      "R/bg_engine_api.R",
      "src/bg_board.cpp"
    ),
    make_reference_row(
      "Engine mechanics",
      "Optional / advanced",
      "bg_roll_dice",
      "Sample random rolls.",
      "R/bg_engine_api.R",
      "src/bg_board.cpp"
    ),
    make_reference_row(
      "Engine mechanics",
      "Optional / advanced",
      "bg_legal_moves",
      "Enumerate legal move sequences for a board and roll.",
      "R/bg_engine_api.R",
      "src/bg_movegen.cpp"
    ),
    make_reference_row(
      "Engine mechanics",
      "Optional / advanced",
      "bg_apply_move_sequence",
      "Apply one legal move sequence to a board.",
      "R/bg_engine_api.R",
      "src/bg_board.cpp"
    ),
    make_reference_row(
      "Engine mechanics",
      "Optional / advanced",
      "bg_board_features",
      "Summarize a board in interpretable features.",
      "R/bg_engine_api.R",
      "R-only wrapper over engine objects"
    ),
    make_reference_row(
      "Engine mechanics",
      "Optional / advanced",
      "bg_move_features",
      "Summarize candidate actions in interpretable features.",
      "R/bg_engine_api.R",
      "R-only wrapper over engine objects"
    ),
    make_reference_row(
      "Engine mechanics",
      "Optional / advanced",
      "bg_play_turn",
      "Simulate one turn under a chosen selection rule.",
      "R/bg_engine_api.R",
      "src/bg_game.cpp"
    ),
    make_reference_row(
      "Engine mechanics",
      "Optional / advanced",
      "bg_play_game",
      "Simulate a full game under a chosen selection rule.",
      "R/bg_engine_api.R",
      "src/bg_game.cpp"
    ),
    make_reference_row(
      "Truth building",
      "Optional / advanced",
      "bg_problem",
      "Build a custom local problem outside the standard opening workflow.",
      "R/bg_problem.R",
      "R-only constructor"
    ),
    make_reference_row(
      "Truth building",
      "Optional / advanced",
      "bg_opening_rolls",
      "List the 21 opening rolls used in the battery.",
      "R/bg_problem.R",
      "R-only lookup"
    ),
    make_reference_row(
      "Truth building",
      "Optional / advanced",
      "bg_reference",
      "Build or extend a high-budget proxy reference.",
      "R/bg_truth.R",
      "src/truth_proxy.cpp"
    ),
    make_reference_row(
      "Truth building",
      "Optional / advanced",
      "bg_truth_state",
      "Build a cached truth object from scratch.",
      "R/bg_truth.R",
      "src/truth_proxy.cpp"
    ),
    make_reference_row(
      "Truth building",
      "Optional / advanced",
      "bg_master_truth_state",
      "Build a reusable master truth from scratch.",
      "R/bg_truth.R",
      "src/truth_proxy.cpp"
    ),
    make_reference_row(
      "Truth building",
      "Optional / advanced",
      "bg_truth_battery",
      "Build a battery of truth objects from a list of problems.",
      "R/bg_truth.R",
      "src/truth_proxy.cpp"
    ),
    make_reference_row(
      "Truth building",
      "Optional / advanced",
      "bg_opening_truth_build_one",
      "Build one opening truth from scratch.",
      "R/bg_truth.R",
      "src/truth_proxy.cpp"
    ),
    make_reference_row(
      "Truth building",
      "Optional / advanced",
      "bg_opening_truth_build_all",
      "Build the full opening battery from scratch.",
      "R/bg_truth.R",
      "src/truth_proxy.cpp"
    ),
    make_reference_row(
      "Truth building",
      "Optional / advanced",
      "bg_opening_truth_load_one",
      "Load one cached opening truth through the convenience loader.",
      "R/bg_truth.R",
      "R-only loader"
    ),
    make_reference_row(
      "Truth building",
      "Optional / advanced",
      "bg_opening_truth_load_all",
      "Load the full cached opening battery through the convenience loader.",
      "R/bg_truth.R",
      "R-only loader"
    ),
    make_reference_row(
      "Truth building",
      "Optional / advanced",
      "bg_opening_truth_index",
      "Inspect the convenience opening-truth cache.",
      "R/bg_truth.R",
      "R-only loader"
    ),
    make_reference_row(
      "Truth building",
      "Optional / advanced",
      "bg_reference_project",
      "Project a proxy reference directly.",
      "R/bg_truth.R",
      "src/model_beta_bernoulli.cpp / src/model_student_t.cpp / src/model_dirichlet_categorical.cpp"
    ),
    make_reference_row(
      "Metrics",
      "Optional / advanced",
      "bg_eval_top1",
      "Compute the top-1 correctness view only.",
      "R/bg_metrics.R",
      "src/metrics_summary.cpp"
    ),
    make_reference_row(
      "Metrics",
      "Optional / advanced",
      "bg_eval_rank",
      "Compute the ranking view only.",
      "R/bg_metrics.R",
      "src/metrics_summary.cpp"
    ),
    make_reference_row(
      "Metrics",
      "Optional / advanced",
      "bg_eval_allocation",
      "Compute the allocation view only.",
      "R/bg_metrics.R",
      "src/metrics_summary.cpp"
    ),
    make_reference_row(
      "Metrics",
      "Optional / advanced",
      "bg_truth_diagnostics",
      "Summarize one truth object in detail.",
      "R/bg_metrics.R",
      "src/metrics_summary.cpp"
    ),
    make_reference_row(
      "Metrics",
      "Optional / advanced",
      "bg_truth_certify",
      "Label truth separation and difficulty.",
      "R/bg_metrics.R",
      "src/metrics_summary.cpp"
    ),
    make_reference_row(
      "Metrics",
      "Optional / advanced",
      "bg_truth_stability",
      "Rebuild a reference ladder to screen proxy-truth stability.",
      "R/bg_truth.R",
      "src/truth_proxy.cpp / src/metrics_summary.cpp"
    ),
    make_reference_row(
      "Studies and persistence",
      "Optional / advanced",
      "bg_opening_compare_study",
      "Convenience study wrapper for the opening battery.",
      "R/bg_studies.R",
      "R-only wrapper over direct method runs"
    ),
    make_reference_row(
      "Studies and persistence",
      "Optional / advanced",
      "bg_study_save",
      "Save a study object.",
      "R/bg_studies.R",
      "R-only persistence wrapper"
    ),
    make_reference_row(
      "Studies and persistence",
      "Optional / advanced",
      "bg_study_load",
      "Load a saved study object.",
      "R/bg_studies.R",
      "R-only persistence wrapper"
    ),
    make_reference_row(
      "Legacy baseline",
      "Optional / advanced",
      "bg_ucb_run",
      "Run the demoted UCB baseline.",
      "R/bg_algorithms.R",
      "src/alloc_core.cpp"
    ),
    make_reference_row(
      "Legacy baseline",
      "Optional / advanced",
      "bg_uniform_run",
      "Backward-compatibility alias for equal allocation.",
      "R/bg_algorithms.R",
      "src/policy_equal.cpp"
    )
  )
)

core_function_reference$signature <- vapply(
  core_function_reference$function_name,
  collapse_formals,
  character(1L)
)

advanced_function_reference$signature <- vapply(
  advanced_function_reference$function_name,
  collapse_formals,
  character(1L)
)

function_reference_all <- rbind(core_function_reference, advanced_function_reference)

workflow_table <- data.frame(
  step = 1:6,
  stage = c(
    "Name the opening problem",
    "Load one master truth",
    "Project to one model stack",
    "Run one direct method",
    "Evaluate and diagnose",
    "Plot or repeat"
  ),
  functions = c(
    "bg_opening_problem()",
    "bg_truth_load()",
    "bg_truth_project()",
    "bg_ts_run(), bg_ttts_run(), bg_multi_sample_ts_run(), bg_soft_elimination_ts_run(), bg_forced_exploration_ts_run(), bg_top_k_ts_run(), bg_equal_run()",
    "bg_eval_reference_aware(), bg_ts_diagnostics()",
    "plot_bg_truth(), plot_bg_ts_trace(), plot_bg_rank_compare(), plot_bg_allocation(), plot_bg_budget_curve(), bg_compare_algorithms()"
  ),
  why_this_step = c(
    "Start from the decision problem, not from raw board mechanics.",
    "Reuse the preserved cache instead of rebuilding truth.",
    "Choose the reward/posterior stack after loading the master truth.",
    "Treat the direct run functions as the real center of the package.",
    "Use the bundled diagnostics before dropping to narrower metric helpers.",
    "Plot the result first; only then move to repeated studies."
  ),
  stringsAsFactors = FALSE
)

core_layer_counts <- as.data.frame(table(core_function_reference$layer), stringsAsFactors = FALSE)
names(core_layer_counts) <- c("layer", "n_functions")

tier_counts <- as.data.frame(table(function_reference_all$tier), stringsAsFactors = FALSE)
names(tier_counts) <- c("tier", "n_functions")

advanced_layer_table <- as.data.frame(
  table(advanced_function_reference$layer),
  stringsAsFactors = FALSE
)
names(advanced_layer_table) <- c("layer", "n_functions")

workflow_segments <- data.frame(
  x = workflow_table$step[-nrow(workflow_table)],
  xend = workflow_table$step[-1L],
  y = 1,
  yend = 1
)

# This plot shows the intended starter surface. It is deliberately much smaller
# than the full export surface because the package should teach as one coherent
# workflow rather than a flat catalog.
core_layer_plot <- ggplot(
  core_layer_counts,
  aes(x = reorder(layer, n_functions), y = n_functions, fill = layer)
) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Starter API by workflow layer",
    subtitle = "The first-pass user story is intentionally narrow: cached truth, direct runs, bundled diagnostics, and a small plot layer.",
    x = NULL,
    y = "Functions in the starter API"
  ) +
  bg_plot_theme_research()

# This plot makes the condensation explicit: most exports are optional, but the
# package presentation should center on the starter API.
tier_plot <- ggplot(
  tier_counts,
  aes(x = tier, y = n_functions, fill = tier)
) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Starter API versus optional appendix",
    subtitle = "Learn the small core first. The rest is there when you need more control.",
    x = NULL,
    y = "Functions in this reference file"
  ) +
  bg_plot_theme_research()

# This workflow plot is the one-slide answer to "what order do I call things
# in?" It keeps the package centered on problem -> master truth -> projection ->
# direct run -> diagnostics -> plots/studies.
workflow_plot <- ggplot(
  workflow_table,
  aes(x = step, y = 1)
) +
  geom_segment(
    data = workflow_segments,
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    linewidth = 0.5,
    color = "#6C757D"
  ) +
  geom_point(size = 4, color = "#0072B2") +
  geom_label(aes(label = stage), nudge_y = 0.15, size = 3.2, label.size = 0.15) +
  scale_x_continuous(breaks = workflow_table$step) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(
    title = "Typical user workflow through the package",
    subtitle = "Read the direct run functions as the center of the workflow. Everything else is either setup, diagnostics, or optional convenience.",
    x = "Workflow step"
  ) +
  bg_plot_theme_research()

presentation_save_table(core_function_reference, repo_root, "11_user_function_reference_core")
presentation_save_table(advanced_function_reference, repo_root, "11_user_function_reference_advanced")
presentation_save_table(advanced_layer_table, repo_root, "11_user_function_reference_advanced_summary")
presentation_save_table(workflow_table, repo_root, "11_user_function_workflow")
presentation_save_plot(core_layer_plot, repo_root, "11_user_function_core_layers", width = 10, height = 6)
presentation_save_plot(tier_plot, repo_root, "11_user_function_core_vs_advanced", width = 9, height = 5.5)
presentation_save_plot(workflow_plot, repo_root, "11_user_function_workflow", width = 12, height = 5.5)

# This is the table a new user should actually keep nearby.
print(core_function_reference)

# This appendix summary makes it clear that the rest of the export surface is
# grouped by purpose, but it does not compete visually with the starter API.
print(advanced_layer_table)

# This smaller table is the "what order do I call things in?" cheat sheet.
print(workflow_table)

print(core_layer_plot)
print(tier_plot)
print(workflow_plot)

# backgammonr walkthrough and results
#
# This is the main guided analysis file for the cleaned package.
# It is intentionally long and heavily commented because its job is to teach
# the package, not just run one benchmark.
#
# What this script does:
# - orient you to the package's research question;
# - load the cached million-rollout opening truths;
# - summarize those proxy truths honestly as model-relative references;
# - walk through one opening problem with TS and TTTS;
# - run a coherent TS vs TTTS opening study;
# - show stopping diagnostics and a small posterior-sensitivity example; and
# - save tables, plots, and study objects under `analysis/output/walkthrough/`.
#
# Run this script from the repository root or from `analysis/`.

repo_root <- if (file.exists("DESCRIPTION")) {
  normalizePath(".", mustWork = TRUE)
} else if (file.exists(file.path("..", "DESCRIPTION"))) {
  normalizePath("..", mustWork = TRUE)
} else {
  stop("Run this script from the package root or from `analysis/`.", call. = FALSE)
}

truth_cache_dir <- file.path(repo_root, "cache", "opening_truths_restart")
output_root <- file.path(repo_root, "analysis", "output", "walkthrough")
truth_output_dir <- file.path(output_root, "truth")
demo_output_dir <- file.path(output_root, "one_problem")
study_output_dir <- file.path(output_root, "opening_study")
posterior_output_dir <- file.path(output_root, "posterior_sensitivity")
for (path in c(output_root, truth_output_dir, demo_output_dir, study_output_dir, posterior_output_dir)) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

demo_roll <- "1-6"
stability_rolls <- c("1-6", "2-3")
study_methods <- c("thompson", "top_two_thompson", "equal")
study_budgets <- c(16L, 32L, 64L, 128L, 256L, 512L)
study_seeds <- 1:12
detected_cores <- parallel::detectCores(logical = FALSE)
if (!is.numeric(detected_cores) || length(detected_cores) != 1L || is.na(detected_cores)) {
  detected_cores <- 1L
}
analysis_cores <- max(1L, min(8L, as.integer(detected_cores) - 1L))
analysis_parallel <- identical(.Platform$OS.type, "unix") && analysis_cores > 1L

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(repo_root, quiet = TRUE)
} else {
  library(backgammonr)
}

library(ggplot2)

announce <- function(title, file = NULL, functions = NULL) {
  line <- paste(rep("=", nchar(title)), collapse = "")
  cat("\n", line, "\n", title, "\n", line, "\n", sep = "")
  if (!is.null(file)) {
    cat("File:", file, "\n")
  }
  if (!is.null(functions)) {
    cat("Functions:", paste(functions, collapse = ", "), "\n")
  }
}

write_csv <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE)
  invisible(path)
}

save_plot <- function(plot, path, width = 9.5, height = 5.8) {
  ggplot2::ggsave(filename = path, plot = plot, width = width, height = height, dpi = 160)
  invisible(path)
}

announce(
  "Package orientation",
  file = "R/backgammonr-package.R, R/bg_problem.R, R/bg_truth.R",
  functions = c("bg_problem", "bg_reference", "bg_opening_problem")
)
cat(
  paste(
    "backgammonr studies one object: a fixed-budget local backgammon decision",
    "problem under a declared rollout model. Proxy truth is a high-budget Monte",
    "Carlo reference under that same rollout environment, not exact backgammon",
    "truth."
  ),
  "\n"
)

announce(
  "Load the cached opening-roll truths",
  file = "R/bg_truth.R",
  functions = c("bg_opening_truth_load_all", "bg_opening_truth_index", "bg_truth_certify")
)
opening_truths <- bg_opening_truth_load_all(cache_dir = truth_cache_dir)
truth_index <- bg_opening_truth_index(cache_dir = truth_cache_dir)
truth_certification <- bg_truth_certify(opening_truths)
truth_hardness <- do.call(rbind, lapply(opening_truths$truths, bg_state_difficulty))
hardness_extra <- truth_hardness[, setdiff(names(truth_hardness), names(truth_certification)), drop = FALSE]
truth_overview <- cbind(
  truth_certification,
  hardness_extra[match(truth_certification$problem_id, truth_hardness$problem_id), , drop = FALSE]
)
truth_overview <- truth_overview[order(truth_overview$top_two_gap_estimate), , drop = FALSE]

write_csv(truth_index, file.path(truth_output_dir, "opening_truth_index.csv"))
write_csv(truth_certification, file.path(truth_output_dir, "opening_truth_certification.csv"))
write_csv(truth_overview, file.path(truth_output_dir, "opening_truth_overview.csv"))

truth_gap_plot <- ggplot(
  truth_overview,
  aes(
    x = reorder(problem_id, top_two_gap_estimate),
    y = top_two_gap_estimate,
    color = certification
  )
) +
  geom_point(size = 2.7) +
  coord_flip() +
  labs(
    title = "Opening-roll proxy truth gaps",
    subtitle = "Smaller top-two gaps indicate harder local best-move identification problems.",
    x = "Opening problem",
    y = "Estimated top-two gap"
  ) +
  theme_minimal(base_size = 12)

truth_battery_plot <- plot_bg_truth(opening_truths)
save_plot(truth_gap_plot, file.path(truth_output_dir, "opening_truth_gaps.png"))
save_plot(truth_battery_plot, file.path(truth_output_dir, "opening_truth_battery.png"), width = 10.5, height = 6)

print(truth_overview[, c(
  "problem_id",
  "best_move_label",
  "top_two_gap_estimate",
  "mc_gap_excludes_zero",
  "n_near_optimal",
  "certification"
)])

announce(
  "Reference stability screen on a clear opening and a hard opening",
  file = "R/bg_truth.R",
  functions = c("bg_truth_stability")
)
stability_truths <- lapply(stability_rolls, bg_opening_truth_load_one, cache_dir = truth_cache_dir)
truth_stability <- bg_truth_stability(
  stability_truths,
  budgets = c(250000L, 500000L, 1000000L),
  seeds = 1:5,
  n_cores = analysis_cores,
  parallel = analysis_parallel,
  truth_block_size = 512L,
  reference_mode = "equal",
  dice_mode = "iid",
  progress = TRUE,
  save_path = file.path(truth_output_dir, "opening_truth_stability_subset.rds"),
  overwrite = FALSE,
  seed = 1L
)
write_csv(truth_stability$summary, file.path(truth_output_dir, "opening_truth_stability_summary.csv"))
write_csv(truth_stability$problem_summary, file.path(truth_output_dir, "opening_truth_stability_problem_summary.csv"))
print(truth_stability$problem_summary)

announce(
  "One-problem workflow on one opening roll",
  file = "R/bg_algorithms.R, R/bg_metrics.R, R/bg_plots.R",
  functions = c("bg_ts_run", "bg_ttts_run", "bg_ts_diagnostics", "plot_bg_ts_trace", "plot_bg_allocation")
)
demo_truth <- bg_opening_truth_load_one(demo_roll, cache_dir = truth_cache_dir)
demo_problem <- demo_truth$problem

demo_ts <- bg_ts_run(
  problem = demo_problem,
  budget = 256L,
  checkpoints = c(16L, 32L, 64L, 128L, 256L),
  proxy_reference = demo_truth$reference,
  seed = 1L
)
demo_ttts <- bg_ttts_run(
  problem = demo_problem,
  budget = 256L,
  checkpoints = c(16L, 32L, 64L, 128L, 256L),
  proxy_reference = demo_truth$reference,
  seed = 1L
)

demo_ts_diag <- bg_ts_diagnostics(demo_ts, truth = demo_truth)
demo_ttts_diag <- bg_ts_diagnostics(demo_ttts, truth = demo_truth)
demo_reference_panel <- rbind(
  cbind(method = "thompson", bg_eval_reference_aware(demo_ts, truth = demo_truth)),
  cbind(method = "top_two_thompson", bg_eval_reference_aware(demo_ttts, truth = demo_truth))
)
write_csv(demo_ts$action_table, file.path(demo_output_dir, "demo_ts_action_table.csv"))
write_csv(demo_ttts$action_table, file.path(demo_output_dir, "demo_ttts_action_table.csv"))
write_csv(demo_ts$checkpoint_table, file.path(demo_output_dir, "demo_ts_checkpoint_table.csv"))
write_csv(demo_ttts$checkpoint_table, file.path(demo_output_dir, "demo_ttts_checkpoint_table.csv"))
write_csv(demo_reference_panel, file.path(demo_output_dir, "demo_reference_panel.csv"))
write_csv(demo_ts_diag$allocation, file.path(demo_output_dir, "demo_ts_allocation_metrics.csv"))
write_csv(demo_ttts_diag$allocation, file.path(demo_output_dir, "demo_ttts_allocation_metrics.csv"))

save_plot(plot_bg_ts_trace(demo_ts), file.path(demo_output_dir, "demo_ts_trace.png"))
save_plot(plot_bg_ts_trace(demo_ttts), file.path(demo_output_dir, "demo_ttts_trace.png"))
save_plot(plot_bg_allocation(demo_ts, truth = demo_truth), file.path(demo_output_dir, "demo_ts_allocation.png"))
save_plot(plot_bg_allocation(demo_ttts, truth = demo_truth), file.path(demo_output_dir, "demo_ttts_allocation.png"))
save_plot(plot_bg_rank_compare(demo_ts, truth = demo_truth), file.path(demo_output_dir, "demo_ts_rank_compare.png"))
save_plot(plot_bg_rank_compare(demo_ttts, truth = demo_truth), file.path(demo_output_dir, "demo_ttts_rank_compare.png"))

print(demo_reference_panel[, c(
  "method",
  "checkpoint",
  "recommended_move_label",
  "top1_match",
  "simple_regret",
  "selected_reference_rank",
  "share_top2_truth",
  "share_mc_screened_suboptimal"
)])

announce(
  "Repeated-seed budget curves on the same opening problem",
  file = "R/bg_algorithms.R, R/bg_studies.R, R/bg_plots.R",
  functions = c("bg_compare_algorithms", "plot_bg_budget_curve")
)
demo_compare <- bg_compare_algorithms(
  problems = demo_problem,
  methods = study_methods,
  budgets = study_budgets,
  seeds = study_seeds,
  proxy_references = demo_truth$reference,
  save_path = file.path(demo_output_dir, "demo_compare.rds"),
  overwrite = FALSE,
  n_cores = analysis_cores,
  parallel = analysis_parallel,
  progress = TRUE
)

demo_budget_top1 <- bg_eval_top1(demo_compare, truth = demo_truth)
demo_budget_alloc <- bg_eval_allocation(demo_compare, truth = demo_truth)
write_csv(demo_budget_top1, file.path(demo_output_dir, "demo_budget_top1.csv"))
write_csv(demo_budget_alloc, file.path(demo_output_dir, "demo_budget_allocation.csv"))
save_plot(
  plot_bg_budget_curve(demo_compare, metric = "top1_match", truth = demo_truth),
  file.path(demo_output_dir, "demo_budget_curve_top1.png")
)
save_plot(
  plot_bg_budget_curve(demo_compare, metric = "share_top_k_truth", truth = demo_truth, top_k = 2L),
  file.path(demo_output_dir, "demo_budget_curve_share_top2.png")
)

announce(
  "Opening-roll TS vs TTTS study",
  file = "R/bg_truth.R, R/bg_metrics.R",
  functions = c("bg_opening_compare_study", "bg_stopping_diagnostics")
)
opening_study <- bg_opening_compare_study(
  proxy_truths = opening_truths,
  methods = study_methods,
  budgets = study_budgets,
  seeds = study_seeds,
  n_cores = analysis_cores,
  parallel = analysis_parallel,
  progress = TRUE,
  bootstrap_reps = 1000L,
  save_path = file.path(study_output_dir, "opening_ts_vs_ttts_study.rds"),
  overwrite = FALSE,
  seed = 1L
)

opening_stop <- bg_stopping_diagnostics(
  opening_study$comparison,
  truth = opening_truths,
  prob_best_threshold = 0.9,
  prob_good_threshold = 0.95,
  eoc_threshold = 0.01,
  stability_checkpoints = 2L
)

write_csv(opening_study$seed_panel, file.path(study_output_dir, "opening_seed_panel.csv"))
write_csv(opening_study$opening_summary, file.path(study_output_dir, "opening_summary.csv"))
write_csv(opening_study$opening_aggregate, file.path(study_output_dir, "opening_aggregate.csv"))
write_csv(opening_study$contrasts, file.path(study_output_dir, "opening_contrasts.csv"))
write_csv(opening_stop$checkpoint_summary, file.path(study_output_dir, "opening_stopping_checkpoint_summary.csv"))
write_csv(opening_stop$threshold_summary, file.path(study_output_dir, "opening_stopping_threshold_summary.csv"))

aggregate_top1 <- subset(opening_study$opening_aggregate, metric == "mean_top1_match")
aggregate_top1_plot <- ggplot(
  aggregate_top1,
  aes(x = checkpoint, y = estimate, color = allocation_policy, ymin = lower_95, ymax = upper_95)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(fill = allocation_policy), alpha = 0.15, linewidth = 0, color = NA) +
  scale_x_continuous(trans = "log2", breaks = study_budgets) +
  labs(
    title = "Probability of correct selection across the opening battery",
    subtitle = "Openings are the scientific units; intervals bootstrap over openings.",
    x = "Budget",
    y = "Mean top-1 match"
  ) +
  theme_minimal(base_size = 12) +
  guides(fill = "none")

aggregate_regret <- subset(opening_study$opening_aggregate, metric == "mean_simple_regret")
aggregate_regret_plot <- ggplot(
  aggregate_regret,
  aes(x = checkpoint, y = estimate, color = allocation_policy, ymin = lower_95, ymax = upper_95)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(fill = allocation_policy), alpha = 0.15, linewidth = 0, color = NA) +
  scale_x_continuous(trans = "log2", breaks = study_budgets) +
  labs(
    title = "Simple regret across the opening battery",
    subtitle = "Lower is better; intervals bootstrap over openings rather than pooling seeds as IID units.",
    x = "Budget",
    y = "Mean simple regret"
  ) +
  theme_minimal(base_size = 12) +
  guides(fill = "none")

contrast_subset <- subset(
  opening_study$contrasts,
  metric %in% c(
    "mean_top1_match",
    "mean_share_top2_truth",
    "mean_gap_weighted_wasted_allocation"
  )
)
contrast_plot <- ggplot(
  contrast_subset,
  aes(x = checkpoint, y = ttts_minus_ts, group = opening_roll)
) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey60") +
  geom_line(alpha = 0.25, color = "#3182bd") +
  stat_summary(aes(group = 1), fun = mean, geom = "line", linewidth = 1.1, color = "#cb181d") +
  facet_wrap(~ metric, scales = "free_y") +
  scale_x_continuous(trans = "log2", breaks = study_budgets) +
  labs(
    title = "How TTTS differs from TS across openings",
    subtitle = "Thin lines are per-opening contrasts; the thick line is the mean contrast over openings.",
    x = "Budget",
    y = "TTTS minus TS"
  ) +
  theme_minimal(base_size = 12)

save_plot(aggregate_top1_plot, file.path(study_output_dir, "opening_aggregate_top1.png"))
save_plot(aggregate_regret_plot, file.path(study_output_dir, "opening_aggregate_regret.png"))
save_plot(contrast_plot, file.path(study_output_dir, "opening_ttts_minus_ts.png"), width = 11, height = 6.5)

print(subset(opening_study$opening_summary, checkpoint == max(checkpoint))[, c(
  "opening_roll",
  "allocation_policy",
  "mean_top1_match",
  "mean_simple_regret",
  "mean_share_top2_truth",
  "mean_share_mc_screened_suboptimal",
  "recommendation_instability",
  "high_confidence_wrong_rate"
)])

announce(
  "Secondary posterior-sensitivity example",
  file = "R/bg_studies.R, R/bg_metrics.R, R/bg_plots.R",
  functions = c("bg_compare_posteriors", "bg_posterior_adequacy", "plot_bg_posterior_compare")
)
demo_posterior_compare <- backgammonr:::bg_compare_posteriors(
  problems = list(demo_problem),
  reward_model = "scalar_payoff",
  posterior_models = c("beta_pseudo", "student_t_marginal"),
  budgets = c(32L, 64L, 128L, 256L),
  seeds = 1:8,
  allocation_policy = "thompson",
  proxy_references = list(demo_truth$reference),
  save_path = file.path(posterior_output_dir, "demo_posterior_compare.rds"),
  overwrite = FALSE,
  n_cores = analysis_cores,
  parallel = analysis_parallel,
  progress = TRUE
)
demo_posterior_adequacy <- bg_posterior_adequacy(demo_ts)

write_csv(demo_posterior_compare$results, file.path(posterior_output_dir, "demo_posterior_compare_results.csv"))
write_csv(demo_posterior_compare$summary, file.path(posterior_output_dir, "demo_posterior_compare_summary.csv"))
write_csv(demo_posterior_adequacy$summary, file.path(posterior_output_dir, "demo_posterior_adequacy_summary.csv"))
write_csv(demo_posterior_adequacy$action_table, file.path(posterior_output_dir, "demo_posterior_adequacy_action_table.csv"))

save_plot(
  plot_bg_posterior_compare(demo_posterior_compare, metric = "simple_regret"),
  file.path(posterior_output_dir, "demo_posterior_compare_regret.png")
)
save_plot(
  plot_bg_posterior_compare(demo_posterior_compare, metric = "top1_match"),
  file.path(posterior_output_dir, "demo_posterior_compare_top1.png")
)

print(demo_posterior_compare$summary)
print(demo_posterior_adequacy$summary)

announce("Finished")
cat(
  paste(
    "Saved tables, plots, and reusable study objects under:",
    output_root,
    "\n",
    "The focused entry points in analysis/01_opening_truth_overview.R and",
    "analysis/02_ts_vs_ttts_opening_study.R provide smaller reproducible subsets",
    "of this full walkthrough."
  ),
  "\n"
)

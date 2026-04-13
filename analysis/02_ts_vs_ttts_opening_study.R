# TS vs TTTS opening study
#
# Purpose:
# - load the cached million-rollout opening truths;
# - run the coherent opening-roll study comparing TS, TTTS, and the equal
#   baseline; and
# - save the core result tables and plots used in presentations.
#
# Run this from the repository root or from `analysis/`.

repo_root <- if (file.exists("DESCRIPTION")) {
  normalizePath(".", mustWork = TRUE)
} else if (file.exists(file.path("..", "DESCRIPTION"))) {
  normalizePath("..", mustWork = TRUE)
} else {
  stop("Run this script from the package root or from `analysis/`.", call. = FALSE)
}

truth_cache_dir <- file.path(repo_root, "cache", "opening_truths_restart")
output_dir <- file.path(repo_root, "analysis", "output", "opening_ts_vs_ttts")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

study_methods <- c("thompson", "top_two_thompson", "equal")
study_budgets <- c(16L, 32L, 64L, 128L, 256L, 512L)
study_seeds <- 1:20
study_cores <- max(1L, min(8L, parallel::detectCores(logical = FALSE) - 1L))
study_parallel <- identical(.Platform$OS.type, "unix") && study_cores > 1L

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(repo_root, quiet = TRUE)
} else {
  library(backgammonr)
}

library(ggplot2)

announce <- function(text) {
  cat("\n", paste(rep("=", nchar(text)), collapse = ""), "\n", text, "\n",
      paste(rep("=", nchar(text)), collapse = ""), "\n", sep = "")
}

write_csv <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE)
  invisible(path)
}

save_plot <- function(plot, path, width = 9, height = 5.5) {
  ggplot2::ggsave(filename = path, plot = plot, width = width, height = height, dpi = 160)
  invisible(path)
}

announce("Load cached opening truths")
opening_truths <- bg_opening_truth_load_all(cache_dir = truth_cache_dir)

announce("Run TS vs TTTS opening study")
opening_study <- bg_opening_compare_study(
  proxy_truths = opening_truths,
  methods = study_methods,
  budgets = study_budgets,
  seeds = study_seeds,
  n_cores = study_cores,
  parallel = study_parallel,
  progress = TRUE,
  bootstrap_reps = 1000L,
  save_path = file.path(output_dir, "opening_ts_vs_ttts_study.rds"),
  overwrite = FALSE,
  seed = 1L
)

announce("Save result tables")
write_csv(opening_study$seed_panel, file.path(output_dir, "opening_seed_panel.csv"))
write_csv(opening_study$opening_summary, file.path(output_dir, "opening_summary.csv"))
write_csv(opening_study$opening_aggregate, file.path(output_dir, "opening_aggregate.csv"))
write_csv(opening_study$contrasts, file.path(output_dir, "opening_contrasts.csv"))

final_checkpoint <- max(opening_study$opening_summary$checkpoint)
final_summary <- subset(opening_study$opening_summary, checkpoint == final_checkpoint)
write_csv(final_summary, file.path(output_dir, "opening_summary_final_checkpoint.csv"))

announce("Run stopping diagnostics")
stopping <- bg_stopping_diagnostics(
  opening_study$comparison,
  truth = opening_truths,
  prob_best_threshold = 0.9,
  prob_good_threshold = 0.95,
  eoc_threshold = 0.01,
  stability_checkpoints = 2L
)
write_csv(stopping$checkpoint_summary, file.path(output_dir, "stopping_checkpoint_summary.csv"))
write_csv(stopping$threshold_summary, file.path(output_dir, "stopping_threshold_summary.csv"))

announce("Create study plots")
aggregate_top1 <- subset(opening_study$opening_aggregate, metric == "mean_top1_match")
plot_top1 <- ggplot(
  aggregate_top1,
  aes(x = checkpoint, y = estimate, color = allocation_policy, ymin = lower_95, ymax = upper_95)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(fill = allocation_policy), alpha = 0.15, linewidth = 0, color = NA) +
  scale_x_continuous(trans = "log2", breaks = study_budgets) +
  labs(
    title = "Opening-study probability of correct selection",
    subtitle = "Aggregated equally over openings with bootstrap intervals over openings.",
    x = "Budget",
    y = "Mean top-1 match"
  ) +
  theme_minimal(base_size = 12) +
  guides(fill = "none")

aggregate_regret <- subset(opening_study$opening_aggregate, metric == "mean_simple_regret")
plot_regret <- ggplot(
  aggregate_regret,
  aes(x = checkpoint, y = estimate, color = allocation_policy, ymin = lower_95, ymax = upper_95)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(fill = allocation_policy), alpha = 0.15, linewidth = 0, color = NA) +
  scale_x_continuous(trans = "log2", breaks = study_budgets) +
  labs(
    title = "Opening-study simple regret",
    subtitle = "Lower is better; intervals are opening-level bootstrap intervals.",
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

plot_contrasts <- ggplot(
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
    subtitle = "Thin lines are per-opening contrasts; thick red lines are opening means.",
    x = "Budget",
    y = "TTTS minus TS"
  ) +
  theme_minimal(base_size = 12)

save_plot(plot_top1, file.path(output_dir, "aggregate_top1_match.png"))
save_plot(plot_regret, file.path(output_dir, "aggregate_simple_regret.png"))
save_plot(plot_contrasts, file.path(output_dir, "ttts_minus_ts_contrasts.png"), width = 11, height = 6.5)

announce("Done")
print(final_summary[, c(
  "opening_roll",
  "allocation_policy",
  "mean_top1_match",
  "mean_simple_regret",
  "mean_share_top2_truth",
  "mean_share_mc_screened_suboptimal",
  "recommendation_instability",
  "high_confidence_wrong_rate"
)])

cat("\nSaved outputs to:\n", output_dir, "\n", sep = "")

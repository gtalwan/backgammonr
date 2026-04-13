# Opening truth overview
#
# Purpose:
# - load the cached million-rollout opening truths;
# - summarize what the package currently treats as proxy truth for the 21
#   opening decisions; and
# - save the core tables and plots used by later studies.
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
output_dir <- file.path(repo_root, "analysis", "output", "opening_truth_overview")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

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

announce("Build truth summary tables")
truth_index <- bg_opening_truth_index(cache_dir = truth_cache_dir)
truth_certification <- bg_truth_certify(opening_truths)
truth_hardness <- do.call(rbind, lapply(opening_truths$truths, bg_state_difficulty))
hardness_extra <- truth_hardness[, setdiff(names(truth_hardness), names(truth_certification)), drop = FALSE]
truth_overview <- cbind(
  truth_certification,
  hardness_extra[match(truth_certification$problem_id, truth_hardness$problem_id), , drop = FALSE]
)
truth_overview <- truth_overview[order(truth_overview$top_two_gap_estimate), , drop = FALSE]

announce("Save truth tables")
write_csv(truth_index, file.path(output_dir, "opening_truth_index.csv"))
write_csv(truth_certification, file.path(output_dir, "opening_truth_certification.csv"))
write_csv(truth_overview, file.path(output_dir, "opening_truth_overview.csv"))

announce("Create truth plots")
truth_gap_plot <- ggplot(
  truth_overview,
  aes(
    x = reorder(problem_id, top_two_gap_estimate),
    y = top_two_gap_estimate,
    color = certification
  )
) +
  geom_point(size = 2.6) +
  coord_flip() +
  labs(
    title = "Opening-roll proxy truths",
    subtitle = "Openings are ordered by the estimated proxy top-two gap.",
    x = "Opening problem",
    y = "Estimated top-two gap"
  ) +
  theme_minimal(base_size = 12)

battery_plot <- plot_bg_truth(opening_truths)

save_plot(truth_gap_plot, file.path(output_dir, "opening_truth_gaps.png"))
save_plot(battery_plot, file.path(output_dir, "opening_truth_battery.png"), width = 10, height = 6)

announce("Done")
print(truth_overview[, c(
  "problem_id",
  "best_move_label",
  "top_two_gap_estimate",
  "mc_gap_excludes_zero",
  "n_near_optimal",
  "certification"
)])

cat("\nSaved outputs to:\n", output_dir, "\n", sep = "")

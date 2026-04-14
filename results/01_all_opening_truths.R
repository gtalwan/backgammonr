# 01_all_opening_truths.R
#
# Purpose:
# - show the preserved master truth results for all 21 opening rolls;
# - save one clean summary table plus a few high-level truth plots; and
# - make it obvious which openings are easy and which are hard.
#
# Package functions demonstrated here:
# - bg_truth_load() via `results_load_all_master_truths()`
# - plot_bg_truth()

source(if (file.exists("DESCRIPTION")) "results/00_results_common.R" else "00_results_common.R")

repo_root <- results_repo_root()
results_load_package(repo_root)
library(ggplot2)

# There is no algorithm budget here because this file only studies the
# preserved master truths in `cache/opening_truths_master`.
opening_truths <- results_load_all_master_truths(repo_root)
truth_summary <- opening_truths$summary

# These small tables are useful for slide-ready summaries.
hardest_openings <- truth_summary[order(truth_summary$top_two_gap_estimate), , drop = FALSE]
easiest_openings <- truth_summary[order(-truth_summary$top_two_gap_estimate), , drop = FALSE]

# This ladder plot is the quickest view of all 21 truth objects.
truth_ladder_plot <- plot_bg_truth(opening_truths) +
  labs(
    title = "All 21 opening master truths",
    subtitle = "The gap between the best and second-best move is the simplest hardness summary."
  )

# This plot shows how local branching and local hardness relate to each other.
gap_vs_moves_plot <- ggplot(
  truth_summary,
  aes(x = n_moves, y = top_two_gap_estimate, label = opening_roll)
) +
  geom_point(size = 2.6, color = "#0072B2") +
  geom_text(nudge_y = 0.0015, size = 3, check_overlap = TRUE) +
  labs(
    title = "Opening hardness versus number of candidate moves",
    subtitle = "Smaller truth gaps are harder; more candidate moves often make the local problem busier.",
    x = "Number of candidate moves",
    y = "top_two_gap_estimate"
  ) +
  results_plot_theme()

# This interval plot is useful when the point gap is small and the opening
# remains locally ambiguous.
gap_interval_plot <- ggplot(
  truth_summary,
  aes(
    x = reorder(opening_roll, top_two_gap_estimate),
    y = top_two_gap_estimate,
    ymin = top_two_gap_mc_lower_95,
    ymax = top_two_gap_mc_upper_95,
    color = mc_gap_excludes_zero
  )
) +
  geom_pointrange(linewidth = 0.4) +
  coord_flip() +
  labs(
    title = "Top-two gap estimate with Monte Carlo interval",
    subtitle = "Openings whose interval still touches zero are harder to separate cleanly.",
    x = NULL,
    y = "top_two_gap_estimate",
    color = "Gap excludes zero"
  ) +
  results_plot_theme()

results_save_table(truth_summary, repo_root, "01_all_opening_truths_summary")
results_save_table(head(hardest_openings, 10L), repo_root, "01_all_opening_truths_hardest")
results_save_table(head(easiest_openings, 10L), repo_root, "01_all_opening_truths_easiest")

results_save_plot(truth_ladder_plot, repo_root, "01_all_opening_truths_ladder", width = 10, height = 7)
results_save_plot(gap_vs_moves_plot, repo_root, "01_all_opening_truths_gap_vs_moves", width = 10, height = 6)
results_save_plot(gap_interval_plot, repo_root, "01_all_opening_truths_gap_interval", width = 10, height = 7)

print(truth_summary)
print(head(hardest_openings, 10L))
print(head(easiest_openings, 10L))
print(truth_ladder_plot)
print(gap_vs_moves_plot)
print(gap_interval_plot)


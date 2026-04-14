# 01_truth_overview_openings.R
#
# Purpose:
# - load the preserved master opening truths;
# - summarize the 21 opening problems; and
# - create presentation-ready truth/hardness figures.
#
# Main package functions used here:
# - bg_truth_load()                       [R/bg_truth.R] via Presentation helper
# - bg_truth_project()                    [R/bg_truth.R]
# - bg_truth_diagnostics()                [R/bg_truth.R]
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
library(ggplot2)

truth_cache_dir <- presentation_truth_cache_dir(repo_root)
opening_truths <- presentation_load_opening_truths(repo_root)
truth_index <- opening_truths$summary
master_reference_budget <- presentation_master_reference_budget(opening_truths)
truth_summary <- opening_truths$summary[
  order(opening_truths$summary$top_two_gap_estimate),
  c(
    "opening_roll",
    "best_move_label",
    "top_two_gap_estimate",
    "top_two_gap_mc_lower_95",
    "top_two_gap_mc_upper_95",
    "difficulty_label",
    "n_moves",
    "n_near_optimal",
    "mc_not_separated_from_best_set_size",
    "mean_reference_se",
    "mean_unresolved_fraction"
  )
]

hardest_openings <- utils::head(truth_summary, 8L)

gap_plot <- plot_bg_truth(opening_truths) +
  labs(
    title = "Opening truth gap ladder",
    subtitle = paste(
      "Smaller top-two gaps indicate harder best-move identification problems.",
      "The preserved master cache carries a reference budget of",
      format(master_reference_budget, big.mark = ","),
      "rollouts per opening."
    )
  )

# This scatter links truth hardness to branching complexity. It gives a quick
# sense of whether hard openings are hard because they have many moves, many
# near-optimal moves, or both.
truth_scatter <- ggplot(
  truth_summary,
  aes(x = n_moves, y = top_two_gap_estimate, color = difficulty_label, size = n_near_optimal)
) +
  geom_point(alpha = 0.9) +
  labs(
    title = "Opening hardness versus branching",
    subtitle = "Hard openings tend to have smaller top-two gaps and more near-optimal moves.",
    x = "Number of candidate moves",
    y = "Top-two gap estimate",
    color = "Difficulty",
    size = "Near-optimal\nmoves"
  ) +
  bg_plot_theme_research()

# This plot tracks Monte Carlo precision against the size of the true gap. It
# is a good reminder that "hard" can mean both small separation and a need for
# tighter uncertainty control.
uncertainty_plot <- ggplot(
  truth_summary,
  aes(x = top_two_gap_estimate, y = mean_reference_se, color = difficulty_label)
) +
  geom_point(size = 2.8, alpha = 0.9) +
  labs(
    title = "Proxy-truth uncertainty versus truth gap",
    subtitle = "Openings with small truth gaps require tighter Monte Carlo precision to screen moves cleanly.",
    x = "Top-two gap estimate",
    y = "Mean reference SE",
    color = "Difficulty"
  ) +
  bg_plot_theme_research()

move_count_plot <- ggplot(
  truth_summary,
  aes(x = reorder(opening_roll, n_moves), y = n_moves, fill = difficulty_label)
) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Candidate-move count by opening roll",
    subtitle = "This is the branching factor each opening problem starts with.",
    x = NULL,
    y = "Number of candidate moves"
  ) +
  bg_plot_theme_research()

near_optimal_plot <- ggplot(
  truth_summary,
  aes(
    x = reorder(opening_roll, n_near_optimal),
    y = n_near_optimal,
    fill = mc_not_separated_from_best_set_size
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "How many moves stay near the best move?",
    subtitle = "Openings with many near-optimal moves are the hardest to screen quickly.",
    x = NULL,
    y = "Near-optimal moves",
    fill = "MC-not-\nseparated\nset size"
  ) +
  bg_plot_theme_research()

presentation_save_table(truth_index, repo_root, "01_truth_index")
presentation_save_table(truth_summary, repo_root, "01_truth_summary")
presentation_save_table(hardest_openings, repo_root, "01_hardest_openings")
presentation_save_plot(gap_plot, repo_root, "01_truth_gap_ladder", width = 10, height = 7)
presentation_save_plot(truth_scatter, repo_root, "01_truth_hardness_vs_moves", width = 9, height = 6)
presentation_save_plot(uncertainty_plot, repo_root, "01_truth_uncertainty_vs_gap", width = 9, height = 6)
presentation_save_plot(move_count_plot, repo_root, "01_truth_move_counts", width = 9, height = 7)
presentation_save_plot(near_optimal_plot, repo_root, "01_truth_near_optimal_counts", width = 9, height = 7)

# This compact table is the opening shortlist to talk through first.
print(hardest_openings)

# The next plots move from a global ladder to specific explanations of what
# makes some openings easy and others hard.
print(gap_plot)
print(truth_scatter)
print(uncertainty_plot)
print(move_count_plot)
print(near_optimal_plot)


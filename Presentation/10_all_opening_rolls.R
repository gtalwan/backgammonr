# 10_all_opening_rolls.R
#
# Purpose:
# - walk through all 21 opening rolls in one place;
# - show what the preserved master cache says about each opening;
# - highlight the top moves for each roll; and
# - create a compact catalog of the full opening battery.
#
# Main package functions used here:
# - bg_opening_rolls()                   [R/bg_truth.R]
# - bg_truth_load()                      [R/bg_truth.R] via Presentation helper
# - bg_truth_certify()                   [R/bg_truth.R]
# - plot_bg_truth()                      [R/bg_plots.R]
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

master_truths <- presentation_load_opening_truths(repo_root)
master_reference_budget <- presentation_master_reference_budget(master_truths)

opening_catalog <- master_truths$summary[
  order(master_truths$summary$die1, master_truths$summary$die2),
  c(
    "opening_roll",
    "best_move_label",
    "top_two_gap_estimate",
    "top_two_gap_mc_lower_95",
    "top_two_gap_mc_upper_95",
    "difficulty_label",
    "n_moves",
    "n_near_optimal",
    "mean_reference_se",
    "mean_unresolved_fraction",
    "is_double",
    "roll_group"
  )
]

top_move_rows <- lapply(
  master_truths$truths,
  function(truth) {
    top_moves <- truth$reference$action_table[
      order(truth$reference$action_table$rank),
      c(
        "move_label",
        "reference_mean",
        "reference_mc_lower_95",
        "reference_mc_upper_95",
        "rank"
      )
    ]
    top_moves <- utils::head(top_moves, 3L)
    top_moves$opening_roll <- presentation_roll_label(truth$problem$roll)
    top_moves
  }
)

top_moves_table <- do.call(rbind, top_move_rows)
top_moves_table <- merge(
  top_moves_table,
  opening_catalog[, c("opening_roll", "difficulty_label", "n_moves")],
  by = "opening_roll",
  all.x = TRUE,
  sort = FALSE
)
top_moves_table$opening_roll <- factor(
  top_moves_table$opening_roll,
  levels = opening_catalog$opening_roll
)

hardest_openings <- utils::head(
  opening_catalog[order(opening_catalog$top_two_gap_estimate), ],
  6L
)
easiest_openings <- utils::head(
  opening_catalog[order(-opening_catalog$top_two_gap_estimate), ],
  6L
)

# This ladder is the global opening catalog view. It should be read before the
# more detailed top-move panels below.
gap_ci_plot <- ggplot(
  opening_catalog,
  aes(
    x = reorder(opening_roll, top_two_gap_estimate),
    y = top_two_gap_estimate,
    ymin = top_two_gap_mc_lower_95,
    ymax = top_two_gap_mc_upper_95,
    color = difficulty_label
  )
) +
  geom_pointrange(linewidth = 0.5) +
  coord_flip() +
  labs(
    title = "All opening rolls ordered by truth top-two gap",
    subtitle = paste(
      "The preserved master cache uses a reference budget of",
      format(master_reference_budget, big.mark = ","),
      "rollouts per opening."
    ),
    x = NULL,
    y = "Top-two gap estimate",
    color = "Difficulty"
  ) +
  bg_plot_theme_research()

# This bar chart is the cleanest "how many options are we asking the algorithm
# to separate?" plot for the entire opening battery.
move_count_plot <- ggplot(
  opening_catalog,
  aes(x = reorder(opening_roll, n_moves), y = n_moves, fill = difficulty_label)
) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Candidate-move count for all opening rolls",
    subtitle = "The opening battery spans both easy low-branching and hard high-branching problems.",
    x = NULL,
    y = "Candidate moves"
  ) +
  bg_plot_theme_research()

# This faceted panel is the most literal "go through all the openings" plot in
# the file: each facet shows the top three moves for one opening.
top_moves_plot <- ggplot(
  top_moves_table,
  aes(
    x = reference_mean,
    y = reorder(move_label, reference_mean),
    xmin = reference_mc_lower_95,
    xmax = reference_mc_upper_95,
    color = factor(rank)
  )
) +
  geom_errorbarh(height = 0.2, linewidth = 0.4) +
  geom_point(size = 2.2) +
  facet_wrap(~ opening_roll, scales = "free_y", ncol = 3) +
  scale_color_manual(values = c("1" = "#D55E00", "2" = "#0072B2", "3" = "#6C757D")) +
  labs(
    title = "Top three moves for every opening roll",
    subtitle = "Rank 1 is the truth-best move in the preserved master cache.",
    x = "Reference mean",
    y = NULL,
    color = "Truth rank"
  ) +
  bg_plot_theme_research()

# This precision plot keeps the cache quality visible. Hard openings are not
# just about small gaps; they also demand tighter Monte Carlo precision.
precision_plot <- ggplot(
  opening_catalog,
  aes(x = top_two_gap_estimate, y = mean_reference_se, color = is_double, size = n_near_optimal)
) +
  geom_point(alpha = 0.9) +
  geom_smooth(
    data = opening_catalog,
    aes(x = top_two_gap_estimate, y = mean_reference_se),
    inherit.aes = FALSE,
    se = FALSE,
    linewidth = 0.8,
    method = "lm",
    color = "#6C757D"
  ) +
  labs(
    title = "Cache precision versus opening hardness",
    subtitle = "Smaller gaps and more near-optimal moves require more careful truth separation.",
    x = "Top-two gap estimate",
    y = "Mean reference SE",
    color = "Is double",
    size = "Near-optimal\nmoves"
  ) +
  bg_plot_theme_research()

presentation_save_table(opening_catalog, repo_root, "10_all_opening_rolls_catalog")
presentation_save_table(top_moves_table, repo_root, "10_all_opening_rolls_top_moves")
presentation_save_table(hardest_openings, repo_root, "10_all_opening_rolls_hardest")
presentation_save_table(easiest_openings, repo_root, "10_all_opening_rolls_easiest")
presentation_save_plot(gap_ci_plot, repo_root, "10_all_opening_rolls_gap_ci", width = 10, height = 8)
presentation_save_plot(move_count_plot, repo_root, "10_all_opening_rolls_move_counts", width = 9, height = 8)
presentation_save_plot(top_moves_plot, repo_root, "10_all_opening_rolls_top_moves", width = 14, height = 12)
presentation_save_plot(precision_plot, repo_root, "10_all_opening_rolls_precision", width = 10, height = 7)

# The catalog table is the shortest way to scan the whole opening battery.
print(opening_catalog)

# These two tables are the cleanest "hard versus easy" slices to use in a talk.
print(hardest_openings)
print(easiest_openings)

print(gap_ci_plot)
print(move_count_plot)
print(top_moves_plot)
print(precision_plot)

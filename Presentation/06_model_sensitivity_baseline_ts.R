# 06_model_sensitivity_baseline_ts.R
#
# Purpose:
# - hold the algorithm fixed and vary the model stack;
# - compare TS against equal within each stack; and
# - identify openings where model choice changes the recommendation story.
#
# Main package functions used here:
# - bg_opening_truth_load_all()          [R/bg_truth.R]
# - bg_truth_project()                   [R/bg_truth.R]
# - bg_opening_compare_study()           [R/bg_truth.R]
#
# Relevant native files:
# - src/model_beta_bernoulli.cpp
# - src/model_student_t.cpp
# - src/model_dirichlet_categorical.cpp
# - src/metrics_summary.cpp
#
# These settings are intentionally presentation-oriented rather than research-
# batch sized: the checkpoint grid is denser and the final budget is higher,
# but the script still avoids a full heavy battery rebuild.

common_path <- if (file.exists("DESCRIPTION")) {
  file.path("Presentation", "presentation_common.R")
} else {
  "presentation_common.R"
}
source(common_path)

repo_root <- presentation_repo_root()
presentation_load_package(repo_root)
library(ggplot2)

stack_ids <- c("beta_bernoulli", "student_t", "dirichlet")
methods <- c("thompson", "equal")
budgets <- presentation_checkpoint_grid(presentation_run_budget())
seeds <- 1L
bootstrap_reps <- 200L

workers <- presentation_detect_workers(max_cores = 4L)
cached_truths <- presentation_load_opening_truths(repo_root)
master_reference_budget <- presentation_master_reference_budget(cached_truths)

studies <- lapply(
  stack_ids,
  function(stack_id) {
    spec <- presentation_stack_spec(stack_id)
    projected_truths <- presentation_project_truths(cached_truths, stack = stack_id)

    study <- bg_opening_compare_study(
      proxy_truths = projected_truths,
      methods = methods,
      budgets = budgets,
      seeds = seeds,
      n_cores = workers$n_cores,
      parallel = workers$parallel,
      progress = TRUE,
      bootstrap_reps = bootstrap_reps,
      save_path = file.path(
        presentation_output_dir(repo_root, "studies"),
        paste0("06_model_sensitivity_", stack_id, "_master_2048.rds")
      ),
      overwrite = TRUE,
      seed = 1L
    )

    list(spec = spec, study = study)
  }
)
names(studies) <- stack_ids

final_stack_table <- do.call(
  rbind,
  lapply(
    studies,
    function(x) {
      study <- x$study
      spec <- x$spec
      final_checkpoint <- max(study$opening_summary$checkpoint)
      final_opening <- subset(study$opening_summary, checkpoint == final_checkpoint)
      final_aggregate <- subset(study$opening_aggregate, checkpoint == final_checkpoint)

      ts_row <- final_opening[final_opening$allocation_policy == "thompson", , drop = FALSE]
      eq_row <- final_opening[final_opening$allocation_policy == "equal", , drop = FALSE]
      ts_agg <- final_aggregate[final_aggregate$allocation_policy == "thompson", , drop = FALSE]

      get_metric <- function(metric) ts_agg$estimate[ts_agg$metric == metric][[1L]]

      data.frame(
        stack = spec$stack,
        stack_label = spec$label,
        final_budget = final_checkpoint,
        ts_top1_match = get_metric("mean_top1_match"),
        ts_simple_regret = get_metric("mean_simple_regret"),
        ts_truth_top2_hit = get_metric("mean_truth_top2_hit"),
        ts_share_top2_truth = get_metric("mean_share_top2_truth"),
        ts_high_confidence_wrong_rate = get_metric("high_confidence_wrong_rate"),
        ts_selected_reference_rank = mean(ts_row$mean_selected_reference_rank, na.rm = TRUE),
        ts_minus_equal_top1 = mean(ts_row$mean_top1_match - eq_row$mean_top1_match, na.rm = TRUE),
        ts_minus_equal_regret = mean(eq_row$mean_simple_regret - ts_row$mean_simple_regret, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }
  )
)

stack_curve <- do.call(
  rbind,
  lapply(
    studies,
    function(x) {
      out <- x$study$opening_aggregate
      out$stack <- x$spec$stack
      out$stack_label <- x$spec$label
      out
    }
  )
)

stack_curve$method <- factor(
  presentation_method_label(stack_curve$allocation_policy),
  levels = presentation_method_label(methods)
)

stack_palette <- presentation_method_palette(methods)

# This is the headline sensitivity plot: does changing the statistical model
# stack change the TS-versus-equal story?
plot_top1 <- ggplot(
  subset(stack_curve, metric == "mean_top1_match"),
  aes(x = checkpoint, y = estimate, color = method, ymin = lower_95, ymax = upper_95)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(fill = method), alpha = 0.12, linewidth = 0, color = NA) +
  facet_wrap(~ stack_label) +
  scale_color_manual(values = stack_palette) +
  scale_fill_manual(values = stack_palette) +
  scale_x_continuous(trans = "log2", breaks = budgets) +
  labs(
    title = "Model sensitivity for baseline TS",
    subtitle = paste(
      "Within each stack, TS is compared against equal allocation.",
      "All stacks are projected from the same preserved master truths at reference budget",
      format(master_reference_budget, big.mark = ","),
      "."
    ),
    x = "Budget",
    y = "Mean top-1 match",
    color = "Method",
    fill = "Method"
  ) +
  bg_plot_theme_research()

# Focus on the truth top-2 is the cross-stack mechanism plot. It explains
# whether a stack encourages better budget concentration.
plot_focus <- ggplot(
  subset(stack_curve, metric == "mean_share_top2_truth"),
  aes(x = checkpoint, y = estimate, color = method, ymin = lower_95, ymax = upper_95)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(fill = method), alpha = 0.12, linewidth = 0, color = NA) +
  facet_wrap(~ stack_label) +
  scale_color_manual(values = stack_palette) +
  scale_fill_manual(values = stack_palette) +
  scale_x_continuous(trans = "log2", breaks = budgets) +
  labs(
    title = "Budget focus across model stacks",
    subtitle = "Higher is better; this tracks how much budget lands on the truth top-2 moves.",
    x = "Budget",
    y = "Mean share on truth top-2",
    color = "Method",
    fill = "Method"
  ) +
  bg_plot_theme_research()

# Regret is the smoother performance metric for cross-stack comparisons.
plot_regret <- ggplot(
  subset(stack_curve, metric == "mean_simple_regret"),
  aes(x = checkpoint, y = estimate, color = method, ymin = lower_95, ymax = upper_95)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(fill = method), alpha = 0.12, linewidth = 0, color = NA) +
  facet_wrap(~ stack_label) +
  scale_color_manual(values = stack_palette) +
  scale_fill_manual(values = stack_palette) +
  scale_x_continuous(trans = "log2", breaks = budgets) +
  labs(
    title = "Simple regret across model stacks",
    subtitle = "Lower is better.",
    x = "Budget",
    y = "Mean simple regret",
    color = "Method",
    fill = "Method"
  ) +
  bg_plot_theme_research()

# This is the strictest error plot in the file. It penalizes confidence that is
# not backed up by actual accuracy.
plot_conf_wrong <- ggplot(
  subset(stack_curve, metric == "high_confidence_wrong_rate"),
  aes(x = checkpoint, y = estimate, color = method, ymin = lower_95, ymax = upper_95)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(fill = method), alpha = 0.12, linewidth = 0, color = NA) +
  facet_wrap(~ stack_label) +
  scale_color_manual(values = stack_palette) +
  scale_fill_manual(values = stack_palette) +
  scale_x_continuous(trans = "log2", breaks = budgets) +
  labs(
    title = "High-confidence wrong rate across model stacks",
    subtitle = "Lower is better; confidence should not amplify mistakes.",
    x = "Budget",
    y = "High-confidence wrong rate",
    color = "Method",
    fill = "Method"
  ) +
  bg_plot_theme_research()

plot_top2_hit <- ggplot(
  subset(stack_curve, metric == "mean_truth_top2_hit"),
  aes(x = checkpoint, y = estimate, color = method, ymin = lower_95, ymax = upper_95)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(fill = method), alpha = 0.12, linewidth = 0, color = NA) +
  facet_wrap(~ stack_label) +
  scale_color_manual(values = stack_palette) +
  scale_fill_manual(values = stack_palette) +
  scale_x_continuous(trans = "log2", breaks = budgets) +
  labs(
    title = "Truth top-2 hit across model stacks",
    subtitle = "This is a forgiving accuracy view for the hardest openings and the most controversial stacks.",
    x = "Budget",
    y = "Mean truth top-2 hit",
    color = "Method",
    fill = "Method"
  ) +
  bg_plot_theme_research()

# This bar chart is the cleanest cross-stack scoreboard at the final budget.
advantage_plot <- ggplot(
  final_stack_table,
  aes(x = reorder(stack_label, ts_minus_equal_top1), y = ts_minus_equal_top1, fill = stack_label)
) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "TS advantage over equal at the final budget",
    subtitle = "Positive values mean TS beats equal on mean top-1 match within that stack.",
    x = NULL,
    y = "TS minus equal top-1 match"
  ) +
  bg_plot_theme_research()

modal_recommendations <- do.call(
  rbind,
  lapply(
    studies,
    function(x) {
      final_seed_panel <- subset(
        x$study$seed_panel,
        checkpoint == max(x$study$seed_panel$checkpoint) &
          allocation_policy == "thompson"
      )

      rows <- lapply(
        split(final_seed_panel, final_seed_panel$opening_roll),
        function(df) {
          modal_move <- names(sort(table(df$recommended_move_label), decreasing = TRUE))[[1L]]
          data.frame(
            opening_roll = df$opening_roll[[1L]],
            stack = x$spec$stack,
            stack_label = x$spec$label,
            modal_recommendation = modal_move,
            mean_selected_reference_rank = mean(df$selected_reference_rank, na.rm = TRUE),
            stringsAsFactors = FALSE
          )
        }
      )
      do.call(rbind, rows)
    }
  )
)

modal_heatmap <- ggplot(
  modal_recommendations,
  aes(x = stack_label, y = factor(opening_roll, levels = unique(cached_truths$summary$opening_roll)), fill = mean_selected_reference_rank)
) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "#0B4F6C", high = "#E6CCB2") +
  labs(
    title = "TS selected truth rank by opening and model stack",
    subtitle = "Lower is better; 1 means TS selected the truth-best move on average.",
    x = NULL,
    y = "Opening roll",
    fill = "Mean selected\ntruth rank"
  ) +
  bg_plot_theme_research()

modal_wide <- reshape(
  modal_recommendations[, c("opening_roll", "stack", "modal_recommendation")],
  idvar = "opening_roll",
  timevar = "stack",
  direction = "wide"
)
disagreement_table <- modal_wide[
  apply(modal_wide[, -1, drop = FALSE], 1L, function(x) length(unique(x)) > 1L),
  ,
  drop = FALSE
]

opening_advantage <- do.call(
  rbind,
  lapply(
    studies,
    function(x) {
      final_opening <- subset(x$study$opening_summary, checkpoint == max(x$study$opening_summary$checkpoint))
      merged <- merge(
        final_opening[final_opening$allocation_policy == "thompson", c("opening_roll", "mean_top1_match")],
        final_opening[final_opening$allocation_policy == "equal", c("opening_roll", "mean_top1_match")],
        by = "opening_roll",
        suffixes = c("_ts", "_equal"),
        all = FALSE
      )
      data.frame(
        opening_roll = merged$opening_roll,
        stack_label = x$spec$label,
        ts_minus_equal_top1 = merged$mean_top1_match_ts - merged$mean_top1_match_equal
      )
    }
  )
)

opening_advantage_heatmap <- ggplot(
  opening_advantage,
  aes(x = stack_label, y = factor(opening_roll, levels = unique(cached_truths$summary$opening_roll)), fill = ts_minus_equal_top1)
) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_gradient2(low = "#0B4F6C", mid = "#F4F1EA", high = "#D55E00", midpoint = 0) +
  labs(
    title = "TS minus equal by opening and model stack",
    subtitle = "Positive cells mean TS improves top-1 match over equal in that stack.",
    x = NULL,
    y = "Opening roll",
    fill = "TS - Equal\nTop-1"
  ) +
  bg_plot_theme_research()

saveRDS(
  list(studies = studies, final_stack_table = final_stack_table, modal_recommendations = modal_recommendations),
  file = file.path(
    presentation_output_dir(repo_root, "studies"),
    "06_model_sensitivity_summary_master_2048.rds"
  )
)

presentation_save_table(final_stack_table, repo_root, "06_model_sensitivity_final_stack_table")
presentation_save_table(disagreement_table, repo_root, "06_model_sensitivity_disagreement_table")
presentation_save_plot(plot_top1, repo_root, "06_model_sensitivity_top1", width = 12, height = 7)
presentation_save_plot(plot_focus, repo_root, "06_model_sensitivity_focus", width = 12, height = 7)
presentation_save_plot(plot_regret, repo_root, "06_model_sensitivity_regret", width = 12, height = 7)
presentation_save_plot(plot_conf_wrong, repo_root, "06_model_sensitivity_high_conf_wrong", width = 12, height = 7)
presentation_save_plot(plot_top2_hit, repo_root, "06_model_sensitivity_top2_hit", width = 12, height = 7)
presentation_save_plot(advantage_plot, repo_root, "06_model_sensitivity_ts_advantage", width = 9, height = 5.5)
presentation_save_plot(modal_heatmap, repo_root, "06_model_sensitivity_modal_rank_heatmap", width = 10, height = 8)
presentation_save_plot(opening_advantage_heatmap, repo_root, "06_model_sensitivity_opening_advantage_heatmap", width = 10, height = 8)

# This final table is the cross-stack summary to present first.
print(final_stack_table)

# These are the openings where the model stack materially changes the TS
# recommendation story.
print(disagreement_table)
print(plot_top1)
print(plot_focus)
print(plot_regret)
print(plot_conf_wrong)
print(plot_top2_hit)
print(advantage_plot)
print(modal_heatmap)
print(opening_advantage_heatmap)

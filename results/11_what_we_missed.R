# 11_what_we_missed.R
#
# Purpose:
# - compare the same opening across the three headline model stacks;
# - run the same direct methods in each stack; and
# - show where the stack choice changes the conclusion.

source(if (file.exists("DESCRIPTION")) "results/00_results_common.R" else "00_results_common.R")

repo_root <- results_repo_root()
results_load_package(repo_root)
library(ggplot2)

# This file compares the same opening across stacks rather than across methods.
roll <- "1-6"
budget <- results_run_budget()
checkpoints <- results_checkpoint_grid(budget)
stacks <- c("beta_bernoulli", "student_t", "dirichlet")

stack_rows <- lapply(
  stacks,
  function(stack_i) {
    master_truth <- results_load_master_truth(repo_root, roll = roll)
    stack_spec <- results_stack_spec(stack_i)
    truth <- results_project_truth(master_truth, stack = stack_i)

    fit_ts <- bg_ts_run(
      problem = truth$problem,
      budget = budget,
      checkpoints = checkpoints,
      proxy_reference = truth$reference,
      seed = 1L
    )

    fit_equal <- bg_equal_run(
      problem = truth$problem,
      budget = budget,
      checkpoints = checkpoints,
      proxy_reference = truth$reference,
      seed = 1L
    )

    ts_summary <- results_final_summary(
      "thompson",
      results_collect_run_panels(fit_ts, truth, checkpoints)$checkpoint_metrics
    )
    equal_summary <- results_final_summary(
      "equal",
      results_collect_run_panels(fit_equal, truth, checkpoints)$checkpoint_metrics
    )

    truth_row <- results_truth_overview(master_truth, truth, roll, stack_spec$label)

    list(
      truth_row = truth_row,
      summary = rbind(
        transform(ts_summary, stack = stack_spec$label),
        transform(equal_summary, stack = stack_spec$label)
      )
    )
  }
)

truth_stack_table <- do.call(rbind, lapply(stack_rows, `[[`, "truth_row"))
final_summary <- do.call(rbind, lapply(stack_rows, `[[`, "summary"))
rownames(truth_stack_table) <- NULL
rownames(final_summary) <- NULL

top1_plot <- ggplot(
  final_summary,
  aes(x = stack, y = top1_match, fill = method)
) +
  geom_col(position = "dodge") +
  labs(
    title = paste("Top-1 match by stack on opening", roll),
    subtitle = "This is the simplest stack-by-stack check for TS versus Equal.",
    x = NULL,
    y = "top1_match",
    fill = "Method"
  ) +
  results_plot_theme()

regret_plot <- ggplot(
  final_summary,
  aes(x = stack, y = simple_regret, fill = method)
) +
  geom_col(position = "dodge") +
  labs(
    title = paste("Simple regret by stack on opening", roll),
    subtitle = "Lower is better. This shows how the stack changes the cost of mistakes.",
    x = NULL,
    y = "simple_regret",
    fill = "Method"
  ) +
  results_plot_theme()

confidence_plot <- ggplot(
  final_summary,
  aes(x = stack, y = recommended_prob_best, fill = method)
) +
  geom_col(position = "dodge") +
  labs(
    title = paste("Posterior confidence by stack on opening", roll),
    subtitle = "This is the model-relative confidence in the recommended move.",
    x = NULL,
    y = "recommended_prob_best",
    fill = "Method"
  ) +
  results_plot_theme()

results_save_table(truth_stack_table, repo_root, "11_what_we_missed_truth_by_stack")
results_save_table(final_summary, repo_root, "11_what_we_missed_final_summary")
results_save_plot(top1_plot, repo_root, "11_what_we_missed_top1", width = 10, height = 6)
results_save_plot(regret_plot, repo_root, "11_what_we_missed_regret", width = 10, height = 6)
results_save_plot(confidence_plot, repo_root, "11_what_we_missed_confidence", width = 10, height = 6)

# Which truth object are we studying in each stack?
print(truth_stack_table)
print(final_summary)

# Does the stack change the final top-1 result?
print(top1_plot)

# Does the stack change the cost of mistakes?
print(regret_plot)

# Does the stack change how confident the method becomes?
print(confidence_plot)

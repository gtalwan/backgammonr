test_that("compare_methods_on_position returns method-level study output", {
  board <- bg_initial_board()
  roll <- bg_roll(1, 6)

  out <- compare_methods_on_position(
    board = board,
    roll = roll,
    methods = c("equal", "thompson"),
    total_budget = 24L,
    rollout_policy = "random",
    max_rollout_turns = 120L,
    fast_diagnostics = TRUE,
    seed = 1L
  )

  expect_s3_class(out, "bg_method_comparison")
  expect_true(all(c("method", "recommended_move_label", "recommended_estimate") %in% names(out$summary)))
  expect_equal(sort(out$summary$method), sort(c("equal", "thompson")))
  expect_equal(length(out$evaluations), 2L)
})

test_that("study_budget_tradeoff evaluates one method across budgets against truth", {
  board <- bg_initial_board()
  roll <- bg_roll(1, 6)

  out <- study_budget_tradeoff(
    board = board,
    roll = roll,
    method = "equal",
    budgets = c(12L, 24L),
    truth_budget = 48L,
    rollout_policy = "random",
    max_rollout_turns = 120L,
    fast_diagnostics = TRUE,
    seed = 2L
  )

  expect_s3_class(out, "bg_budget_tradeoff")
  expect_equal(nrow(out$results), 2L)
  expect_true(all(c("total_budget", "correct_selection", "simple_regret", "mse") %in% names(out$results)))
  expect_true(all(sort(out$results$total_budget) == c(12L, 24L)))
  expect_s3_class(out$truth, "bg_action_evaluation")
})

test_that("study_variance_controls crosses dice_mode and CRN settings", {
  board <- bg_initial_board()
  roll <- bg_roll(1, 6)

  out <- study_variance_controls(
    board = board,
    roll = roll,
    method = "thompson",
    total_budget = 24L,
    dice_modes = c("iid", "stratified_first_roll"),
    crn_values = c(FALSE, TRUE),
    truth_budget = 64L,
    rollout_policy = "random",
    max_rollout_turns = 120L,
    fast_diagnostics = TRUE,
    seed = 3L
  )

  expect_s3_class(out, "bg_variance_control_study")
  expect_equal(nrow(out$results), 4L)
  expect_true(all(c("dice_mode", "crn", "simple_regret", "mse") %in% names(out$results)))
})

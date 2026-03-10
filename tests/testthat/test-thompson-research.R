test_that("evaluate_actions_ttts returns coherent finite-budget output", {
  board <- bg_initial_board()
  roll <- bg_roll(1L, 6L)

  out <- evaluate_actions_ttts(
    board = board,
    roll = roll,
    total_budget = 64L,
    rollout_policy = "random",
    max_rollout_turns = 120L,
    seed = 1L
  )

  expect_s3_class(out, "bg_action_evaluation")
  expect_identical(out$method, "ttts")
  expect_equal(sum(out$results$allocation_count), 64L)
  expect_true(any(out$results$recommended))
})

test_that("trace_thompson_allocation returns non-empty trace and checkpoints", {
  board <- bg_initial_board()
  roll <- bg_roll(1L, 6L)

  tr <- trace_thompson_allocation(
    board = board,
    roll = roll,
    method = "thompson",
    total_budget = 64L,
    rollout_policy = "random",
    max_rollout_turns = 120L,
    trace_every = 8L,
    seed = 2L
  )

  expect_s3_class(tr, "bg_thompson_trace")
  expect_true(nrow(tr$trace) > 0L)
  expect_true(nrow(tr$checkpoint_summary) > 0L)
  expect_true(all(c(
    "checkpoint",
    "selected_candidate",
    "leader_index",
    "leader_estimate"
  ) %in% names(tr$checkpoint_summary)))
})

test_that("reference comparison and certification outputs are available", {
  board <- bg_initial_board()
  roll <- bg_roll(1L, 6L)

  cmp <- compare_thompson_to_reference(
    board = board,
    roll = roll,
    method = "ttts",
    total_budget = 96L,
    reference_budget = 512L,
    rollout_policy = "random",
    max_rollout_turns = 120L,
    seed = 3L
  )

  expect_s3_class(cmp, "bg_thompson_reference_comparison")
  expect_true(all(c(
    "method",
    "proxy_pcs",
    "simple_regret",
    "mse",
    "reference_certified"
  ) %in% names(cmp$summary)))
  expect_true(all(c(
    "candidate_index",
    "finite_estimate",
    "reference_estimate",
    "abs_error"
  ) %in% names(cmp$action_table)))

  cert <- certify_reference_truth(reference = cmp$reference)
  expect_s3_class(cert, "bg_reference_certificate")
  expect_true(all(c(
    "reference_best_index",
    "top_two_gap_estimate",
    "certified",
    "difficulty_label"
  ) %in% names(cert$certificate)))
})

test_that("benchmark_thompson and summarization produce Thompson-focused tables", {
  board <- bg_initial_board()
  roll <- bg_roll(1L, 6L)
  case1 <- bg_benchmark_case(board, roll, case_id = "init_1_6")

  bm <- benchmark_thompson(
    cases = list(case1),
    budgets = c(64L, 128L),
    reference_budget = 512L,
    rollout_policy = "random",
    max_rollout_turns = 120L,
    seed = 4L
  )

  expect_s3_class(bm, "bg_thompson_benchmark")
  expect_true("thompson" %in% bm$summary$method)
  expect_true(is.list(bm$thompson_focus))

  sm <- summarize_thompson_benchmark(bm)
  expect_s3_class(sm, "bg_thompson_benchmark_summary")
  expect_true(all(c(
    "method",
    "total_budget",
    "correct_selection_rate",
    "mean_simple_regret"
  ) %in% names(sm$thompson)))
})

test_that("Thompson plotting helpers return data without error", {
  board <- bg_initial_board()
  roll <- bg_roll(1L, 6L)

  tr <- trace_thompson_allocation(
    board = board,
    roll = roll,
    method = "thompson",
    total_budget = 48L,
    rollout_policy = "random",
    max_rollout_turns = 120L,
    trace_every = 8L,
    seed = 5L
  )

  case1 <- bg_benchmark_case(board, roll, case_id = "init_1_6")
  bm <- benchmark_thompson(
    cases = list(case1),
    budgets = c(64L, 128L),
    reference_budget = 512L,
    rollout_policy = "random",
    max_rollout_turns = 120L,
    seed = 6L
  )

  tf <- tempfile(fileext = ".pdf")
  grDevices::pdf(tf)
  d1 <- plot_thompson_convergence(tr, top_n = 3L, metric = "estimate")
  d2 <- plot_thompson_vs_baselines(bm, metric = "mean_simple_regret")
  grDevices::dev.off()

  expect_true(is.data.frame(d1))
  expect_true(is.data.frame(d2))
  expect_true(file.exists(tf))
})

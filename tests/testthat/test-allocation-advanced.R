make_sparse_points_alloc_adv <- function(...) {
  points <- integer(24)
  dots <- list(...)
  for (nm in names(dots)) {
    points[as.integer(nm)] <- as.integer(dots[[nm]])
  }
  points
}

make_board_alloc_adv <- function(points = integer(24), bar = c(0L, 0L), turn = 1L) {
  points <- as.integer(points)
  bar <- as.integer(bar)
  p1 <- sum(pmax(points, 0L)) + bar[1L]
  p2 <- sum(pmax(-points, 0L)) + bar[2L]
  if (p1 == 0L) {
    points[1L] <- points[1L] + 1L
    p1 <- 1L
  }
  if (p2 == 0L) {
    points[24L] <- points[24L] - 1L
    p2 <- 1L
  }
  bg_board(points = points, bar = bar, off = c(15L - p1, 15L - p2), turn = turn)
}

test_that("OCBA evaluation is available and respects budget", {
  board <- make_board_alloc_adv(points = make_sparse_points_alloc_adv(`8` = 1L, `17` = -1L), turn = 1L)
  roll <- bg_roll(3, 2)

  out <- evaluate_actions_ocba(board, roll = roll, total_budget = 10L, seed = 10)
  expect_s3_class(out, "bg_action_evaluation")
  expect_true("candidate_index" %in% names(out$results))
  expect_equal(sum(out$results$allocation_count), 10L)
})

test_that("trace output is deterministic and has expected columns", {
  board <- make_board_alloc_adv(points = make_sparse_points_alloc_adv(`8` = 1L, `17` = -1L), turn = 1L)
  roll <- bg_roll(3, 2)

  ev1 <- evaluate_actions_thompson(board, roll = roll, total_budget = 12L, trace = TRUE, trace_every = 1L, seed = 111)
  ev2 <- evaluate_actions_thompson(board, roll = roll, total_budget = 12L, trace = TRUE, trace_every = 1L, seed = 111)
  oc <- evaluate_actions_ocba(board, roll = roll, total_budget = 12L, trace = TRUE, trace_every = 1L, seed = 111)

  expect_equal(ev1$trace, ev2$trace)
  expect_true(all(c("checkpoint", "candidate_index", "allocation_count", "estimate", "selection_score", "leader_index") %in% names(ev1$trace)))
  expect_equal(max(ev1$trace$checkpoint), 12L)
  expect_true(nrow(oc$trace) > 0L)
})

test_that("stratified modes and CRN are reproducible", {
  board <- make_board_alloc_adv(points = make_sparse_points_alloc_adv(`8` = 1L, `17` = -1L), turn = 1L)
  roll <- bg_roll(3, 2)

  a1 <- evaluate_actions_ucb(
    board,
    roll = roll,
    total_budget = 16L,
    dice_mode = "stratified_first_roll",
    seed = 91
  )
  a2 <- evaluate_actions_ucb(
    board,
    roll = roll,
    total_budget = 16L,
    dice_mode = "stratified_first_roll",
    seed = 91
  )
  expect_equal(a1$results, a2$results)

  legal <- bg_legal_moves(board, roll)
  b1 <- evaluate_actions_equal(board, legal_moves = legal, total_budget = 12L, crn = TRUE, seed = 5)
  b2 <- evaluate_actions_equal(board, legal_moves = rev(legal), total_budget = 12L, crn = TRUE, seed = 5)
  expect_equal(sort(b1$results$estimate), sort(b2$results$estimate), tolerance = 1e-12)
})

test_that("extended benchmark grid returns rectangular outputs", {
  board <- make_board_alloc_adv(points = make_sparse_points_alloc_adv(`8` = 1L, `17` = -1L), turn = 1L)
  case <- bg_benchmark_case(board = board, roll = bg_roll(3, 2), case_id = "grid_case")

  bm <- benchmark_allocation_methods(
    cases = list(case),
    methods = c("equal", "ocba"),
    budgets = c(4L, 8L),
    dice_modes = c("iid", "stratified_first_roll"),
    crn_values = c(FALSE, TRUE),
    truth_budget = 32L,
    seed = 99
  )

  expect_s3_class(bm, "bg_allocation_benchmark")
  expect_true(all(c("method", "total_budget", "dice_mode", "crn", "simple_regret", "mse") %in% names(bm$results)))
  expect_true(all(c("method", "total_budget", "dice_mode", "crn", "probability_correct_selection") %in% names(bm$summary)))
})

test_that("metric aliases are available", {
  expect_equal(compute_probability_of_correct_selection(c(1L, 2L), c(1L, 1L)), 0.5)
  expect_equal(compute_simple_regret(0.4, 0.6), 0.2)
  expect_equal(compute_value_mse(c(0.2, 0.4), c(0.1, 0.5)), compute_mse(c(0.2, 0.4), c(0.1, 0.5)))
})

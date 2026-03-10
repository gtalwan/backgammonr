make_sparse_points_allocation <- function(...) {
  points <- integer(24)
  dots <- list(...)

  for (nm in names(dots)) {
    points[as.integer(nm)] <- as.integer(dots[[nm]])
  }

  points
}

make_test_board_allocation <- function(points = integer(24), bar = c(0L, 0L), turn = 1L) {
  points <- as.integer(points)
  bar <- as.integer(bar)
  p1_on_board <- sum(pmax(points, 0L)) + bar[1L]
  p2_on_board <- sum(pmax(-points, 0L)) + bar[2L]

  if (p1_on_board == 0L) {
    points[1L] <- points[1L] + 1L
    p1_on_board <- 1L
  }
  if (p2_on_board == 0L) {
    points[24L] <- points[24L] - 1L
    p2_on_board <- 1L
  }

  off <- c(
    15L - p1_on_board,
    15L - p2_on_board
  )

  bg_board(points = points, bar = bar, off = off, turn = turn)
}

test_that("research-facing evaluation functions return structured action-evaluation objects", {
  board <- make_test_board_allocation(
    points = make_sparse_points_allocation(`8` = 1L, `17` = -1L),
    turn = 1L
  )
  roll <- bg_roll(3, 2)

  equal_eval <- evaluate_actions_equal(board, roll = roll, total_budget = 8L, seed = 123)
  greedy_eval <- evaluate_actions_greedy(board, roll = roll, total_budget = 8L, seed = 123)
  ucb_eval <- evaluate_actions_ucb(board, roll = roll, total_budget = 8L, seed = 123)
  thompson_eval <- evaluate_actions_thompson(board, roll = roll, total_budget = 8L, seed = 123)

  expect_s3_class(equal_eval, "bg_action_evaluation")
  expect_s3_class(greedy_eval, "bg_action_evaluation")
  expect_s3_class(ucb_eval, "bg_action_evaluation")
  expect_s3_class(thompson_eval, "bg_action_evaluation")

  expect_true(all(c(
    "candidate_index", "n_equivalent_sequences", "allocation_count", "estimate", "posterior_sd",
    "lower_95", "upper_95", "prob_best", "posterior_expected_regret",
    "move_label", "move", "recommended"
  ) %in% names(equal_eval$results)))
  expect_equal(sum(equal_eval$results$allocation_count), 8L)
  expect_equal(sum(greedy_eval$results$allocation_count), 8L)
  expect_equal(sum(ucb_eval$results$allocation_count), 8L)
  expect_equal(sum(thompson_eval$results$allocation_count), 8L)
  expect_equal(sum(equal_eval$results$prob_best), 1, tolerance = 1e-8)
  expect_true(is_bg_move_sequence(equal_eval$recommended_move))
  expect_output(print(equal_eval), "<bg_action_evaluation>")
})

test_that("equivalent legal sequences are collapsed by resulting board", {
  board <- make_test_board_allocation(
    points = make_sparse_points_allocation(`8` = 1L, `17` = -1L),
    turn = 1L
  )
  roll <- bg_roll(3, 2)
  legal_moves <- bg_legal_moves(board, roll)
  expect_equal(length(legal_moves), 2L)

  equal_eval <- evaluate_actions_equal(board, roll = roll, total_budget = 8L, seed = 123)

  expect_equal(nrow(equal_eval$results), 1L)
  expect_equal(equal_eval$results$n_equivalent_sequences, 2L)
  expect_true(is_bg_move_sequence(equal_eval$recommended_move))
})

test_that("evaluation objects are reproducible with the same seed", {
  board <- make_test_board_allocation(
    points = make_sparse_points_allocation(`8` = 1L, `17` = -1L),
    turn = 1L
  )
  roll <- bg_roll(3, 2)

  ev1 <- evaluate_actions_thompson(board, roll = roll, total_budget = 10L, seed = 999)
  ev2 <- evaluate_actions_thompson(board, roll = roll, total_budget = 10L, seed = 999)

  expect_equal(ev1$results, ev2$results)
  expect_identical(ev1$recommended_index, ev2$recommended_index)
  expect_equal(ev1$recommended_move, ev2$recommended_move)
})

test_that("approximate truth, regret, accuracy, MSE, and difficulty helpers work", {
  board <- make_test_board_allocation(
    points = make_sparse_points_allocation(`8` = 1L, `17` = -1L),
    turn = 1L
  )
  roll <- bg_roll(3, 2)

  truth <- approximate_action_truth(board, roll = roll, truth_budget = 32L, seed = 123)
  eval <- evaluate_actions_equal(board, roll = roll, total_budget = 8L, seed = 123)

  expect_s3_class(truth, "bg_action_evaluation")
  expect_true(inherits(truth, "bg_action_truth"))

  truth_tab <- truth$results[order(truth$results$candidate_index), , drop = FALSE]
  eval_tab <- eval$results[order(eval$results$candidate_index), , drop = FALSE]

  best_index <- truth_tab$candidate_index[[which.max(truth_tab$estimate)]]
  selected_truth_value <- truth_tab$estimate[[eval$recommended_index]]
  best_truth_value <- max(truth_tab$estimate)

  expect_true(is.numeric(compute_regret(selected_truth_value, best_truth_value)))
  expect_equal(compute_best_action_accuracy(c(eval$recommended_index, best_index), c(best_index, best_index)), 1)
  expect_true(compute_mse(eval_tab$estimate, truth_tab$estimate) >= 0)

  difficulty <- stratify_positions_by_difficulty(c(0.01, 0.03, 0.10))
  expect_identical(as.character(difficulty), c("hard", "moderate", "easy"))
})

test_that("benchmark_allocation_methods returns case-level and summary outputs", {
  board <- make_test_board_allocation(
    points = make_sparse_points_allocation(`8` = 1L, `17` = -1L),
    turn = 1L
  )

  case <- bg_benchmark_case(
    board = board,
    roll = bg_roll(3, 2),
    case_id = "allocation_demo"
  )

  bm <- benchmark_allocation_methods(
    cases = list(case),
    methods = c("equal", "thompson"),
    total_budget = 8L,
    truth_budget = 32L,
    seed = 123
  )

  expect_s3_class(bm, "bg_allocation_benchmark")
  expect_true(all(c(
    "case_id", "method", "chosen_index", "truth_best_index", "correct_selection",
    "simple_regret", "mse", "runtime_seconds"
  ) %in% names(bm$results)))
  expect_true(all(c(
    "method", "n_cases", "decision_cases", "probability_correct_selection",
    "mean_simple_regret", "mean_mse", "mean_runtime_seconds"
  ) %in% names(bm$summary)))
  expect_true(all(c(
    "case_id", "truth_best_index", "truth_best_value", "difficulty_gap", "difficulty_label"
  ) %in% names(bm$truth)))
  expect_output(print(bm), "<bg_allocation_benchmark>")
})

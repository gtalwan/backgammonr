test_that("research alias constructors and mechanics wrappers work", {
  points <- integer(24)
  points[8] <- 1L
  points[17] <- -1L

  state <- initialize_board(points = points, off = c(14L, 14L), turn = 1L)
  expect_s3_class(state, "bg_board")
  expect_true(validate_board(state))
  expect_output(print_board(state), "<bg_board_ascii>")

  roll <- roll_dice(seed = 1)
  expect_true(is_bg_roll(roll))
  moves <- generate_legal_moves(state, roll)
  expect_true(is.list(moves))
  move_tbl <- summarize_legal_moves(moves, max_candidates = 3L, print_table = FALSE)
  expect_s3_class(move_tbl, "data.frame")
  expect_true(all(c("candidate_index", "move_label", "n_steps", "dice_used") %in% names(move_tbl)))
  expect_true(is.numeric(attr(move_tbl, "n_total_candidates")))
  if (length(moves) > 0L) {
    next_state <- apply_move(state, moves[[1L]])
    expect_s3_class(next_state, "bg_board")
  }
})

test_that("research alias evaluators and diagnostics return expected shapes", {
  points <- integer(24)
  points[8] <- 1L
  points[17] <- -1L
  state <- initialize_board(points = points, off = c(14L, 14L), turn = 1L)
  dice <- bg_roll(3, 2)

  cand <- enumerate_candidate_moves(state, dice)
  expect_true(length(cand) >= 1L)

  one <- estimate_action_value(state, cand[[1L]], n_rollouts = 8L, seed = 1)
  expect_s3_class(one, "data.frame")
  expect_true(all(c("estimate", "allocation_count") %in% names(one)))

  eq <- evaluate_moves_equal_allocation(state, dice, budget = 16L, seed = 1)
  ts <- evaluate_moves_thompson(state, dice, budget = 16L, trace = TRUE, seed = 1)
  ucb <- evaluate_moves_ucb(state, dice, budget = 16L, seed = 1)
  se <- evaluate_moves_successive_elimination(state, dice, budget = 16L, seed = 1)

  expect_s3_class(eq, "bg_action_evaluation")
  expect_s3_class(ts, "bg_action_evaluation")
  expect_s3_class(ucb, "bg_action_evaluation")
  expect_s3_class(se, "bg_action_evaluation")
  expect_true(nrow(trace_allocation_history(ts)) > 0L)
  expect_true(is.character(explain_position(state, dice)))
  expect_true(is.character(explain_move_evaluation(ts)))
  expect_s3_class(compare_action_posteriors(ts), "data.frame")
})

test_that("benchmark aliases integrate with benchmark objects", {
  points <- integer(24)
  points[8] <- 1L
  points[17] <- -1L
  state <- initialize_board(points = points, off = c(14L, 14L), turn = 1L)
  dice <- bg_roll(3, 2)

  ref <- identify_reference_best_move(state, dice, large_budget = 32L, seed = 2)
  expect_true(is.list(ref))
  expect_true(!is.null(ref$reference_index))

  case <- bg_benchmark_case(board = state, roll = dice, case_id = "alias_case")
  bm <- benchmark_evaluators(
    test_positions = list(case),
    budgets = c(8L, 16L),
    methods = c("equal", "thompson"),
    truth_budget = 64L,
    seed = 2
  )

  expect_s3_class(bm, "bg_allocation_benchmark")
  expect_s3_class(summarize_benchmark_results(bm), "data.frame")
  expect_no_error(plot_benchmark_results(bm, metric = "probability_correct_selection"))
  expect_no_error(plot_budget_accuracy_curve(bm))
  expect_no_error(plot_runtime_curve(bm))
})

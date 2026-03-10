make_sparse_points_vis <- function(...) {
  points <- integer(24)
  dots <- list(...)
  for (nm in names(dots)) {
    points[as.integer(nm)] <- as.integer(dots[[nm]])
  }
  points
}

test_that("board formatting and printing are available", {
  board <- bg_initial_board()
  txt <- format(board)

  expect_true(is.character(txt))
  expect_true(length(txt) == 1L)
  expect_match(txt, "<bg_board_ascii>")
  expect_output(print(board), "<bg_board_ascii>")
})

test_that("board and move plotting helpers run without error", {
  points <- make_sparse_points_vis(`8` = 1L, `17` = -1L)
  board <- bg_position(points = points, off = c(14L, 14L), turn = 1L)
  roll <- bg_roll(3, 2)
  moves <- bg_legal_moves(board, roll)
  expect_true(length(moves) > 0L)

  f <- tempfile(fileext = ".png")
  grDevices::png(filename = f, width = 1200, height = 900)
  expect_no_error(bg_plot_board(board))
  expect_no_error(bg_plot_move(board, moves[[1L]]))
  expect_no_error(bg_plot_legal_moves(board, roll, top_n = 2L, method = "thompson", total_budget = 8L, seed = 1))
  grDevices::dev.off()
})

test_that("ranking, trace, benchmark, and report plotting helpers run", {
  points <- make_sparse_points_vis(`8` = 1L, `17` = -1L)
  board <- bg_position(points = points, off = c(14L, 14L), turn = 1L)
  roll <- bg_roll(3, 2)

  ev <- evaluate_actions_thompson(board, roll = roll, total_budget = 12L, trace = TRUE, seed = 12)
  rec <- bg_recommend_move(board, roll, method = "thompson", total_budget = 12L, trace = TRUE, seed = 12)
  rep <- bg_analysis_report(board, roll, method = "thompson", total_budget = 12L, trace = TRUE, seed = 12)

  f <- tempfile(fileext = ".png")
  grDevices::png(filename = f, width = 1400, height = 900)
  expect_no_error(bg_plot_move_ranking(ev))
  expect_no_error(bg_plot_move_ranking(rec))
  expect_no_error(bg_plot_allocation_trace(ev))
  expect_no_error(plot(rep))
  stability <- bg_plot_budget_stability(board, roll, budgets = c(4L, 8L), methods = c("equal", "ocba"), seed = 9)
  expect_s3_class(stability, "data.frame")

  case <- bg_benchmark_case(board = board, roll = roll, case_id = "plot_case")
  bm <- benchmark_allocation_methods(
    cases = list(case),
    methods = c("equal", "ocba"),
    budgets = c(4L, 8L),
    truth_budget = 32L,
    seed = 21
  )
  expect_no_error(bg_plot_benchmark_summary(bm, metric = "probability_correct_selection"))
  grDevices::dev.off()
})

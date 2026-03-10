make_sparse_points_recommend <- function(...) {
  points <- integer(24)
  dots <- list(...)

  for (nm in names(dots)) {
    points[as.integer(nm)] <- as.integer(dots[[nm]])
  }

  points
}

test_that("bg_position is a user-friendly alias for bg_board", {
  points <- make_sparse_points_recommend(`8` = 1L, `17` = -1L)
  pos <- bg_position(points = points, off = c(14L, 14L), turn = 1L)
  ref <- bg_board(points = points, off = c(14L, 14L), turn = 1L)

  expect_s3_class(pos, "bg_board")
  expect_equal(pos, ref)
})

test_that("gamer-facing ranking and recommendation helpers return useful outputs", {
  points <- make_sparse_points_recommend(`8` = 1L, `17` = -1L)
  board <- bg_position(points = points, off = c(14L, 14L), turn = 1L)
  roll <- bg_roll(3, 2)

  ranking <- bg_rank_moves(board, roll, method = "thompson", total_budget = 8L, seed = 123)
  rec <- bg_recommend_move(board, roll, method = "thompson", total_budget = 8L, seed = 123)

  expect_s3_class(ranking, "data.frame")
  expect_true(all(c("rank", "recommended", "move_label", "estimate", "move") %in% names(ranking)))
  expect_equal(sum(ranking$recommended), 1L)
  expect_true(is_bg_move_sequence(ranking$move[[1L]]))

  expect_s3_class(rec, "bg_move_recommendation")
  expect_true(is_bg_move_sequence(rec$recommended_move))
  expect_true(is.character(rec$explanation))
  expect_true(length(rec$explanation) == 1L)
  expect_output(print(rec), "<bg_move_recommendation>")
})

test_that("bg_explain_recommendation works for recommendation objects and direct calls", {
  points <- make_sparse_points_recommend(`8` = 1L, `17` = -1L)
  board <- bg_position(points = points, off = c(14L, 14L), turn = 1L)
  roll <- bg_roll(3, 2)

  rec <- bg_recommend_move(board, roll, method = "thompson", total_budget = 8L, seed = 123)
  explanation1 <- bg_explain_recommendation(rec)
  explanation2 <- bg_explain_recommendation(board, roll = roll, method = "thompson", total_budget = 8L, seed = 123)

  expect_identical(explanation1, rec$explanation)
  expect_identical(explanation1, explanation2)
})


test_that("recommendation helpers handle forced pass positions", {
  points <- integer(24)
  points[19:24] <- -2L
  board <- bg_position(points = points, bar = c(1L, 0L), off = c(14L, 3L), turn = 1L)
  roll <- bg_roll(3, 2)

  ranking <- bg_rank_moves(board, roll, method = "equal", total_budget = 8L, seed = 123)
  rec <- bg_recommend_move(board, roll, method = "equal", total_budget = 8L, seed = 123)

  expect_s3_class(ranking, "data.frame")
  expect_equal(nrow(ranking), 0L)

  expect_s3_class(rec, "bg_move_recommendation")
  expect_null(rec$recommended_move)
  expect_match(rec$explanation, "must pass")
  expect_output(print(rec), "<pass>")
  expect_match(bg_explain_recommendation(rec), "must pass")
})

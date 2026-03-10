test_that("initial board has the expected structure and totals", {
  board <- bg_initial_board()

  expect_s3_class(board, "bg_board")
  expect_true(is_bg_board(board))
  expect_false(is_bg_board(unclass(board)))
  expect_identical(names(board), c("points", "bar", "off", "turn"))
  expect_type(board$points, "integer")
  expect_length(board$points, 24L)
  expect_identical(board$bar, c(0L, 0L))
  expect_identical(board$off, c(0L, 0L))
  expect_identical(board$turn, 1L)

  expect_equal(sum(pmax(board$points, 0L)) + board$bar[1L] + board$off[1L], 15L)
  expect_equal(sum(pmax(-board$points, 0L)) + board$bar[2L] + board$off[2L], 15L)
  expect_true(bg_validate_board(board))
})

test_that("bg_board constructor normalizes integer-like numeric inputs", {
  board <- bg_board(
    points = c(-2, rep(0, 4), 5, 0, 3, rep(0, 3), -5, 5, rep(0, 3), -3, 0, -5, rep(0, 4), 2),
    bar = c(0, 0),
    off = c(0, 0),
    turn = 1
  )

  expect_s3_class(board, "bg_board")
  expect_type(board$points, "integer")
  expect_type(board$bar, "integer")
  expect_type(board$off, "integer")
  expect_type(board$turn, "integer")
  expect_true(bg_validate_board(board))
})

test_that("initial board can be created for player -1", {
  board <- bg_initial_board(turn = -1L)

  expect_identical(board$turn, -1L)
  expect_true(bg_validate_board(board))
})

test_that("clone returns an independent board object and preserves extra fields", {
  board <- bg_initial_board()
  board$meta <- list(stage = "board-layer")

  clone <- bg_clone_board(board)

  expect_s3_class(clone, "bg_board")
  expect_equal(clone, board)
  expect_equal(clone$meta, board$meta)

  clone$bar[1] <- 1L
  clone$points[24] <- clone$points[24] - 1L
  clone$points[13] <- clone$points[13] - 1L
  clone$off[1] <- 1L

  expect_identical(board$bar, c(0L, 0L))
  expect_identical(board$off, c(0L, 0L))
  expect_equal(board$points[24], 2L)
  expect_true(bg_validate_board(clone))
})

test_that("validation reports invalid checker totals", {
  board <- bg_initial_board()
  broken <- unclass(board)
  broken$points[24] <- broken$points[24] - 1L
  class(broken) <- "bg_board"

  report <- bg_validate_board(broken, error = FALSE)

  expect_false(report$valid)
  expect_true(any(grepl("Player 1 must have exactly 15 total checkers", report$messages)))
  expect_error(bg_validate_board(broken), "Player 1 must have exactly 15 total checkers")
})

test_that("validation reports malformed structural fields", {
  board <- bg_initial_board()
  broken <- unclass(board)
  broken$bar <- c(-1L, 0L)
  class(broken) <- "bg_board"

  report <- bg_validate_board(broken, error = FALSE)
  expect_false(report$valid)
  expect_true(any(grepl("`bar` must be nonnegative", report$messages, fixed = TRUE)))

  malformed <- list(points = integer(24), bar = c(0, 0), off = c(0L, 0L), turn = 1L)
  report2 <- bg_validate_board(malformed, error = FALSE)
  expect_false(report2$valid)
  expect_true(any(grepl("`bar` must be stored as an integer vector", report2$messages, fixed = TRUE)))
})

test_that("validation catches missing fields, NAs, point bounds, and invalid turn", {
  missing_field <- list(points = integer(24), bar = c(0L, 0L), off = c(0L, 0L))
  report1 <- bg_validate_board(missing_field, error = FALSE)
  expect_false(report1$valid)
  expect_true(any(grepl("Missing required field `turn`", report1$messages, fixed = TRUE)))

  board <- bg_initial_board()
  bad_na <- unclass(board)
  bad_na$points[1] <- NA_integer_
  class(bad_na) <- "bg_board"
  report2 <- bg_validate_board(bad_na, error = FALSE)
  expect_false(report2$valid)
  expect_true(any(grepl("`points` cannot contain `NA` values", report2$messages, fixed = TRUE)))

  bad_bound <- unclass(board)
  bad_bound$points[1] <- -16L
  bad_bound$points[12] <- -4L
  class(bad_bound) <- "bg_board"
  report3 <- bg_validate_board(bad_bound, error = FALSE)
  expect_false(report3$valid)
  expect_true(any(grepl("Absolute checker count at point 1 cannot exceed 15", report3$messages, fixed = TRUE)))

  bad_turn <- unclass(board)
  bad_turn$turn <- 0L
  class(bad_turn) <- "bg_board"
  report4 <- bg_validate_board(bad_turn, error = FALSE)
  expect_false(report4$valid)
  expect_true(any(grepl("`turn` must be either 1L or -1L", report4$messages, fixed = TRUE)))
})

test_that("validation handles non-list input and validates error flag", {
  report <- bg_validate_board(42, error = FALSE)
  expect_false(report$valid)
  expect_true(any(grepl("`board` must be a `bg_board` object", report$messages, fixed = TRUE)))

  expect_error(bg_validate_board(bg_initial_board(), error = NA), "`error` must be TRUE or FALSE", fixed = TRUE)
})

test_that("constructor rejects non-whole numbers and invalid validate flag", {
  expect_error(
    bg_board(points = c(rep(0, 23), 1.5), turn = 1L),
    "`points` must contain whole numbers only",
    fixed = TRUE
  )

  expect_error(
    bg_board(points = integer(24), turn = 1L, validate = NA),
    "`validate` must be TRUE or FALSE",
    fixed = TRUE
  )
})

test_that("inspect returns a structured summary and print works", {
  board <- bg_initial_board()
  board$meta <- list(tag = "fixture")

  info <- bg_inspect_board(board)

  expect_identical(info$turn, "player_1")
  expect_identical(unname(info$bar), c(0L, 0L))
  expect_identical(unname(info$off), c(0L, 0L))
  expect_equal(nrow(info$points), 24L)
  expect_identical(info$points$point, seq_len(24L))
  expect_true(all(c("point", "signed_count", "owner", "n_checkers") %in% names(info$points)))
  expect_identical(info$extra_fields, "meta")

  expect_output(print(board), "<bg_board>")
  expect_output(print(board), "occupied points")
})


test_that("validation rejects impossible double-terminal positions", {
  board <- bg_board(
    points = integer(24),
    bar = c(0L, 0L),
    off = c(15L, 15L),
    turn = 1L,
    validate = FALSE
  )

  report <- bg_validate_board(board, error = FALSE)
  expect_false(report$valid)
  expect_true(any(grepl("Both players cannot simultaneously have all 15 checkers borne off", report$messages, fixed = TRUE)))
  expect_error(
    bg_validate_board(board),
    "Both players cannot simultaneously have all 15 checkers borne off",
    fixed = TRUE
  )
})

test_that("bg_print_board renders a readable ASCII board diagram", {
  board <- bg_initial_board()

  expect_output(bg_print_board(board), "<bg_board_ascii>")
  expect_output(bg_print_board(board), "Top \\(24 -> 13\\)")
  expect_output(bg_print_board(board), "Bottom \\(12 -> 1\\)")
  expect_output(bg_print_board(board), "X5")
  expect_output(bg_print_board(board), "O5")
  returned <- NULL
  invisible(capture.output({
    returned <- bg_print_board(board)
  }))
  expect_identical(returned, board)
})

test_that("bg_print_board validates inputs", {
  expect_error(
    bg_print_board(1),
    "`board` must inherit from class 'bg_board'.",
    fixed = TRUE
  )
  expect_error(
    bg_print_board(bg_initial_board(), show_indices = NA),
    "`show_indices` must be TRUE or FALSE.",
    fixed = TRUE
  )
})

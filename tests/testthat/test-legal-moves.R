make_sparse_points <- function(...) {
  points <- integer(24)
  dots <- list(...)

  for (nm in names(dots)) {
    points[as.integer(nm)] <- as.integer(dots[[nm]])
  }

  points
}

make_test_board <- function(points = integer(24), bar = c(0L, 0L), turn = 1L) {
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

compact_move_sequences <- function(moves) {
  lapply(moves, function(sequence) {
    vapply(sequence$steps, function(step) {
      paste(step$from, step$to, step$die, if (isTRUE(step$hit)) 1L else 0L, sep = ":")
    }, character(1L))
  })
}

test_that("legal move generator returns all full-turn sequences for a simple non-home position", {
  board <- make_test_board(
    points = make_sparse_points(`8` = 1L),
    turn = 1L
  )
  moves <- bg_legal_moves(board, bg_roll(3, 2))

  expect_length(moves, 2L)
  expect_true(all(vapply(moves, is_bg_move_sequence, logical(1L))))
  expect_true(all(vapply(moves, function(x) x$n_steps, integer(1L)) == 2L))
  actual <- vapply(compact_move_sequences(moves), paste, collapse = "|", FUN.VALUE = character(1L))
  expected <- c("8:5:3:0|5:3:2:0", "8:6:2:0|6:3:3:0")
  expect_setequal(actual, expected)
})

test_that("blocked points and forced use of both dice are handled", {
  board <- make_test_board(
    points = make_sparse_points(`8` = 1L, `5` = -2L),
    turn = 1L
  )
  moves <- bg_legal_moves(board, bg_roll(3, 2))

  expect_length(moves, 1L)
  expect_identical(compact_move_sequences(moves)[[1L]], c("8:6:2:0", "6:3:3:0"))
})

test_that("hitting blots is represented in the generated steps", {
  board <- make_test_board(
    points = make_sparse_points(`8` = 1L, `6` = -2L, `5` = -1L),
    turn = 1L
  )
  moves <- bg_legal_moves(board, bg_roll(3, 2))

  expect_length(moves, 1L)
  expect_identical(compact_move_sequences(moves)[[1L]], c("8:5:3:1", "5:3:2:0"))
  expect_true(isTRUE(moves[[1L]]$steps[[1L]]$hit))
})

test_that("checkers on the bar must enter before any other move", {
  board <- make_test_board(
    points = make_sparse_points(`24` = -2L),
    bar = c(1L, 0L),
    turn = 1L
  )
  moves <- bg_legal_moves(board, bg_roll(3, 1))

  expect_length(moves, 1L)
  expect_identical(compact_move_sequences(moves)[[1L]], c("0:22:3:0", "22:21:1:0"))
})

test_that("no legal bar entry produces an empty move set", {
  board <- make_test_board(
    points = make_sparse_points(`24` = -2L, `22` = -2L),
    bar = c(1L, 0L),
    turn = 1L
  )
  moves <- bg_legal_moves(board, bg_roll(3, 1))

  expect_identical(moves, list())
})

test_that("higher die rule is enforced when only one die can be played", {
  board <- make_test_board(
    points = make_sparse_points(`22` = -2L),
    bar = c(1L, 0L),
    turn = 1L
  )
  moves <- bg_legal_moves(board, bg_roll(1, 2))

  expect_length(moves, 1L)
  expect_identical(compact_move_sequences(moves)[[1L]], c("0:23:2:0"))
})

test_that("doubles generate four-step sequences when all four dice can be used", {
  board <- make_test_board(
    points = make_sparse_points(`8` = 1L),
    turn = 1L
  )
  moves <- bg_legal_moves(board, bg_roll(2, 2))

  expect_length(moves, 1L)
  expect_identical(
    compact_move_sequences(moves)[[1L]],
    c("8:6:2:0", "6:4:2:0", "4:2:2:0", "2:25:2:0")
  )
  expect_identical(moves[[1L]]$dice_used, c(2L, 2L, 2L, 2L))
})

test_that("bearing off obeys the highest-point overshoot restriction", {
  board <- make_test_board(
    points = make_sparse_points(`6` = 1L, `5` = 1L),
    turn = 1L
  )
  moves <- bg_legal_moves(board, bg_roll(6, 6))

  expect_length(moves, 1L)
  expect_identical(compact_move_sequences(moves)[[1L]], c("6:25:6:0", "5:25:6:0"))
})

test_that("generator works for player -1 movement direction", {
  board <- make_test_board(
    points = make_sparse_points(`17` = -1L),
    turn = -1L
  )
  moves <- bg_legal_moves(board, bg_roll(3, 2))

  expect_length(moves, 2L)
  expect_equal(
    compact_move_sequences(moves),
    list(
      c("17:19:2:0", "19:22:3:0"),
      c("17:20:3:0", "20:22:2:0")
    )
  )
})

test_that("player defaults to board turn and explicit player overrides are accepted", {
  board <- make_test_board(
    points = make_sparse_points(`8` = 1L),
    turn = 1L
  )

  moves_default <- bg_legal_moves(board, bg_roll(3, 2))
  moves_explicit <- bg_legal_moves(board, bg_roll(3, 2), player = 1L)

  expect_equal(compact_move_sequences(moves_default), compact_move_sequences(moves_explicit))
})

test_that("legal move generator validates board, roll, and player inputs", {
  board <- make_test_board(points = make_sparse_points(`8` = 1L), turn = 1L)

  expect_error(bg_legal_moves(unclass(board), bg_roll(3, 2)), "`board` must inherit from class 'bg_board'", fixed = TRUE)
  expect_error(bg_legal_moves(board, list(foo = 1)), "A roll-like list must contain a `dice` field", fixed = TRUE)
  expect_error(bg_legal_moves(board, bg_roll(3, 2), player = 0), "`player` must be either 1L or -1L", fixed = TRUE)
})

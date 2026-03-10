make_engine_points <- function(...) {
  points <- integer(24)
  dots <- list(...)

  for (nm in names(dots)) {
    points[as.integer(nm)] <- as.integer(dots[[nm]])
  }

  points
}

make_engine_board <- function(points = integer(24), bar = c(0L, 0L), turn = 1L) {
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

test_that("bg_apply_move_sequence applies a legal sequence and flips the turn", {
  board <- make_engine_board(
    points = make_engine_points(`8` = 1L, `5` = -2L),
    turn = 1L
  )
  move <- bg_legal_moves(board, bg_roll(3, 2))[[1L]]

  after <- bg_apply_move_sequence(board, move)

  expect_s3_class(after, "bg_board")
  expect_identical(after$turn, -1L)
  expect_identical(after$points[8], 0L)
  expect_identical(after$points[6], 0L)
  expect_identical(after$points[3], 1L)
  expect_identical(after$off, board$off)
})

test_that("bg_apply_move_sequence rejects illegal or roll-free sequences", {
  board <- make_engine_board(
    points = make_engine_points(`8` = 1L, `5` = -2L),
    turn = 1L
  )
  no_roll <- bg_move_sequence(
    player = 1,
    steps = list(
      bg_move_step(8, 6, 2),
      bg_move_step(6, 3, 3)
    )
  )

  expect_error(
    bg_apply_move_sequence(board, no_roll),
    "`move_sequence` must include a `roll` field",
    fixed = TRUE
  )

  bad <- bg_move_sequence(
    player = 1,
    roll = bg_roll(3, 2),
    steps = list(
      bg_move_step(8, 5, 3),
      bg_move_step(5, 3, 2)
    )
  )

  expect_error(
    bg_apply_move_sequence(board, bad),
    "`move_sequence` is not legal for the supplied board and roll",
    fixed = TRUE
  )
})

test_that("bg_play_turn passes when no legal move exists", {
  board <- make_engine_board(
    points = make_engine_points(`24` = -2L, `22` = -2L),
    bar = c(1L, 0L),
    turn = 1L
  )

  result <- bg_play_turn(board, roll = bg_roll(3, 1))

  expect_s3_class(result, "bg_turn_result")
  expect_true(is_bg_turn_result(result))
  expect_identical(result$player, 1L)
  expect_identical(result$n_legal_moves, 0L)
  expect_true(result$turn_passed)
  expect_null(result$chosen_move)
  expect_equal(result$board_before$points, board$points)
  expect_equal(result$board_before$bar, board$bar)
  expect_equal(result$board_after$points, board$points)
  expect_equal(result$board_after$bar, board$bar)
  expect_identical(result$board_after$turn, -1L)
  expect_false(result$game_over)
})

test_that("bg_play_turn can terminate the game", {
  board <- make_engine_board(
    points = make_engine_points(`1` = 1L, `24` = -1L),
    turn = 1L
  )

  result <- bg_play_turn(board, roll = bg_roll(1, 1))

  expect_s3_class(result, "bg_turn_result")
  expect_true(is_bg_turn_result(result))
  expect_false(result$turn_passed)
  expect_true(is_bg_move_sequence(result$chosen_move))
  expect_true(result$game_over)
  expect_identical(result$winner, 1L)
  expect_identical(result$board_after$off[1], 15L)
  expect_identical(result$board_after$points[1], 0L)
})

test_that("bg_play_game alternates players and records history", {
  board <- make_engine_board(
    points = make_engine_points(`8` = 1L, `17` = -1L),
    turn = 1L
  )

  game <- bg_play_game(
    board,
    roll_sequence = list(bg_roll(1, 1), bg_roll(1, 1)),
    max_turns = 2L
  )

  expect_s3_class(game, "bg_game_result")
  expect_true(is_bg_game_result(game))
  expect_length(game$turns, 2L)
  expect_identical(vapply(game$turns, function(x) x$player, integer(1L)), c(1L, -1L))
  expect_equal(game$history$player, c(1L, -1L))
  expect_identical(game$n_turns, 2L)
  expect_false(game$game_over)
  expect_true(game$turn_limit_reached)
  expect_identical(game$final_board$turn, 1L)
})

test_that("bg_play_game terminates on a scripted winning roll", {
  board <- make_engine_board(
    points = make_engine_points(`1` = 1L, `24` = -1L),
    turn = 1L
  )

  game <- bg_play_game(
    board,
    roll_sequence = list(bg_roll(1, 1)),
    max_turns = 10L
  )

  expect_true(game$game_over)
  expect_identical(game$winner, 1L)
  expect_identical(game$n_turns, 1L)
  expect_false(game$turn_limit_reached)
  expect_false(game$roll_sequence_exhausted)
  expect_identical(game$final_board$off[1], 15L)
})

test_that("bg_play_game returns a partial result when scripted rolls are exhausted", {
  board <- make_engine_board(
    points = make_engine_points(`8` = 1L, `17` = -1L),
    turn = 1L
  )

  game <- bg_play_game(
    board,
    roll_sequence = list(bg_roll(1, 1)),
    max_turns = 10L
  )

  expect_false(game$game_over)
  expect_true(game$roll_sequence_exhausted)
  expect_identical(game$n_turns, 1L)
})

test_that("bg_play_game handles already terminal starting positions", {
  board <- bg_board(
    points = make_engine_points(`24` = -1L),
    off = c(15L, 14L),
    turn = -1L
  )

  game <- bg_play_game(board, max_turns = 10L)

  expect_true(game$game_over)
  expect_identical(game$winner, 1L)
  expect_identical(game$n_turns, 0L)
  expect_length(game$turns, 0L)
  expect_equal(nrow(game$history), 0L)
})

test_that("random single-turn and game calls support seeds and print methods", {
  board <- make_engine_board(
    points = make_engine_points(`8` = 1L, `17` = -1L),
    turn = 1L
  )

  turn1 <- bg_play_turn(board, seed = 123)
  turn2 <- bg_play_turn(board, seed = 123)
  expect_equal(turn1$roll, turn2$roll)

  game1 <- bg_play_game(board, max_turns = 3L, seed = 123)
  game2 <- bg_play_game(board, max_turns = 3L, seed = 123)
  expect_equal(game1$history[, c("player", "die1", "die2")], game2$history[, c("player", "die1", "die2")])

  expect_output(print(turn1), "<bg_turn_result>")
  expect_output(print(game1), "<bg_game_result>")
})

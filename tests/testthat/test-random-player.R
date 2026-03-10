make_random_points <- function(...) {
  points <- integer(24)
  dots <- list(...)

  for (nm in names(dots)) {
    points[as.integer(nm)] <- as.integer(dots[[nm]])
  }

  points
}

make_random_board <- function(points = integer(24), bar = c(0L, 0L), turn = 1L) {
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

compact_random_sequence <- function(sequence) {
  if (is.null(sequence)) {
    return("<none>")
  }

  paste(vapply(sequence$steps, function(step) {
    paste(step$from, step$to, step$die, if (isTRUE(step$hit)) 1L else 0L, sep = ":")
  }, character(1L)), collapse = "|")
}

test_that("bg_random_move chooses one of the supplied legal moves", {
  board <- make_random_board(
    points = make_random_points(`8` = 1L),
    turn = 1L
  )
  legal_moves <- bg_legal_moves(board, bg_roll(3, 2))

  choice <- bg_random_move(legal_moves, seed = 123)
  legal_keys <- vapply(legal_moves, compact_random_sequence, character(1L))

  expect_s3_class(choice, "bg_move_sequence")
  expect_true(compact_random_sequence(choice) %in% legal_keys)
})

test_that("bg_random_move is reproducible when seeded", {
  board <- make_random_board(
    points = make_random_points(`8` = 1L),
    turn = 1L
  )
  legal_moves <- bg_legal_moves(board, bg_roll(3, 2))

  choice1 <- bg_random_move(legal_moves, seed = 999)
  choice2 <- bg_random_move(legal_moves, seed = 999)

  expect_identical(compact_random_sequence(choice1), compact_random_sequence(choice2))
})

test_that("bg_play_turn_random_player chooses a valid legal move and respects the seed", {
  board <- make_random_board(
    points = make_random_points(`8` = 1L),
    turn = 1L
  )

  turn1 <- bg_play_turn_random_player(board, roll = bg_roll(3, 2), seed = 321)
  turn2 <- bg_play_turn_random_player(board, roll = bg_roll(3, 2), seed = 321)
  legal_keys <- vapply(turn1$legal_moves, compact_random_sequence, character(1L))
  chosen_key <- compact_random_sequence(turn1$chosen_move)

  expect_s3_class(turn1, "bg_turn_result")
  expect_false(turn1$turn_passed)
  expect_true(chosen_key %in% legal_keys)
  expect_identical(chosen_key, compact_random_sequence(turn2$chosen_move))
})

test_that("bg_play_game_random_players is reproducible with scripted rolls and a seed", {
  board <- make_random_board(
    points = make_random_points(`8` = 1L, `17` = -1L),
    turn = 1L
  )
  rolls <- list(bg_roll(3, 2), bg_roll(3, 2))

  game1 <- bg_play_game_random_players(
    board = board,
    roll_sequence = rolls,
    max_turns = 2L,
    seed = 777
  )
  game2 <- bg_play_game_random_players(
    board = board,
    roll_sequence = rolls,
    max_turns = 2L,
    seed = 777
  )

  expect_s3_class(game1, "bg_game_result")
  expect_length(game1$turns, 2L)
  expect_equal(
    vapply(game1$turns, function(turn) compact_random_sequence(turn$chosen_move), character(1L)),
    vapply(game2$turns, function(turn) compact_random_sequence(turn$chosen_move), character(1L))
  )

  for (turn in game1$turns) {
    legal_keys <- vapply(turn$legal_moves, compact_random_sequence, character(1L))
    expect_true(compact_random_sequence(turn$chosen_move) %in% legal_keys)
  }
})

test_that("bg_random_move rejects empty legal-move sets", {
  expect_error(
    bg_random_move(list(), seed = 1),
    "Cannot choose a move from an empty legal-move set",
    fixed = TRUE
  )
})

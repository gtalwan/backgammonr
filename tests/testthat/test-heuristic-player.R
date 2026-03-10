make_heuristic_points <- function(...) {
  points <- integer(24)
  dots <- list(...)

  for (nm in names(dots)) {
    points[as.integer(nm)] <- as.integer(dots[[nm]])
  }

  points
}

make_heuristic_board <- function(points = integer(24), bar = c(0L, 0L), turn = 1L) {
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

compact_heuristic_sequence <- function(sequence) {
  if (is.null(sequence)) {
    return("<none>")
  }

  paste(vapply(sequence$steps, function(step) {
    paste(step$from, step$to, step$die, if (isTRUE(step$hit)) 1L else 0L, sep = ":")
  }, character(1L)), collapse = "|")
}

test_that("aggressive board scoring rewards putting the opponent on the bar", {
  no_hit_board <- make_heuristic_board(
    points = make_heuristic_points(`3` = 1L, `5` = -1L),
    turn = -1L
  )
  hit_board <- make_heuristic_board(
    points = make_heuristic_points(`3` = 1L),
    bar = c(0L, 1L),
    turn = -1L
  )

  expect_gt(
    bg_score_board_aggressive(hit_board, player = 1L),
    bg_score_board_aggressive(no_hit_board, player = 1L)
  )
})

test_that("defensive board scoring penalizes exposed risky blots", {
  risky_board <- make_heuristic_board(
    points = make_heuristic_points(`6` = 1L, `5` = 1L, `2` = -1L),
    turn = -1L
  )
  safe_board <- make_heuristic_board(
    points = make_heuristic_points(`5` = 2L, `2` = -1L),
    turn = -1L
  )

  expect_gt(
    bg_score_board_defensive(safe_board, player = 1L),
    bg_score_board_defensive(risky_board, player = 1L)
  )
})

test_that("aggressive move choice prefers hitting when a hit is available", {
  board <- make_heuristic_board(
    points = make_heuristic_points(`8` = 1L, `5` = -1L),
    turn = 1L
  )
  legal_moves <- bg_legal_moves(board, bg_roll(3, 2))

  choice <- bg_aggressive_move(board, legal_moves)
  after <- bg_apply_move_sequence(board, choice)
  legal_scores <- vapply(
    legal_moves,
    function(move) bg_score_board_aggressive(bg_apply_move_sequence(board, move), player = 1L),
    numeric(1L)
  )

  expect_s3_class(choice, "bg_move_sequence")
  expect_true(any(vapply(choice$steps, function(step) isTRUE(step$hit), logical(1L))))
  expect_identical(after$bar[2], 1L)
  expect_equal(
    bg_score_board_aggressive(after, player = 1L),
    max(legal_scores)
  )
})

test_that("defensive move choice prefers the safer resulting structure", {
  board <- make_heuristic_board(
    points = make_heuristic_points(`8` = 1L, `4` = 1L, `2` = 1L),
    turn = 1L
  )
  legal_moves <- bg_legal_moves(board, bg_roll(3, 2))

  choice <- bg_defensive_move(board, legal_moves)
  after <- bg_apply_move_sequence(board, choice)
  legal_scores <- vapply(
    legal_moves,
    function(move) bg_score_board_defensive(bg_apply_move_sequence(board, move), player = 1L),
    numeric(1L)
  )

  expect_s3_class(choice, "bg_move_sequence")
  expect_identical(after$points[2], 2L)
  expect_identical(after$points[5], 1L)
  expect_identical(after$points[4], 0L)
  expect_equal(
    bg_score_board_defensive(after, player = 1L),
    max(legal_scores)
  )
})

test_that("heuristic turn wrappers choose the same move as direct heuristic selection", {
  aggressive_board <- make_heuristic_board(
    points = make_heuristic_points(`8` = 1L, `5` = -1L),
    turn = 1L
  )
  aggressive_turn <- bg_play_turn_aggressive_player(aggressive_board, roll = bg_roll(3, 2))
  aggressive_choice <- bg_aggressive_move(aggressive_board, aggressive_turn$legal_moves)

  defensive_board <- make_heuristic_board(
    points = make_heuristic_points(`8` = 1L, `4` = 1L, `2` = 1L),
    turn = 1L
  )
  defensive_turn <- bg_play_turn_defensive_player(defensive_board, roll = bg_roll(3, 2))
  defensive_choice <- bg_defensive_move(defensive_board, defensive_turn$legal_moves)

  expect_identical(
    compact_heuristic_sequence(aggressive_turn$chosen_move),
    compact_heuristic_sequence(aggressive_choice)
  )
  expect_identical(aggressive_turn$selection, "aggressive")

  expect_identical(
    compact_heuristic_sequence(defensive_turn$chosen_move),
    compact_heuristic_sequence(defensive_choice)
  )
  expect_identical(defensive_turn$selection, "defensive")
})

test_that("mixed heuristic-game wrapper records player selections and history", {
  board <- make_heuristic_board(
    points = make_heuristic_points(`8` = 1L, `17` = -1L),
    turn = 1L
  )
  rolls <- list(bg_roll(3, 2), bg_roll(3, 2))

  game <- bg_play_game_heuristic_players(
    board = board,
    player1 = "aggressive",
    player2 = "defensive",
    roll_sequence = rolls,
    max_turns = 2L,
    seed = 123L
  )

  expect_s3_class(game, "bg_game_result")
  expect_identical(game$player1_selection, "aggressive")
  expect_identical(game$player2_selection, "defensive")
  expect_length(game$turns, 2L)
  expect_identical(game$history$selection, c("aggressive", "defensive"))
})

test_that("same-archetype game wrappers set the expected selection metadata", {
  board <- make_heuristic_board(
    points = make_heuristic_points(`8` = 1L, `17` = -1L),
    turn = 1L
  )
  rolls <- list(bg_roll(3, 2), bg_roll(3, 2))

  aggressive_game <- bg_play_game_aggressive_players(
    board = board,
    roll_sequence = rolls,
    max_turns = 2L,
    seed = 10L
  )
  defensive_game <- bg_play_game_defensive_players(
    board = board,
    roll_sequence = rolls,
    max_turns = 2L,
    seed = 10L
  )

  expect_identical(aggressive_game$player1_selection, "aggressive")
  expect_identical(aggressive_game$player2_selection, "aggressive")
  expect_identical(defensive_game$player1_selection, "defensive")
  expect_identical(defensive_game$player2_selection, "defensive")
})

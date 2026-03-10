make_sparse_points_rollout <- function(...) {
  points <- integer(24)
  dots <- list(...)

  for (nm in names(dots)) {
    points[as.integer(nm)] <- as.integer(dots[[nm]])
  }

  points
}

make_test_board_rollout <- function(points = integer(24), bar = c(0L, 0L), turn = 1L) {
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

compact_move_sequence_rollout <- function(sequence) {
  vapply(sequence$steps, function(step) {
    paste(step$from, step$to, step$die, if (isTRUE(step$hit)) 1L else 0L, sep = ":")
  }, character(1L))
}

test_that("equal-allocation rollout uses a fixed total budget across legal moves", {
  board <- make_test_board_rollout(
    points = make_sparse_points_rollout(`8` = 1L, `17` = -1L),
    turn = 1L
  )
  roll <- bg_roll(3, 2)
  legal_moves <- bg_legal_moves(board, roll)

  scores <- bg_rollout_evaluate_moves(
    board,
    legal_moves,
    rollout_budget = 5L,
    rollout_policy = "random",
    max_rollout_turns = 20L,
    seed = 123
  )

  expect_s3_class(scores, "data.frame")
  expect_lte(nrow(scores), length(legal_moves))
  expect_true(all(c(
    "candidate_index", "n_equivalent_sequences", "allocation_count", "wins", "losses", "unresolved",
    "empirical_value", "estimate", "lower_95", "upper_95", "recommended"
  ) %in% names(scores)))
  expect_equal(sum(scores$n_equivalent_sequences), length(legal_moves))
  expect_equal(sum(scores$allocation_count), 5L)
  expect_true(all(scores$wins + scores$losses + scores$unresolved == scores$allocation_count))
  expect_true(all(scores$estimate >= 0))
  expect_true(all(scores$estimate <= 1))
})

test_that("equal-allocation rollout move choice is reproducible and legal", {
  board <- make_test_board_rollout(
    points = make_sparse_points_rollout(`8` = 1L, `17` = -1L),
    turn = 1L
  )
  roll <- bg_roll(3, 2)
  legal_moves <- bg_legal_moves(board, roll)

  scores1 <- bg_rollout_evaluate_moves(board, legal_moves, rollout_budget = 6L, seed = 123)
  scores2 <- bg_rollout_evaluate_moves(board, legal_moves, rollout_budget = 6L, seed = 123)
  expect_equal(scores1, scores2)

  move1 <- bg_rollout_move(board, legal_moves, rollout_budget = 6L, seed = 123)
  move2 <- bg_rollout_move(board, legal_moves, rollout_budget = 6L, seed = 123)

  expect_s3_class(move1, "bg_move_sequence")
  expect_identical(compact_move_sequence_rollout(move1), compact_move_sequence_rollout(move2))

  compact_legal <- lapply(legal_moves, compact_move_sequence_rollout)
  expect_true(any(vapply(compact_legal, identical, logical(1L), compact_move_sequence_rollout(move1))))
})

test_that("rollout turn wrapper chooses the same move as direct rollout selection", {
  board <- make_test_board_rollout(
    points = make_sparse_points_rollout(`8` = 1L, `17` = -1L),
    turn = 1L
  )
  roll <- bg_roll(3, 2)
  legal_moves <- bg_legal_moves(board, roll)

  chosen <- bg_rollout_move(
    board,
    legal_moves,
    rollout_budget = 6L,
    rollout_policy = "random",
    max_rollout_turns = 20L,
    seed = 999
  )

  turn_result <- bg_play_turn_rollout_player(
    board,
    roll = roll,
    rollout_budget = 6L,
    rollout_policy = "random",
    max_rollout_turns = 20L,
    seed = 999
  )

  expect_s3_class(turn_result, "bg_turn_result")
  expect_identical(turn_result$selection, "rollout")
  expect_equal(turn_result$n_legal_moves, length(legal_moves))
  expect_false(is.null(turn_result$chosen_move))
  expect_identical(compact_move_sequence_rollout(turn_result$chosen_move), compact_move_sequence_rollout(chosen))
})

test_that("equal-rollout variants can be used in game and simulation wrappers", {
  board <- make_test_board_rollout(
    points = make_sparse_points_rollout(`8` = 1L, `17` = -1L),
    turn = 1L
  )

  game1 <- bg_play_game_matchup(
    board = board,
    player1 = "equal_rollout",
    player2 = "random",
    max_turns = 20L,
    rollout_budget = 4L,
    rollout_policy = "random",
    max_rollout_turns = 20L,
    seed = 123
  )

  game2 <- bg_play_game_matchup(
    board = board,
    player1 = "equal_rollout",
    player2 = "random",
    max_turns = 20L,
    rollout_budget = 4L,
    rollout_policy = "random",
    max_rollout_turns = 20L,
    seed = 123
  )

  expect_s3_class(game1, "bg_game_result")
  expect_identical(game1$player1_selection, "equal_rollout")
  expect_equal(game1$history, game2$history)

  rollout_game1 <- bg_play_game_rollout_players(
    board = board,
    max_turns = 20L,
    rollout_budget = 4L,
    rollout_policy = "random",
    max_rollout_turns = 20L,
    seed = 123
  )
  rollout_game2 <- bg_play_game_rollout_players(
    board = board,
    max_turns = 20L,
    rollout_budget = 4L,
    rollout_policy = "random",
    max_rollout_turns = 20L,
    seed = 123
  )

  expect_s3_class(rollout_game1, "bg_game_result")
  expect_identical(rollout_game1$player1_selection, "rollout")
  expect_identical(rollout_game1$player2_selection, "rollout")
  expect_equal(rollout_game1$history, rollout_game2$history)

  sim1 <- simulate_matchup(
    player1 = "ucb_rollout",
    player2 = "random",
    n_games = 3L,
    board = board,
    max_turns = 20L,
    rollout_budget = 4L,
    rollout_policy = "random",
    max_rollout_turns = 20L,
    seed = 123
  )

  sim2 <- simulate_matchup(
    player1 = "ucb_rollout",
    player2 = "random",
    n_games = 3L,
    board = board,
    max_turns = 20L,
    rollout_budget = 4L,
    rollout_policy = "random",
    max_rollout_turns = 20L,
    seed = 123
  )

  expect_s3_class(sim1, "bg_matchup_simulation")
  expect_identical(sim1$settings$player1_selection, "ucb_rollout")
  expect_equal(sim1$games, sim2$games)
  expect_equal(sim1$summary, sim2$summary)
})

test_that("non-rollout calls ignore rollout-specific tuning arguments", {
  board <- make_test_board_rollout(
    points = make_sparse_points_rollout(`8` = 1L, `17` = -1L),
    turn = 1L
  )
  roll <- bg_roll(3, 2)

  expect_no_error(
    bg_play_turn(
      board,
      roll = roll,
      selection = "random",
      rollout_budget = 0L,
      rollout_policy = "not_used",
      max_rollout_turns = -1L,
      seed = 123
    )
  )

  expect_no_error(
    simulate_matchup(
      player1 = "random",
      player2 = "random",
      n_games = 2L,
      board = board,
      max_turns = 5L,
      seed = 123,
      include_summary = FALSE,
      rollout_budget = 0L,
      rollout_policy = "not_used",
      max_rollout_turns = -1L
    )
  )
})

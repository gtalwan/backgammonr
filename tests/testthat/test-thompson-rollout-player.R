make_sparse_points_thompson <- function(...) {
  points <- integer(24)
  dots <- list(...)

  for (nm in names(dots)) {
    points[as.integer(nm)] <- as.integer(dots[[nm]])
  }

  points
}

make_test_board_thompson <- function(points = integer(24), bar = c(0L, 0L), turn = 1L) {
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

compact_move_sequence_thompson <- function(sequence) {
  vapply(sequence$steps, function(step) {
    paste(step$from, step$to, step$die, if (isTRUE(step$hit)) 1L else 0L, sep = ":")
  }, character(1L))
}

test_that("Thompson rollout evaluation returns posterior diagnostics under a total budget", {
  board <- make_test_board_thompson(
    points = make_sparse_points_thompson(`8` = 1L, `17` = -1L),
    turn = 1L
  )
  legal_moves <- bg_legal_moves(board, bg_roll(3, 2))

  scores <- bg_thompson_rollout_evaluate_moves(
    board,
    legal_moves,
    rollout_budget = 8L,
    rollout_policy = "random",
    max_rollout_turns = 20L,
    seed = 123
  )

  expect_s3_class(scores, "data.frame")
  expect_lte(nrow(scores), length(legal_moves))
  expect_true(all(c(
    "candidate_index", "n_equivalent_sequences", "allocation_count", "wins", "losses", "unresolved",
    "alpha", "beta", "estimate", "posterior_sd", "selection_score"
  ) %in% names(scores)))
  expect_equal(sum(scores$n_equivalent_sequences), length(legal_moves))
  expect_equal(sum(scores$allocation_count), 8L)
  expect_true(all(scores$wins + scores$losses + scores$unresolved == scores$allocation_count))
  expect_true(all(scores$alpha > 0))
  expect_true(all(scores$beta > 0))
  expect_true(all(scores$estimate >= 0))
  expect_true(all(scores$estimate <= 1))
})

test_that("Thompson rollout move choice is reproducible and returns a legal move", {
  board <- make_test_board_thompson(
    points = make_sparse_points_thompson(`8` = 1L, `17` = -1L),
    turn = 1L
  )
  legal_moves <- bg_legal_moves(board, bg_roll(3, 2))

  scores1 <- bg_thompson_rollout_evaluate_moves(board, legal_moves, rollout_budget = 10L, seed = 321)
  scores2 <- bg_thompson_rollout_evaluate_moves(board, legal_moves, rollout_budget = 10L, seed = 321)
  expect_equal(scores1, scores2)

  move1 <- bg_thompson_rollout_move(board, legal_moves, rollout_budget = 10L, seed = 321)
  move2 <- bg_thompson_rollout_move(board, legal_moves, rollout_budget = 10L, seed = 321)

  expect_s3_class(move1, "bg_move_sequence")
  expect_identical(compact_move_sequence_thompson(move1), compact_move_sequence_thompson(move2))

  compact_legal <- lapply(legal_moves, compact_move_sequence_thompson)
  expect_true(any(vapply(compact_legal, identical, logical(1L), compact_move_sequence_thompson(move1))))
})

test_that("Thompson rollout turn wrapper matches direct Thompson move selection", {
  board <- make_test_board_thompson(
    points = make_sparse_points_thompson(`8` = 1L, `17` = -1L),
    turn = 1L
  )
  roll <- bg_roll(3, 2)
  legal_moves <- bg_legal_moves(board, roll)

  chosen <- bg_thompson_rollout_move(
    board,
    legal_moves,
    rollout_budget = 10L,
    rollout_policy = "random",
    max_rollout_turns = 20L,
    seed = 999
  )

  turn_result <- bg_play_turn_thompson_rollout_player(
    board,
    roll = roll,
    rollout_budget = 10L,
    rollout_policy = "random",
    max_rollout_turns = 20L,
    seed = 999
  )

  turn_result2 <- bg_play_turn(
    board,
    roll = roll,
    selection = "thompson_rollout",
    rollout_budget = 10L,
    rollout_policy = "random",
    max_rollout_turns = 20L,
    seed = 999
  )

  expect_s3_class(turn_result, "bg_turn_result")
  expect_identical(turn_result$selection, "thompson_rollout")
  expect_equal(turn_result$n_legal_moves, length(legal_moves))
  expect_false(is.null(turn_result$chosen_move))
  expect_identical(compact_move_sequence_thompson(turn_result$chosen_move), compact_move_sequence_thompson(chosen))
  expect_identical(compact_move_sequence_thompson(turn_result$chosen_move), compact_move_sequence_thompson(turn_result2$chosen_move))
})

test_that("Thompson rollout can be used against a random player", {
  board <- make_test_board_thompson(
    points = make_sparse_points_thompson(`8` = 1L, `17` = -1L),
    turn = 1L
  )

  game1 <- bg_play_game_matchup(
    board = board,
    player1 = "thompson_rollout",
    player2 = "random",
    max_turns = 20L,
    rollout_budget = 6L,
    rollout_policy = "random",
    max_rollout_turns = 20L,
    seed = 123
  )

  game2 <- bg_play_game_matchup(
    board = board,
    player1 = "thompson_rollout",
    player2 = "random",
    max_turns = 20L,
    rollout_budget = 6L,
    rollout_policy = "random",
    max_rollout_turns = 20L,
    seed = 123
  )

  expect_s3_class(game1, "bg_game_result")
  expect_identical(game1$player1_selection, "thompson_rollout")
  expect_identical(game1$player2_selection, "random")
  expect_equal(game1$history, game2$history)
  expect_equal(game1$final_board, game2$final_board)
  expect_identical(game1$winner, game2$winner)
})

test_that("Thompson rollout self-play wrapper returns a valid game result", {
  board <- make_test_board_thompson(
    points = make_sparse_points_thompson(`8` = 1L, `17` = -1L),
    turn = 1L
  )

  game <- bg_play_game_thompson_rollout_players(
    board = board,
    max_turns = 10L,
    rollout_budget = 4L,
    rollout_policy = "random",
    max_rollout_turns = 20L,
    seed = 123
  )

  expect_s3_class(game, "bg_game_result")
  expect_identical(game$selection, "thompson_rollout")
  expect_identical(game$player1_selection, "thompson_rollout")
  expect_identical(game$player2_selection, "thompson_rollout")
})

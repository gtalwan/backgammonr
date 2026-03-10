make_sim_sparse_points <- function(...) {
  points <- integer(24)
  dots <- list(...)

  for (nm in names(dots)) {
    points[as.integer(nm)] <- as.integer(dots[[nm]])
  }

  points
}

make_sim_board <- function(points = integer(24), bar = c(0L, 0L), turn = 1L) {
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

test_that("simulate_matchup returns compact per-game results and summary statistics", {
  board <- make_sim_board(
    points = make_sim_sparse_points(`1` = 1L, `24` = -1L),
    turn = 1L
  )

  sim <- simulate_matchup(
    player1 = "aggressive",
    player2 = "defensive",
    n_games = 3L,
    board = board,
    roll_sequence = list(bg_roll(1, 1)),
    max_turns = 5L,
    include_summary = TRUE
  )

  expect_true(is_bg_matchup_simulation(sim))
  expect_s3_class(sim$games, "data.frame")
  expect_s3_class(sim$summary, "data.frame")
  expect_identical(sim$settings$player1_selection, "aggressive")
  expect_identical(sim$settings$player2_selection, "defensive")
  expect_identical(sim$settings$n_games, 3L)
  expect_true(isTRUE(sim$settings$used_scripted_rolls))

  expect_identical(
    names(sim$games),
    c("game_id", "winner", "winner_label", "n_turns", "game_over", "turn_limit_reached", "roll_sequence_exhausted")
  )
  expect_equal(nrow(sim$games), 3L)
  expect_true(all(sim$games$winner == 1L))
  expect_true(all(sim$games$winner_label == "player_1"))
  expect_true(all(sim$games$n_turns == 1L))
  expect_true(all(sim$games$game_over))
  expect_true(!any(sim$games$turn_limit_reached))
  expect_true(!any(sim$games$roll_sequence_exhausted))

  expect_equal(sim$summary$n_games, 3L)
  expect_equal(sim$summary$completed_games, 3L)
  expect_equal(sim$summary$unresolved_games, 0L)
  expect_equal(sim$summary$player1_wins, 3L)
  expect_equal(sim$summary$player2_wins, 0L)
  expect_equal(sim$summary$player1_win_rate, 1)
  expect_equal(sim$summary$player2_win_rate, 0)
  expect_equal(sim$summary$mean_turns, 1)
  expect_equal(sim$summary$min_turns, 1L)
  expect_equal(sim$summary$max_turns, 1L)

  expect_output(print(sim), "<bg_matchup_simulation>")
  expect_equal(summary(sim), sim$summary)
})

test_that("simulate_matchup can omit summary output", {
  board <- make_sim_board(
    points = make_sim_sparse_points(`1` = 1L, `24` = -1L),
    turn = 1L
  )

  sim <- simulate_matchup(
    player1 = "aggressive",
    player2 = "defensive",
    n_games = 2L,
    board = board,
    roll_sequence = list(bg_roll(1, 1)),
    include_summary = FALSE
  )

  expect_null(sim$summary)
  expect_equal(nrow(sim$games), 2L)
})

test_that("simulate_matchup is reproducible with a fixed seed", {
  board <- make_sim_board(
    points = make_sim_sparse_points(`8` = 1L, `17` = -1L),
    turn = 1L
  )

  sim1 <- simulate_matchup(
    player1 = "random",
    player2 = "random",
    n_games = 5L,
    board = board,
    max_turns = 50L,
    seed = 123
  )

  sim2 <- simulate_matchup(
    player1 = "random",
    player2 = "random",
    n_games = 5L,
    board = board,
    max_turns = 50L,
    seed = 123
  )

  expect_equal(sim1$games, sim2$games)
  expect_equal(sim1$summary, sim2$summary)
})

test_that("simulate_matchup validates its inputs", {
  board <- make_sim_board(
    points = make_sim_sparse_points(`8` = 1L, `17` = -1L),
    turn = 1L
  )

  expect_error(
    simulate_matchup(player1 = "random", player2 = "defensive", n_games = 0L, board = board),
    "`n_games` must be at least 1",
    fixed = TRUE
  )

  expect_error(
    simulate_matchup(player1 = "random", player2 = "defensive", board = unclass(board)),
    "`board` must inherit from class 'bg_board'",
    fixed = TRUE
  )

  expect_error(
    simulate_matchup(player1 = "random", player2 = "defensive", board = board, include_summary = NA),
    "`include_summary` must be TRUE or FALSE",
    fixed = TRUE
  )
})


test_that("simulate_matchup records unresolved games when max_turns is zero", {
  board <- make_sim_board(
    points = make_sim_sparse_points(`8` = 1L, `17` = -1L),
    turn = 1L
  )

  sim <- simulate_matchup(
    player1 = "aggressive",
    player2 = "defensive",
    n_games = 2L,
    board = board,
    max_turns = 0L,
    include_summary = TRUE
  )

  expect_true(all(sim$games$winner == 0L))
  expect_true(all(sim$games$n_turns == 0L))
  expect_true(!any(sim$games$game_over))
  expect_true(all(sim$games$turn_limit_reached))
  expect_equal(sim$summary$completed_games, 0L)
  expect_equal(sim$summary$unresolved_games, 2L)
})


test_that("simulate_matchup records scripted-roll exhaustion", {
  board <- make_sim_board(
    points = make_sim_sparse_points(`8` = 1L, `17` = -1L),
    turn = 1L
  )

  sim <- simulate_matchup(
    player1 = "aggressive",
    player2 = "defensive",
    n_games = 2L,
    board = board,
    roll_sequence = list(bg_roll(1, 1)),
    max_turns = 5L,
    include_summary = TRUE
  )

  expect_true(all(sim$games$roll_sequence_exhausted))
  expect_true(all(!sim$games$game_over))
  expect_equal(sim$summary$roll_sequence_exhausted_games, 2L)
  expect_equal(sim$summary$completed_games, 0L)
})

make_sparse_points_benchmark <- function(...) {
  points <- integer(24)
  dots <- list(...)

  for (nm in names(dots)) {
    points[as.integer(nm)] <- as.integer(dots[[nm]])
  }

  points
}

make_test_board_benchmark <- function(points = integer(24), bar = c(0L, 0L), turn = 1L) {
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

strip_matchup_runtime <- function(summary_df) {
  summary_df[, setdiff(names(summary_df), c("runtime_seconds", "seconds_per_game", "games_per_second")), drop = FALSE]
}

strip_move_runtime <- function(summary_df) {
  summary_df[, setdiff(names(summary_df), c("total_runtime_seconds", "mean_runtime_seconds")), drop = FALSE]
}

test_that("benchmark_matchup returns benchmark-friendly structure", {
  board <- make_test_board_benchmark(
    points = make_sparse_points_benchmark(`8` = 1L, `17` = -1L),
    turn = 1L
  )

  bm <- benchmark_matchup(
    player1 = "random",
    player2 = "aggressive",
    n_games = 2L,
    board = board,
    roll_sequence = list(bg_roll(3, 2), bg_roll(2, 1), bg_roll(1, 1)),
    max_turns = 3L,
    seed = 123
  )

  expect_s3_class(bm, "bg_matchup_benchmark")
  expect_true(is_bg_matchup_benchmark(bm))
  expect_true(all(c("winner", "n_turns") %in% names(bm$games)))
  expect_true(all(c("runtime_seconds", "seconds_per_game", "games_per_second") %in% names(bm$summary)))
  expect_equal(nrow(bm$games), 2L)
  expect_equal(nrow(bm$summary), 1L)
  expect_true(is.numeric(bm$summary$runtime_seconds))
  expect_true(bm$summary$runtime_seconds >= 0)
})

test_that("benchmark_matchup is reproducible up to runtime columns", {
  board <- make_test_board_benchmark(
    points = make_sparse_points_benchmark(`8` = 1L, `17` = -1L),
    turn = 1L
  )

  bm1 <- benchmark_matchup(
    player1 = "random",
    player2 = "random",
    n_games = 3L,
    board = board,
    roll_sequence = list(bg_roll(3, 2), bg_roll(2, 1), bg_roll(1, 1), bg_roll(2, 2)),
    max_turns = 4L,
    seed = 42
  )
  bm2 <- benchmark_matchup(
    player1 = "random",
    player2 = "random",
    n_games = 3L,
    board = board,
    roll_sequence = list(bg_roll(3, 2), bg_roll(2, 1), bg_roll(1, 1), bg_roll(2, 2)),
    max_turns = 4L,
    seed = 42
  )

  expect_equal(bm1$games, bm2$games)
  expect_equal(strip_matchup_runtime(bm1$summary), strip_matchup_runtime(bm2$summary))
})

test_that("benchmark_move_evaluators returns result and summary data frames", {
  case1 <- bg_benchmark_case(
    board = make_test_board_benchmark(
      points = make_sparse_points_benchmark(`8` = 1L),
      turn = 1L
    ),
    roll = bg_roll(3, 2),
    case_id = "case_a"
  )
  case2 <- bg_benchmark_case(
    board = make_test_board_benchmark(
      points = make_sparse_points_benchmark(`17` = -1L),
      turn = -1L
    ),
    roll = bg_roll(3, 2),
    case_id = "case_b"
  )
  expect_true(is_bg_benchmark_case(case1))
  expect_true(is_bg_benchmark_case(case2))

  bm <- benchmark_move_evaluators(
    cases = list(case1, case2),
    methods = c("aggressive", "rollout"),
    reference_method = "aggressive",
    rollout_budget = 3L,
    max_rollout_turns = 10L,
    seed = 123
  )

  expect_s3_class(bm, "bg_move_evaluation_benchmark")
  expect_true(is_bg_move_evaluation_benchmark(bm))
  expect_true(all(c(
    "case_id", "method", "n_legal_moves", "chosen_index",
    "reference_choice_index", "match_reference", "runtime_seconds"
  ) %in% names(bm$results)))
  expect_true(all(c(
    "method", "n_cases", "decision_cases", "mean_n_legal_moves",
    "total_runtime_seconds", "mean_runtime_seconds", "best_move_match_rate"
  ) %in% names(bm$summary)))
  expect_equal(nrow(bm$results), 4L)
  expect_equal(sort(unique(bm$results$case_id)), c("case_a", "case_b"))
  expect_equal(
    bm$summary$best_move_match_rate[bm$summary$method == "aggressive"],
    1
  )
})

test_that("benchmark_move_evaluators is reproducible up to runtime columns", {
  case1 <- bg_benchmark_case(
    board = make_test_board_benchmark(
      points = make_sparse_points_benchmark(`8` = 1L),
      turn = 1L
    ),
    roll = bg_roll(3, 2)
  )
  case2 <- bg_benchmark_case(
    board = make_test_board_benchmark(
      points = make_sparse_points_benchmark(`17` = -1L),
      turn = -1L
    ),
    roll = bg_roll(3, 2)
  )

  bm1 <- benchmark_move_evaluators(
    cases = list(case1, case2),
    methods = c("random", "rollout", "thompson_rollout"),
    reference_method = "rollout",
    rollout_budget = 3L,
    reference_rollout_budget = 4L,
    max_rollout_turns = 10L,
    reference_max_rollout_turns = 10L,
    seed = 99
  )
  bm2 <- benchmark_move_evaluators(
    cases = list(case1, case2),
    methods = c("random", "rollout", "thompson_rollout"),
    reference_method = "rollout",
    rollout_budget = 3L,
    reference_rollout_budget = 4L,
    max_rollout_turns = 10L,
    reference_max_rollout_turns = 10L,
    seed = 99
  )

  expect_equal(
    bm1$results[, setdiff(names(bm1$results), "runtime_seconds"), drop = FALSE],
    bm2$results[, setdiff(names(bm2$results), "runtime_seconds"), drop = FALSE]
  )
  expect_equal(strip_move_runtime(bm1$summary), strip_move_runtime(bm2$summary))
  expect_true(all(bm1$results$chosen_index >= 0L))
})

test_that("benchmark print methods are available", {
  board <- make_test_board_benchmark(
    points = make_sparse_points_benchmark(`8` = 1L, `17` = -1L),
    turn = 1L
  )
  matchup_bm <- benchmark_matchup(
    player1 = "random",
    player2 = "defensive",
    n_games = 1L,
    board = board,
    roll_sequence = list(bg_roll(3, 2)),
    max_turns = 1L,
    seed = 123
  )
  move_bm <- benchmark_move_evaluators(
    cases = bg_benchmark_case(
      board = make_test_board_benchmark(
        points = make_sparse_points_benchmark(`8` = 1L),
        turn = 1L
      ),
      roll = bg_roll(3, 2)
    ),
    methods = c("aggressive", "random"),
    reference_method = "aggressive",
    seed = 123
  )

  expect_identical(unique(move_bm$results$case_id), "case_1")
  expect_output(print(matchup_bm), "<bg_matchup_benchmark>")
  expect_output(print(move_bm), "<bg_move_evaluation_benchmark>")
})

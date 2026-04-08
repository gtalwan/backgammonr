test_that("model spec resolves coherent reward/posterior stacks", {
  expect_error(
    backgammonr:::bg_resolve_model_spec(
      reward_model = "win_indicator",
      posterior_model = "beta_bernoulli",
      unresolved_value = 0.5
    ),
    "has been removed"
  )

  categorical <- backgammonr:::bg_resolve_model_spec(
    reward_model = "categorical_outcome",
    posterior_model = "dirichlet_multinomial",
    unresolved_value = 0.5
  )
  expect_identical(categorical$reward_model_canonical, "categorical_outcome")
  expect_identical(categorical$posterior_model_canonical, "dirichlet_multinomial")
  expect_true(categorical$exact)
  expect_length(categorical$posterior_prior$alpha, 7L)
  expect_length(categorical$posterior_prior$payoff, 7L)
})

test_that("explicit posterior stacks run through the public TS wrappers", {
  scalar_problem <- bg_problem(
    state = bg_initial_board(),
    roll = bg_roll(1L, 6L),
    reward_model = "scalar_payoff",
    posterior_model = "student_t_marginal",
    cache = FALSE,
    problem_id = "scalar_student_t"
  )
  categorical_problem <- bg_problem(
    state = bg_initial_board(),
    roll = bg_roll(1L, 6L),
    reward_model = "categorical_outcome",
    posterior_model = "dirichlet_multinomial",
    cache = FALSE,
    problem_id = "cat_dm"
  )
  win_problem <- bg_problem(
    state = bg_initial_board(),
    roll = bg_roll(1L, 6L),
    unresolved_value = 0,
    reward_model = "win_loss",
    posterior_model = "beta_bernoulli",
    cache = FALSE,
    problem_id = "win_beta"
  )

  scalar_fit <- bg_ts_run(scalar_problem, budget = 16L, checkpoints = c(4L, 8L, 16L), seed = 41L)
  categorical_fit <- bg_ttts_run(categorical_problem, budget = 16L, checkpoints = c(4L, 8L, 16L), seed = 43L)
  win_fit <- bg_ts_run(win_problem, budget = 16L, checkpoints = c(4L, 8L, 16L), seed = 47L)

  expect_s3_class(scalar_fit, "bg_ts_run")
  expect_s3_class(categorical_fit, "bg_ts_run")
  expect_s3_class(win_fit, "bg_ts_run")
  expect_equal(sum(scalar_fit$action_table$allocation_count), 16)
  expect_equal(sum(categorical_fit$action_table$allocation_count), 16)
  expect_equal(sum(win_fit$action_table$allocation_count), 16)
  expect_true(any(is.na(categorical_fit$action_table$alpha)))
})

test_that("terminal scored outcomes classify singles, gammons, and backgammons", {
  single_loss_board <- bg_board(
    points = c(rep(0L, 23L), -14L),
    bar = c(0L, 0L),
    off = c(15L, 1L),
    turn = -1L
  )
  gammon_loss_board <- bg_board(
    points = c(rep(0L, 6L), -15L, rep(0L, 17L)),
    bar = c(0L, 0L),
    off = c(15L, 0L),
    turn = -1L
  )
  backgammon_loss_board <- bg_board(
    points = c(-14L, rep(0L, 22L), 0L),
    bar = c(0L, 1L),
    off = c(15L, 0L),
    turn = -1L
  )

  expect_identical(backgammonr:::bg_cpp_terminal_score_class(unclass(single_loss_board), -1L), "single_loss")
  expect_identical(backgammonr:::bg_cpp_terminal_score_class(unclass(gammon_loss_board), -1L), "gammon_loss")
  expect_identical(backgammonr:::bg_cpp_terminal_score_class(unclass(backgammon_loss_board), -1L), "backgammon_loss")
  expect_identical(backgammonr:::bg_cpp_terminal_score_class(unclass(backgammon_loss_board), 1L), "backgammon_win")
})

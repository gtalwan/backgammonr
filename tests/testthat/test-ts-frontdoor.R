test_that("core public workflow builds coherent problem, truth, and TS objects", {
  problem <- bg_problem(
    state = bg_initial_board(),
    roll = bg_roll(1L, 6L),
    simulation_policy = "random",
    reward_model = "scalar_payoff",
    posterior_model = "beta_pseudo",
    cache = FALSE,
    problem_id = "opening_1_6"
  )

  expect_s3_class(problem, "bg_problem")
  expect_false("legal_summary" %in% names(problem))
  expect_true(nrow(problem$candidate_table) >= 1L)
  expect_equal(nrow(tibble::as_tibble(problem)), nrow(problem$candidate_table))

  truth <- bg_truth_state(
    problem = problem,
    budget = 64L,
    n_cores = 1L,
    parallel = FALSE,
    cache = FALSE,
    seed = 11L
  )

  fit1 <- bg_ts_run(
    problem,
    budget = 32L,
    checkpoints = c(8L, 16L, 32L),
    proxy_reference = truth$reference,
    seed = 11L
  )
  fit2 <- bg_ts_run(
    problem,
    budget = 32L,
    checkpoints = c(8L, 16L, 32L),
    proxy_reference = truth$reference,
    seed = 11L
  )
  ttts <- bg_ttts_run(problem, budget = 32L, proxy_reference = truth$reference, seed = 11L)
  ucb <- bg_ucb_run(problem, budget = 32L, checkpoints = c(8L, 16L, 32L), proxy_reference = truth$reference, seed = 11L)
  equal <- bg_uniform_run(problem, budget = 32L, checkpoints = c(8L, 16L, 32L), proxy_reference = truth$reference, seed = 11L)

  expect_s3_class(fit1, "bg_ts_run")
  expect_s3_class(ttts, "bg_ts_run")
  expect_s3_class(ucb, "bg_ts_run")
  expect_s3_class(equal, "bg_ts_run")
  expect_identical(fit1$recommended_index, fit2$recommended_index)
  expect_equal(fit1$action_table$allocation_count, fit2$action_table$allocation_count)
  expect_s3_class(plot_bg_ts_trace(fit1, metric = "allocation"), "ggplot")
  expect_s3_class(plot_bg_allocation(fit1), "ggplot")
  expect_s3_class(plot_bg_rank_compare(fit1), "ggplot")
  expect_s3_class(plot_bg_truth(truth), "ggplot")
})

test_that("win_indicator is hard-deprecated and comparator restrictions are honest", {
  expect_error(
    bg_problem(
      state = bg_initial_board(),
      roll = bg_roll(1L, 6L),
      reward_model = "win_indicator",
      posterior_model = "beta_bernoulli",
      cache = FALSE
    ),
    "has been removed"
  )

  categorical_problem <- bg_problem(
    bg_initial_board(),
    bg_roll(1L, 6L),
    reward_model = "categorical_outcome",
    posterior_model = "dirichlet_multinomial",
    cache = FALSE
  )

  expect_error(
    bg_ucb_run(categorical_problem, budget = 16L, seed = 5L),
    "legacy scalar engine"
  )
  expect_error(
    bg_compare_algorithms(
      problems = categorical_problem,
      methods = c("thompson", "ucb"),
      budgets = 16L,
      seeds = 1:2,
      progress = FALSE
    ),
    "Scalar-engine comparators"
  )
})

test_that("proxy-reference extension and focused mode preserve full sufficient statistics", {
  problem <- bg_problem(
    bg_initial_board(),
    bg_roll(1L, 6L),
    reward_model = "categorical_outcome",
    posterior_model = "dirichlet_multinomial",
    cache = FALSE
  )
  scored_cols <- backgammonr:::bg_scored_outcome_columns()
  stat_cols <- backgammonr:::bg_reference_stats_columns()

  ref_small <- bg_reference(problem, budget = 24L, workers_truth = 1L, truth_block_size = 8L, seed = 101L)
  ref_extended <- bg_reference(
    problem,
    budget = 48L,
    workers_truth = 1L,
    truth_block_size = 8L,
    extend_existing_reference = ref_small,
    seed = 101L
  )
  ref_focused <- bg_reference(
    problem,
    budget = 48L,
    workers_truth = 1L,
    truth_block_size = 8L,
    reference_mode = "focused",
    seed = 103L
  )

  expect_true(all(stat_cols %in% names(ref_extended$action_table)))
  expect_true(all(stat_cols %in% names(ref_focused$action_table)))
  expect_equal(sum(ref_extended$action_table$allocation_count), 48)
  expect_equal(sum(ref_focused$action_table$allocation_count), 48)
  expect_true(all(colSums(ref_extended$action_table[, scored_cols, drop = FALSE]) >=
    colSums(ref_small$action_table[, scored_cols, drop = FALSE])))
})

test_that("compare-algorithms save_path reuses existing study objects", {
  problem <- bg_problem(bg_initial_board(), bg_roll(1L, 6L), cache = FALSE)
  truth <- bg_truth_state(problem = problem, budget = 48L, n_cores = 1L, parallel = FALSE, cache = FALSE, seed = 1L)
  path <- tempfile(fileext = ".rds")

  cmp1 <- bg_compare_algorithms(
    problems = problem,
    methods = c("thompson", "equal"),
    budgets = c(8L, 16L),
    seeds = 1:2,
    proxy_references = truth$reference,
    save_path = path,
    overwrite = TRUE,
    progress = FALSE
  )
  cmp2 <- bg_compare_algorithms(
    problems = problem,
    methods = c("thompson", "equal"),
    budgets = c(8L, 16L),
    seeds = 1:2,
    proxy_references = truth$reference,
    save_path = path,
    overwrite = FALSE,
    progress = FALSE
  )

  expect_true(file.exists(path))
  expect_s3_class(cmp1, "bg_method_compare")
  expect_s3_class(cmp2, "bg_method_compare")
  expect_equal(cmp1$summary, cmp2$summary)
})

test_that("trimmed public namespace does not export internal study helpers", {
  exports <- getNamespaceExports("backgammonr")

  expect_false("bg_ts_decide" %in% exports)
  expect_false("bg_ts_profile" %in% exports)
  expect_false("bg_ts_trace" %in% exports)
  expect_false("bg_opening_study" %in% exports)
  expect_false("bg_state_battery" %in% exports)
  expect_false("bg_eval_seed_stability" %in% exports)
  expect_false("bg_profile_runtime" %in% exports)
  expect_false("plot_bg_state_battery" %in% exports)
})

test_that("TS front door handles positions with no legal moves", {
  points <- integer(24)
  points[24] <- -2L
  points[23] <- -2L
  points[1] <- -11L
  board <- bg_board(
    points = points,
    bar = c(1L, 0L),
    off = c(14L, 0L),
    turn = 1L
  )

  problem <- bg_problem(board, bg_roll(1L, 2L), cache = FALSE, problem_id = "no_move")
  fit <- bg_ts_run(problem, budget = 16L, seed = 31L)
  ref <- bg_reference(problem, budget = 32L, seed = 33L)

  expect_length(problem$legal_moves, 0L)
  expect_equal(nrow(problem$candidate_table), 0L)
  expect_s3_class(fit, "bg_ts_run")
  expect_equal(nrow(fit$action_table), 0L)
  expect_true(all(is.na(fit$checkpoint_table$recommended_index)))
  expect_s3_class(ref, "bg_reference")
  expect_equal(nrow(ref$action_table), 0L)
  expect_true(is.na(ref$summary$proxy_reference_best_index[[1L]]))
})

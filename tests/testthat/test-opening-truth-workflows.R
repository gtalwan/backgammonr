test_that("opening truth helpers support one-by-one and batch workflows", {
  cache_dir <- tempfile("bg-opening-cache-")

  rolls <- bg_opening_rolls()
  expect_equal(nrow(rolls), 21L)

  truth_one <- bg_opening_truth_build_one(
    "1-6",
    budget = 32L,
    n_cores = 1L,
    parallel = FALSE,
    cache_dir = cache_dir,
    seed = 21L
  )

  expect_s3_class(truth_one, "bg_truth_state")
  expect_equal(truth_one$problem$problem_id, "opening_1_6")

  loaded_one <- bg_opening_truth_load_one("1-6", cache_dir = cache_dir)
  expect_s3_class(loaded_one, "bg_truth_state")
  expect_identical(truth_one$metadata$problem_hash, loaded_one$metadata$problem_hash)

  index_one <- bg_opening_truth_index(cache_dir = cache_dir)
  row_one <- index_one[index_one$opening_roll == "1-6", , drop = FALSE]
  expect_equal(row_one$status[[1L]], "cached")
  expect_true(file.exists(row_one$path[[1L]]))

  truth_all <- bg_opening_truth_build_all(
    rolls = c("1-6", "2-4"),
    budget = 16L,
    n_cores = 1L,
    parallel = FALSE,
    cache_dir = cache_dir,
    seed = 22L,
    verbose = FALSE
  )

  expect_s3_class(truth_all, "bg_truth_battery")
  expect_equal(nrow(truth_all$summary), 2L)
  expect_true(all(c("opening_roll", "die1", "die2", "is_double", "roll_group") %in% names(truth_all$summary)))
})

test_that("truth certification and diagnostics expose the focused research summaries", {
  cache_dir <- tempfile("bg-diagnostics-cache-")
  truth <- bg_opening_truth_build_one(
    "1-6",
    budget = 32L,
    n_cores = 1L,
    parallel = FALSE,
    cache_dir = cache_dir,
    seed = 31L
  )
  fit <- bg_ts_run(
    truth$problem,
    budget = 16L,
    checkpoints = c(8L, 16L),
    proxy_reference = truth$reference,
    seed = 31L
  )

  cert <- bg_truth_certify(truth)
  expect_true(all(c(
    "top_two_gap_estimate",
    "top_two_gap_mc_lower_95",
    "top_two_gap_mc_upper_95",
    "mc_gap_excludes_zero",
    "certification"
  ) %in% names(cert)))

  diag <- bg_ts_diagnostics(fit)
  expect_true(all(c("allocation", "accuracy", "efficiency", "failures", "panel") %in% names(diag)))
  expect_true(all(c("share_top_k_truth", "share_mc_screened_suboptimal") %in% names(diag$allocation)))
  expect_true(all(c("top1_match", "pairwise_ordering_accuracy") %in% names(diag$accuracy)))
})

test_that("truth projection reuses scored outcome counts across reward systems", {
  source_truth <- bg_opening_truth_build_one(
    "1-6",
    budget = 32L,
    reward_model = "categorical_outcome",
    posterior_model = "dirichlet_multinomial",
    n_cores = 1L,
    parallel = FALSE,
    cache = FALSE,
    seed = 51L
  )

  scalar_truth <- bg_truth_project(
    source_truth,
    reward_model = "scalar_payoff",
    posterior_model = "beta_pseudo",
    unresolved_value = 0.5
  )
  win_loss_truth <- bg_truth_project(
    source_truth,
    reward_model = "win_loss",
    posterior_model = "beta_bernoulli",
    unresolved_value = 0
  )

  score_cols <- backgammonr:::bg_scored_outcome_columns()
  source_tab <- source_truth$reference$action_table
  scalar_tab <- scalar_truth$reference$action_table[
    match(source_tab$candidate_index, scalar_truth$reference$action_table$candidate_index),
    ,
    drop = FALSE
  ]
  win_loss_tab <- win_loss_truth$reference$action_table[
    match(source_tab$candidate_index, win_loss_truth$reference$action_table$candidate_index),
    ,
    drop = FALSE
  ]
  source_counts <- source_tab[, score_cols, drop = FALSE]
  scalar_counts <- scalar_tab[, score_cols, drop = FALSE]
  win_loss_counts <- win_loss_tab[, score_cols, drop = FALSE]

  rownames(source_counts) <- NULL
  rownames(scalar_counts) <- NULL
  rownames(win_loss_counts) <- NULL

  expected_scalar_reward_sum <- source_tab$wins + (0.5 * source_tab$unresolved)
  expected_win_loss_reward_sum <- source_tab$wins

  expect_equal(source_counts, scalar_counts)
  expect_equal(source_counts, win_loss_counts)
  expect_equal(scalar_tab$reward_sum, expected_scalar_reward_sum)
  expect_equal(win_loss_tab$reward_sum, expected_win_loss_reward_sum)
  expect_identical(
    scalar_truth$metadata$projection_source_hash,
    source_truth$metadata$truth_hash
  )
  expect_equal(
    win_loss_truth$problem$settings$posterior_model_canonical,
    "beta_bernoulli"
  )
})

test_that("opening truth loads can align one scalar cache with a different posterior family", {
  cache_dir <- tempfile("bg-opening-projection-cache-")

  truth <- bg_opening_truth_build_one(
    "1-6",
    budget = 32L,
    reward_model = "scalar_payoff",
    posterior_model = "beta_pseudo",
    n_cores = 1L,
    parallel = FALSE,
    cache_dir = cache_dir,
    seed = 61L
  )

  loaded_student_t <- bg_opening_truth_load_one(
    "1-6",
    reward_model = "scalar_payoff",
    posterior_model = "student_t_marginal",
    cache_dir = cache_dir
  )

  expect_equal(truth$metadata$truth_hash, loaded_student_t$metadata$truth_hash)
  expect_equal(
    loaded_student_t$problem$settings$posterior_model_canonical,
    "student_t_marginal"
  )
  expect_equal(
    loaded_student_t$metadata$projection_source_posterior_model,
    truth$problem$settings$posterior_model
  )
})

test_that("master truth wrappers give one scored-outcome truth that can be reused", {
  master_truth <- bg_master_truth_state(
    state = bg_initial_board(),
    roll = bg_roll(1L, 6L),
    budget = 32L,
    n_cores = 1L,
    parallel = FALSE,
    cache = FALSE,
    seed = 71L
  )

  opening_master_truth <- bg_opening_master_truth_build_one(
    "1-6",
    budget = 32L,
    n_cores = 1L,
    parallel = FALSE,
    cache = FALSE,
    seed = 72L
  )

  projected_scalar <- bg_truth_project(
    master_truth,
    reward_model = "scalar_payoff",
    posterior_model = "student_t_marginal",
    unresolved_value = 0.5
  )

  expect_s3_class(master_truth, "bg_truth_state")
  expect_s3_class(opening_master_truth, "bg_truth_state")
  expect_equal(master_truth$problem$settings$reward_model_canonical, "categorical_outcome")
  expect_equal(master_truth$problem$settings$posterior_model_canonical, "dirichlet_multinomial")
  expect_equal(opening_master_truth$problem$settings$reward_model_canonical, "categorical_outcome")
  expect_equal(opening_master_truth$problem$settings$posterior_model_canonical, "dirichlet_multinomial")
  expect_equal(projected_scalar$problem$settings$reward_model_canonical, "scalar_payoff")
  expect_equal(projected_scalar$problem$settings$posterior_model_canonical, "student_t_marginal")
})

test_that("TS and TTTS routing stays explicit across scalar and posterior engines", {
  scalar_problem <- bg_problem(
    bg_initial_board(),
    bg_roll(1L, 6L),
    reward_model = "scalar_payoff",
    posterior_model = "beta_pseudo",
    cache = FALSE
  )
  explicit_problem <- bg_problem(
    bg_initial_board(),
    bg_roll(1L, 6L),
    reward_model = "categorical_outcome",
    posterior_model = "dirichlet_multinomial",
    cache = FALSE
  )

  scalar_ts <- bg_ts_run(scalar_problem, budget = 16L, seed = 41L)
  scalar_ttts <- bg_ttts_run(scalar_problem, budget = 16L, seed = 41L)
  explicit_ts <- bg_ts_run(explicit_problem, budget = 16L, seed = 41L)
  explicit_ttts <- bg_ttts_run(explicit_problem, budget = 16L, seed = 41L)

  expect_equal(scalar_ts$settings$engine_path, "legacy_scalar_engine")
  expect_equal(scalar_ttts$settings$engine_path, "legacy_scalar_engine")
  expect_equal(explicit_ts$settings$engine_path, "explicit_posterior_engine")
  expect_equal(explicit_ttts$settings$engine_path, "explicit_posterior_engine")
})

test_that("supported TS variants stay live while weak approximations stay gated out", {
  problem <- bg_problem(
    bg_initial_board(),
    bg_roll(1L, 6L),
    cache = FALSE
  )

  expect_s3_class(bg_multi_sample_ts_run(problem, budget = 16L, seed = 1L), "bg_ts_run")
  expect_s3_class(bg_soft_elimination_ts_run(problem, budget = 16L, seed = 1L), "bg_ts_run")
  expect_s3_class(bg_forced_exploration_ts_run(problem, budget = 16L, seed = 1L), "bg_ts_run")
  expect_s3_class(bg_top_k_ts_run(problem, budget = 16L, seed = 1L), "bg_ts_run")

  expect_error(
    backgammonr:::bg_ts_variant_run(problem, budget = 16L, variant = "tempered_thompson"),
    "one of"
  )
  expect_error(
    backgammonr:::bg_ts_variant_run(problem, budget = 16L, variant = "budget_aware_thompson"),
    "one of"
  )
})

test_that("truth workflows save, load, and summarize coherently", {
  problem <- bg_problem(
    state = bg_initial_board(),
    roll = bg_roll(1L, 6L),
    cache = FALSE,
    problem_id = "truth_case"
  )

  path <- tempfile(fileext = ".rds")
  truth <- bg_truth_state(
    problem = problem,
    budget = 64L,
    n_cores = 1L,
    parallel = FALSE,
    save_path = path,
    overwrite = TRUE,
    seed = 9L
  )

  expect_s3_class(truth, "bg_truth_state")
  expect_true(file.exists(path))
  expect_equal(truth$reference$summary$reference_budget[[1L]], 64L)

  truth2 <- bg_truth_load(path)
  expect_s3_class(truth2, "bg_truth_state")
  expect_identical(truth$metadata$problem_hash, truth2$metadata$problem_hash)

  diag <- bg_truth_diagnostics(truth)
  expect_true(all(c("summary", "move_table") %in% names(diag)))
  expect_true("top_two_gap_estimate" %in% names(diag$summary))

  opening <- bg_truth_opening(
    rolls = list(bg_roll(1L, 6L), bg_roll(2L, 4L)),
    budget = 32L,
    n_cores = 1L,
    parallel = FALSE,
    cache = FALSE,
    verbose = FALSE,
    seed = 5L
  )

  expect_s3_class(opening, "bg_truth_battery")
  expect_equal(nrow(opening$summary), 2L)
  expect_true(all(c("opening_roll", "die1", "die2", "is_double", "roll_group") %in% names(opening$summary)))
})

test_that("evaluation helpers return coherent public summaries", {
  problem <- bg_problem(bg_initial_board(), bg_roll(1L, 6L), cache = FALSE)
  truth <- bg_truth_state(problem = problem, budget = 64L, n_cores = 1L, parallel = FALSE, cache = FALSE, seed = 1L)
  fit <- bg_ts_run(problem, budget = 32L, checkpoints = c(8L, 16L, 32L), proxy_reference = truth$reference, seed = 2L)

  top1 <- bg_eval_top1(fit)
  rank <- bg_eval_rank(fit)
  alloc <- bg_eval_allocation(fit)
  efficiency <- bg_eval_efficiency(fit)
  calibration <- bg_eval_calibration(fit)
  ref_aware <- bg_eval_reference_aware(fit)

  expect_true(all(c("top1_match", "simple_regret", "selected_reference_rank") %in% names(top1)))
  expect_true(all(c("spearman", "kendall", "top_k_overlap") %in% names(rank)))
  expect_true(all(c("share_top_k_truth", "share_mc_screened_suboptimal") %in% names(alloc)))
  expect_true(all(c("first_budget_top1_match", "auc_simple_regret", "mean_brier_top1") %in% names(efficiency)))
  expect_true(all(c("raw", "summary") %in% names(calibration)))
  expect_true(all(c("brier_top1", "calibration_bin") %in% names(calibration$raw)))
  expect_true(all(c("mean_predicted_prob_best", "observed_top1_rate", "ece_component") %in% names(calibration$summary)))
  expect_true(all(c("top1_match", "spearman", "share_top_k_truth", "near_tie") %in% names(ref_aware)))
})

test_that("efficiency and calibration helpers summarize checkpoint paths cleanly", {
  problem <- bg_problem(bg_initial_board(), bg_roll(1L, 6L), cache = FALSE)
  truth <- bg_truth_state(problem = problem, budget = 64L, n_cores = 1L, parallel = FALSE, cache = FALSE, seed = 3L)
  cmp <- bg_compare_algorithms(
    problems = problem,
    methods = c("thompson", "equal"),
    budgets = c(8L, 16L),
    seeds = 1:2,
    proxy_references = truth$reference
  )

  efficiency <- bg_eval_efficiency(cmp)
  calibration <- bg_eval_calibration(cmp, bins = 4L)

  expect_true(nrow(efficiency) == 4L)
  expect_true(all(efficiency$max_checkpoint == 16L))
  expect_true(all(is.na(efficiency$first_budget_top1_match) | efficiency$first_budget_top1_match %in% c(8, 16)))
  expect_true(all(calibration$summary$calibration_bin %in% 1:4))
})

test_that("reference-aware evaluation keeps ts_mode in merge keys", {
  problem <- bg_problem(bg_initial_board(), bg_roll(1L, 6L), cache = FALSE)
  truth <- bg_truth_state(problem = problem, budget = 64L, n_cores = 1L, parallel = FALSE, cache = FALSE, seed = 7L)
  sequential <- bg_ts_run(problem, budget = 16L, checkpoints = c(8L, 16L), proxy_reference = truth$reference, seed = 9L)
  batched <- backgammonr:::bg_ts_decide(
    problem,
    budget = 16L,
    checkpoints = c(8L, 16L),
    proxy_reference = truth$reference,
    ts_mode = "batched",
    batch_size = 4L,
    seed = 9L
  )

  cmp <- structure(
    list(
      problems = list(problem),
      references = list(truth$reference),
      results = rbind(sequential$checkpoint_table, batched$checkpoint_table),
      summary = data.frame(),
      runs = list(sequential = sequential, batched = batched),
      settings = list()
    ),
    class = "bg_method_compare"
  )

  panel <- bg_eval_reference_aware(cmp)
  dup_counts <- table(panel$checkpoint)
  expect_true(all(dup_counts == 2L))
  expect_true(all(c("sequential", "batched") %in% panel$ts_mode))
})

test_that("posterior and reward comparison workflows save and reload cleanly", {
  base_problem <- bg_problem(
    state = bg_initial_board(),
    roll = bg_roll(1L, 6L),
    cache = FALSE,
    problem_id = "posterior_cmp"
  )

  posterior_path <- tempfile(fileext = ".rds")
  reward_path <- tempfile(fileext = ".rds")

  posterior_cmp <- bg_compare_posteriors(
    problems = base_problem,
    reward_model = "scalar_payoff",
    posterior_models = c("beta_pseudo", "student_t_marginal", "bootstrap"),
    budgets = c(8L, 16L),
    seeds = 1:2,
    reference_budget = 32L,
    n_cores = 1L,
    parallel = FALSE,
    progress = FALSE,
    save_path = posterior_path,
    overwrite = TRUE
  )
  posterior_cmp_loaded <- bg_compare_posteriors(
    problems = base_problem,
    reward_model = "scalar_payoff",
    posterior_models = c("beta_pseudo", "student_t_marginal", "bootstrap"),
    budgets = c(8L, 16L),
    seeds = 1:2,
    reference_budget = 32L,
    n_cores = 1L,
    parallel = FALSE,
    progress = FALSE,
    save_path = posterior_path,
    overwrite = FALSE
  )

  reward_cmp <- bg_compare_reward_models(
    problems = base_problem,
    reward_model_map = c(
      scalar_payoff = "beta_pseudo",
      categorical_outcome = "dirichlet_multinomial"
    ),
    budgets = c(8L, 16L),
    seeds = 1:2,
    reference_budget = 32L,
    n_cores = 1L,
    parallel = FALSE,
    progress = FALSE,
    save_path = reward_path,
    overwrite = TRUE
  )

  expect_true(inherits(posterior_cmp, "bg_posterior_compare"))
  expect_true(inherits(reward_cmp, "bg_reward_model_compare"))
  expect_equal(posterior_cmp$summary, posterior_cmp_loaded$summary)
  expect_true(all(c("reward_model", "posterior_model") %in% names(posterior_cmp$results)))
  expect_true(all(c("reward_model", "posterior_model") %in% names(reward_cmp$results)))
  expect_s3_class(plot_bg_posterior_compare(posterior_cmp, metric = "spearman"), "ggplot")
})

test_that("state classifiers stay public and direct", {
  problem <- bg_problem(bg_initial_board(), bg_roll(1L, 6L), cache = FALSE)
  cls <- bg_state_classify(problem)
  diff <- bg_state_difficulty(problem)

  expect_true(all(c("state_class", "contact", "player_prime_length") %in% names(cls)))
  expect_true(all(c("difficulty_score", "top_two_gap_estimate") %in% names(diff)))
  expect_true(all(c("candidate_index", "move_label", "n_steps") %in% names(bg_move_features(problem))))
})

test_that("truth stability separates reference stability from decision screening", {
  truth <- bg_opening_truth_build_one(
    "1-6",
    budget = 32L,
    n_cores = 1L,
    parallel = FALSE,
    cache = FALSE,
    seed = 1L
  )

  stability <- bg_truth_stability(
    truth,
    budgets = c(16L, 32L),
    seeds = 1:2,
    n_cores = 1L,
    parallel = FALSE,
    progress = FALSE,
    seed = 1L
  )

  expect_s3_class(stability, "bg_truth_stability")
  expect_true(all(c(
    "reference_stability_label",
    "decision_screen_label",
    "modal_top1_share",
    "gap_clear_rate"
  ) %in% names(stability$summary)))
  expect_true(all(c("first_reference_stable_budget", "first_decision_clear_budget") %in% names(stability$problem_summary)))
})

test_that("top-k, restricted rank, stopping, and posterior adequacy workflows expose new research metrics", {
  problem <- bg_problem(bg_initial_board(), bg_roll(1L, 6L), cache = FALSE)
  truth <- bg_truth_state(problem = problem, budget = 48L, n_cores = 1L, parallel = FALSE, cache = FALSE, seed = 2L)
  fit <- bg_ts_run(problem, budget = 16L, checkpoints = c(8L, 16L), proxy_reference = truth$reference, seed = 2L)

  topk <- bg_eval_topk(fit, k = 3L)
  restricted <- bg_eval_restricted_rank(fit, top_m = 3L)
  stopping <- bg_stopping_diagnostics(fit, truth = truth)
  adequacy <- bg_posterior_adequacy(fit)

  expect_true(all(c("truth_top2_hit", "truth_top_k_hit") %in% names(topk)))
  expect_true(all(c("restricted_pairwise_ordering_accuracy", "restricted_pairwise_disagreement_count") %in% names(restricted)))
  expect_true(all(c("raw", "checkpoint_summary", "threshold_summary", "settings") %in% names(stopping)))
  expect_true(all(c("prob_good_selection_analogue", "suggest_stop_combined") %in% names(stopping$raw)))
  expect_true(all(c("summary", "action_table") %in% names(adequacy)))
  expect_true(all(c("mean_abs_mean_gap", "gross_mismatch_actions") %in% names(adequacy$summary)))
})

test_that("opening comparison study summarizes within opening before aggregation", {
  cache_dir <- tempfile("bg-opening-study-cache-")
  study <- bg_opening_compare_study(
    rolls = c("1-6", "2-4"),
    methods = c("thompson", "top_two_thompson"),
    budgets = c(8L, 16L),
    seeds = 1:2,
    truth_budget = 32L,
    n_cores = 1L,
    parallel = FALSE,
    progress = FALSE,
    cache_dir = cache_dir,
    bootstrap_reps = 10L,
    seed = 3L
  )

  expect_s3_class(study, "bg_opening_compare_study")
  expect_true(all(c("opening_summary", "opening_aggregate", "contrasts") %in% names(study)))
  expect_true(all(c(
    "mean_top1_match",
    "mean_share_top2_truth",
    "recommendation_instability",
    "high_confidence_wrong_rate"
  ) %in% names(study$opening_summary)))
  expect_true(all(c("metric", "estimate", "lower_95", "upper_95") %in% names(study$opening_aggregate)))
})

test_that("backend parity compares fast and explicit TS paths on the scalar stack", {
  problem <- bg_problem(
    bg_initial_board(),
    bg_roll(1L, 6L),
    reward_model = "scalar_payoff",
    posterior_model = "beta_pseudo",
    cache = FALSE
  )

  parity <- bg_backend_parity(
    problem,
    allocation_policy = "thompson",
    budgets = c(8L, 16L),
    seeds = 1:2,
    progress = FALSE
  )

  expect_true(all(c("raw", "summary", "reference", "settings") %in% names(parity)))
  expect_true(all(c("recommended_match", "allocation_share_l1", "max_prob_best_gap") %in% names(parity$raw)))
})

test_that("bg_sanity_lab returns compact case, result, and summary tables", {
  lab <- bg_sanity_lab(
    allocation_policies = c("thompson", "top_two_thompson", "equal"),
    budget = 16L,
    seeds = 1:2
  )

  expect_true(is.list(lab))
  expect_true(all(c("cases", "results", "summary") %in% names(lab)))
  expect_true(is.data.frame(lab$cases))
  expect_true(is.data.frame(lab$results))
  expect_true(is.data.frame(lab$summary))
  expect_true(all(c(
    "bernoulli_clear",
    "bernoulli_near_tie",
    "bernoulli_many_dominated",
    "bernoulli_many_near_optimal",
    "categorical_variance_asymmetry",
    "categorical_multimodal_bounded"
  ) %in% lab$cases$case_id))
  expect_true(all(c("equal", "thompson", "top_two_thompson") %in% lab$summary$allocation_policy))
  expect_true(all(c("truth_top2_hit", "gap_weighted_wasted_allocation") %in% names(lab$results)))
  expect_true(all(c("recommendation_instability", "restricted_pairwise_ordering_accuracy") %in% names(lab$summary)))
})

test_that("bg_sanity_lab keeps the policy set intentionally narrow", {
  expect_error(
    bg_sanity_lab(allocation_policies = "ranking_aware_thompson", budget = 8L, seeds = 1L),
    "currently supports only"
  )
})

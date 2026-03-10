test_that("bg_roll returns normalized roll objects", {
  roll <- bg_roll(3, 5)

  expect_s3_class(roll, "bg_roll")
  expect_identical(roll$dice, c(3L, 5L))
  expect_identical(roll$is_double, FALSE)
  expect_identical(roll$expanded, c(3L, 5L))
})

test_that("bg_roll handles doubles explicitly", {
  roll <- bg_roll(4, 4)

  expect_true(is_bg_roll(roll))
  expect_identical(roll$dice, c(4L, 4L))
  expect_identical(roll$is_double, TRUE)
  expect_identical(roll$expanded, c(4L, 4L, 4L, 4L))
})

test_that("bg_roll rejects invalid die values", {
  expect_error(bg_roll(0, 2), "`die1` must be between 1 and 6", fixed = TRUE)
  expect_error(bg_roll(2, 7), "`die2` must be between 1 and 6", fixed = TRUE)
  expect_error(bg_roll(1.5, 2), "`die1` must contain whole numbers only", fixed = TRUE)
})

test_that("bg_roll_dice returns deterministic results when seeded", {
  rolls1 <- bg_roll_dice(n = 5, seed = 123)
  rolls2 <- bg_roll_dice(n = 5, seed = 123)

  expect_length(rolls1, 5L)
  expect_true(all(vapply(rolls1, is_bg_roll, logical(1L))))
  expect_equal(rolls1, rolls2)
})

test_that("bg_roll_dice returns a single roll when n equals one", {
  roll <- bg_roll_dice(seed = 42)

  expect_s3_class(roll, "bg_roll")
  expect_length(roll$dice, 2L)
  expect_true(all(roll$dice %in% 1:6))
  expect_true(all(roll$expanded %in% 1:6))
})

test_that("bg_roll_dice validates n and seed", {
  expect_error(bg_roll_dice(n = 0), "`n` must be at least 1", fixed = TRUE)
  expect_error(bg_roll_dice(seed = -1), "`seed` must be nonnegative", fixed = TRUE)
})

test_that("print method for bg_roll is available", {
  roll <- bg_roll(2, 2)
  expect_output(print(roll), "<bg_roll>")
  expect_output(print(roll), "expanded")
})

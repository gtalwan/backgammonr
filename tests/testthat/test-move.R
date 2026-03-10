test_that("bg_move_step returns normalized step objects", {
  step <- bg_move_step(from = 24, to = 21, die = 3)

  expect_s3_class(step, "bg_move_step")
  expect_identical(step$from, 24L)
  expect_identical(step$to, 21L)
  expect_identical(step$die, 3L)
  expect_identical(step$hit, FALSE)
})

test_that("bg_move_step supports bar entry and bearing off sentinels", {
  enter <- bg_move_step(from = 0, to = 3, die = 3)
  bearoff <- bg_move_step(from = 2, to = 25, die = 2)

  expect_identical(enter$from, 0L)
  expect_identical(bearoff$to, 25L)
})

test_that("bg_move_step rejects malformed inputs", {
  expect_error(bg_move_step(from = -1, to = 3, die = 3), "`from` must be between 0 and 24", fixed = TRUE)
  expect_error(bg_move_step(from = 3, to = 26, die = 3), "`to` must be between 1 and 25", fixed = TRUE)
  expect_error(bg_move_step(from = 3, to = 3, die = 3), "`from` and `to` must differ", fixed = TRUE)
  expect_error(bg_move_step(from = 3, to = 1, die = 0), "`die` must be between 1 and 6", fixed = TRUE)
  expect_error(bg_move_step(from = 3, to = 1, die = 2, hit = NA), "`hit` must be TRUE or FALSE", fixed = TRUE)
})

test_that("bg_move_sequence builds ordered full-turn structures", {
  roll <- bg_roll(2, 5)
  seqn <- bg_move_sequence(
    player = 1,
    roll = roll,
    steps = list(
      bg_move_step(from = 24, to = 22, die = 2),
      bg_move_step(from = 22, to = 17, die = 5, hit = TRUE)
    )
  )

  expect_s3_class(seqn, "bg_move_sequence")
  expect_identical(seqn$player, 1L)
  expect_s3_class(seqn$roll, "bg_roll")
  expect_length(seqn$steps, 2L)
  expect_true(all(vapply(seqn$steps, is_bg_move_step, logical(1L))))
  expect_identical(seqn$dice_used, c(2L, 5L))
  expect_identical(seqn$n_steps, 2L)
})

test_that("bg_move_sequence rejects empty step lists", {
  expect_error(
    bg_move_sequence(player = -1, steps = list()),
    "`steps` must contain at least one move step",
    fixed = TRUE
  )
})

test_that("bg_move_sequence validates player and step containers", {
  expect_error(bg_move_sequence(player = 0, steps = list()), "`player` must be either 1L or -1L", fixed = TRUE)
  expect_error(bg_move_sequence(player = 1, steps = 1), "`steps` must be a list", fixed = TRUE)
  expect_error(
    bg_move_sequence(player = 1, steps = list(1)),
    "Each step must be a `bg_move_step` object or a list-like step representation",
    fixed = TRUE
  )
})

test_that("bg_move_sequence enforces roll consistency for non-doubles", {
  roll <- bg_roll(2, 5)

  ok <- bg_move_sequence(
    player = 1,
    roll = roll,
    steps = list(
      bg_move_step(24, 22, 2),
      bg_move_step(22, 17, 5)
    )
  )
  expect_s3_class(ok, "bg_move_sequence")

  expect_error(
    bg_move_sequence(
      player = 1,
      roll = roll,
      steps = list(
        bg_move_step(24, 22, 2),
        bg_move_step(22, 20, 2)
      )
    ),
    "Die value 2 is used 2 times but only 1 occurrence(s) are available",
    fixed = TRUE
  )
})

test_that("bg_move_sequence enforces roll consistency for doubles", {
  roll <- bg_roll(3, 3)

  ok <- bg_move_sequence(
    player = 1,
    roll = roll,
    steps = list(
      bg_move_step(24, 21, 3),
      bg_move_step(24, 21, 3),
      bg_move_step(13, 10, 3),
      bg_move_step(13, 10, 3)
    )
  )
  expect_identical(ok$dice_used, c(3L, 3L, 3L, 3L))
  expect_identical(ok$n_steps, 4L)

  expect_error(
    bg_move_sequence(
      player = 1,
      roll = roll,
      steps = list(
        bg_move_step(24, 21, 3),
        bg_move_step(24, 21, 3),
        bg_move_step(13, 10, 3),
        bg_move_step(13, 10, 3),
        bg_move_step(8, 5, 3)
      )
    ),
    "Die value 3 is used 5 times but only 4 occurrence(s) are available",
    fixed = TRUE
  )
})

test_that("move and sequence print methods are available", {
  step <- bg_move_step(24, 21, 3)
  seqn <- bg_move_sequence(player = 1, steps = list(step), roll = bg_roll(3, 5))

  expect_output(print(step), "<bg_move_step>")
  expect_output(print(seqn), "<bg_move_sequence>")
  expect_output(print(seqn), "dice_used")
})

test_that("bg_move_sequence normalizes step-like and roll-like lists", {
  seqn <- bg_move_sequence(
    player = 1,
    roll = list(dice = c(3, 3)),
    steps = list(
      list(from = 24, to = 21, die = 3, hit = FALSE),
      list(from = 13, to = 10, die = 3)
    )
  )

  expect_s3_class(seqn, "bg_move_sequence")
  expect_s3_class(seqn$roll, "bg_roll")
  expect_true(all(vapply(seqn$steps, is_bg_move_step, logical(1L))))
  expect_identical(seqn$dice_used, c(3L, 3L))
})

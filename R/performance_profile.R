#' Profile rollout runtime components
#'
#' Profiles key runtime components for random-policy simulation workloads:
#' legal move generation, move application, one-rollout cost, and batched
#' rollout evaluation cost.
#'
#' @param board A `bg_board` object.
#' @param roll A `bg_roll` object.
#' @param legal_reps Number of repetitions for legal-move generation timing.
#' @param apply_reps Number of repetitions for move-application timing.
#' @param one_rollout_reps Number of repetitions for one-rollout timing.
#' @param total_budget Rollout budget for batched rollout timing.
#' @param rollout_policy Rollout baseline policy.
#' @param max_rollout_turns Maximum turns for each rollout.
#' @param seed Optional integer-like seed for reproducibility.
#'
#' @return A named list with timing values in seconds.
#' @export
bg_profile_runtime <- function(
    board,
    roll,
    legal_reps = 200L,
    apply_reps = 2000L,
    one_rollout_reps = 25L,
    total_budget = 256L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 200L,
    seed = NULL) {
  if (!is_bg_board(board)) {
    stop("`board` must inherit from class 'bg_board'.", call. = FALSE)
  }
  bg_validate_board(board)
  roll <- bg_as_roll(roll)

  legal_reps <- bg_coerce_integerish(legal_reps, "legal_reps", 1L)
  apply_reps <- bg_coerce_integerish(apply_reps, "apply_reps", 1L)
  one_rollout_reps <- bg_coerce_integerish(one_rollout_reps, "one_rollout_reps", 1L)
  total_budget <- bg_coerce_integerish(total_budget, "total_budget", 1L)
  rollout_policy <- bg_match_rollout_policy(rollout_policy)
  max_rollout_turns <- bg_coerce_integerish(max_rollout_turns, "max_rollout_turns", 1L)

  seed_args <- bg_normalize_seed_args(seed)

  out <- bg_cpp_profile_rollout_runtime(
    unclass(board),
    unclass(roll),
    legal_reps,
    apply_reps,
    one_rollout_reps,
    total_budget,
    rollout_policy,
    max_rollout_turns,
    seed_args$seed,
    seed_args$use_seed
  )

  out$settings <- list(
    legal_reps = legal_reps,
    apply_reps = apply_reps,
    one_rollout_reps = one_rollout_reps,
    total_budget = total_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns,
    seed = if (is.null(seed)) NULL else bg_coerce_integerish(seed, "seed", 1L)
  )
  class(out) <- "bg_runtime_profile"
  out
}

#' @export
print.bg_runtime_profile <- function(x, ...) {
  cat("<bg_runtime_profile>\n", sep = "")
  cat("n_legal_moves:            ", x$n_legal_moves[[1L]], "\n", sep = "")
  cat("legal_generation_seconds: ", format(x$legal_generation_seconds[[1L]], digits = 6), "\n", sep = "")
  cat("move_application_seconds: ", format(x$move_application_seconds[[1L]], digits = 6), "\n", sep = "")
  cat("one_rollout_seconds:      ", format(x$one_rollout_seconds[[1L]], digits = 6), "\n", sep = "")
  cat("batched_rollout_seconds:  ", format(x$batched_rollout_seconds[[1L]], digits = 6), "\n", sep = "")
  invisible(x)
}

# Rollout-based move-selection wrappers and convenience helpers.
bg_match_rollout_policy <- function(policy) {
  match.arg(policy, choices = c("random", "aggressive", "defensive"))
}

bg_normalize_rollout_args <- function(
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    unresolved_value = 0.5,
    initial_allocations = 1L,
    ucb_exploration = 1,
    prior_alpha = 1,
    prior_beta = 1) {
  alloc <- bg_normalize_allocation_args(
    total_budget = rollout_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns,
    unresolved_value = unresolved_value,
    initial_allocations = initial_allocations,
    ucb_exploration = ucb_exploration,
    prior_alpha = prior_alpha,
    prior_beta = prior_beta
  )

  c(
    list(
      rollout_budget = alloc$total_budget,
      rollout_policy = alloc$rollout_policy,
      max_rollout_turns = alloc$max_rollout_turns
    ),
    alloc[setdiff(names(alloc), c("total_budget", "rollout_policy", "max_rollout_turns"))]
  )
}

#' Evaluate legal moves by equal-allocation rollout
#'
#' Compatibility wrapper around [evaluate_actions_equal()]. A rollout is a game
#' simulation started from a candidate move and played forward under a baseline
#' policy. This function splits a fixed total rollout budget across the legal
#' moves as evenly as possible and returns one row per candidate move.
#'
#' @param board A `bg_board` object.
#' @param legal_moves A list of legal `bg_move_sequence` objects or a single
#'   `bg_move_sequence` object.
#' @param rollout_budget Integer-like scalar giving the total number of rollouts
#'   to spend across all candidate moves.
#' @param rollout_policy Baseline policy used inside rollout playouts.
#' @param max_rollout_turns Integer-like scalar giving the maximum number of
#'   turns for each rollout playout.
#' @param seed Optional integer-like scalar for reproducible rollout sampling.
#'
#' @return A data frame with one row per candidate move.
#' @export
bg_rollout_evaluate_moves <- function(
    board,
    legal_moves,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    fast_diagnostics = FALSE,
    seed = NULL) {
  evaluate_actions_equal(
    board = board,
    legal_moves = legal_moves,
    total_budget = rollout_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns,
    fast_diagnostics = fast_diagnostics,
    seed = seed
  )$results
}

#' Choose a legal move by equal-allocation rollout
#'
#' Compatibility wrapper around [evaluate_actions_equal()] that returns the
#' recommended move only.
#'
#' @inheritParams bg_rollout_evaluate_moves
#'
#' @return A single `bg_move_sequence` object.
#' @export
bg_rollout_move <- function(
    board,
    legal_moves,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    fast_diagnostics = FALSE,
    seed = NULL) {
  evaluate_actions_equal(
    board = board,
    legal_moves = legal_moves,
    total_budget = rollout_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns,
    fast_diagnostics = fast_diagnostics,
    seed = seed
  )$recommended_move
}

#' Play one turn with the equal-allocation rollout archetype
#'
#' Convenience wrapper around [bg_play_turn()] using `selection = "rollout"`.
#'
#' @inheritParams bg_play_turn
#'
#' @return A `bg_turn_result` object.
#' @export
bg_play_turn_rollout_player <- function(
    board,
    roll = NULL,
    seed = NULL,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L) {
  bg_play_turn(
    board = board,
    roll = roll,
    selection = "rollout",
    seed = seed,
    rollout_budget = rollout_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns
  )
}

#' Simulate a game between two equal-allocation rollout players
#'
#' Convenience wrapper around [bg_play_game()] using `selection = "rollout"` for
#' both players.
#'
#' @inheritParams bg_play_game
#'
#' @return A `bg_game_result` object.
#' @export
bg_play_game_rollout_players <- function(
    board = bg_initial_board(),
    roll_sequence = NULL,
    max_turns = 1000L,
    seed = NULL,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L) {
  bg_play_game(
    board = board,
    roll_sequence = roll_sequence,
    max_turns = max_turns,
    selection = "rollout",
    seed = seed,
    rollout_budget = rollout_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns
  )
}

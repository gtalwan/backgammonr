# Legacy OCBA rollout wrappers retained for compatibility with older APIs.
#' Evaluate legal moves with OCBA rollout allocation
#'
#' Compatibility wrapper around [evaluate_actions_ocba()]. The rollout budget
#' is interpreted as a fixed total budget shared across candidate legal moves.
#'
#' @param board A `bg_board` object.
#' @param legal_moves A list of legal `bg_move_sequence` objects or a single
#'   `bg_move_sequence` object.
#' @param rollout_budget Integer-like scalar giving the total rollout budget.
#' @param rollout_policy Baseline policy used inside rollout playouts.
#' @param max_rollout_turns Integer-like scalar giving the maximum number of
#'   turns per rollout playout.
#' @param seed Optional integer-like scalar for reproducible rollout sampling.
#'
#' @return A data frame with one row per candidate move.
#' @export
bg_ocba_rollout_evaluate_moves <- function(
    board,
    legal_moves,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    fast_diagnostics = FALSE,
    seed = NULL) {
  evaluate_actions_ocba(
    board = board,
    legal_moves = legal_moves,
    total_budget = rollout_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns,
    fast_diagnostics = fast_diagnostics,
    seed = seed
  )$results
}

#' Choose a legal move with OCBA rollout allocation
#'
#' Compatibility wrapper around [evaluate_actions_ocba()] that returns the
#' recommended move only.
#'
#' @inheritParams bg_ocba_rollout_evaluate_moves
#'
#' @return A single `bg_move_sequence` object.
#' @export
bg_ocba_rollout_move <- function(
    board,
    legal_moves,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    fast_diagnostics = FALSE,
    seed = NULL) {
  evaluate_actions_ocba(
    board = board,
    legal_moves = legal_moves,
    total_budget = rollout_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns,
    fast_diagnostics = fast_diagnostics,
    seed = seed
  )$recommended_move
}

#' Play one turn with the OCBA rollout archetype
#'
#' Convenience wrapper around [bg_play_turn()] using
#' `selection = "ocba_rollout"`.
#'
#' @inheritParams bg_play_turn
#'
#' @return A `bg_turn_result` object.
#' @export
bg_play_turn_ocba_rollout_player <- function(
    board,
    roll = NULL,
    seed = NULL,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L) {
  bg_play_turn(
    board = board,
    roll = roll,
    selection = "ocba_rollout",
    seed = seed,
    rollout_budget = rollout_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns
  )
}

#' Simulate a game between two OCBA rollout players
#'
#' Convenience wrapper around [bg_play_game()] using
#' `selection = "ocba_rollout"` for both players.
#'
#' @inheritParams bg_play_game
#'
#' @return A `bg_game_result` object.
#' @export
bg_play_game_ocba_rollout_players <- function(
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
    selection = "ocba_rollout",
    seed = seed,
    rollout_budget = rollout_budget,
    rollout_policy = rollout_policy,
    max_rollout_turns = max_rollout_turns
  )
}

\name{rollout-compatibility}
\alias{rollout-compatibility}
\alias{bg_rollout_evaluate_moves}
\alias{bg_rollout_move}
\alias{bg_play_turn_rollout_player}
\alias{bg_play_game_rollout_players}
\alias{bg_thompson_rollout_evaluate_moves}
\alias{bg_thompson_rollout_move}
\alias{bg_play_turn_thompson_rollout_player}
\alias{bg_play_game_thompson_rollout_players}
\title{Compatibility wrappers for rollout and Thompson-rollout players}
\usage{
bg_rollout_evaluate_moves(
  board,
  legal_moves,
  rollout_budget = 16L,
  rollout_policy = c("random", "aggressive", "defensive"),
  max_rollout_turns = 1000L,
  seed = NULL
)

bg_rollout_move(
  board,
  legal_moves,
  rollout_budget = 16L,
  rollout_policy = c("random", "aggressive", "defensive"),
  max_rollout_turns = 1000L,
  seed = NULL
)

bg_play_turn_rollout_player(
  board,
  roll = NULL,
  seed = NULL,
  rollout_budget = 16L,
  rollout_policy = c("random", "aggressive", "defensive"),
  max_rollout_turns = 1000L
)

bg_play_game_rollout_players(
  board = bg_initial_board(),
  roll_sequence = NULL,
  max_turns = 1000L,
  seed = NULL,
  rollout_budget = 16L,
  rollout_policy = c("random", "aggressive", "defensive"),
  max_rollout_turns = 1000L
)

bg_thompson_rollout_evaluate_moves(
  board,
  legal_moves,
  rollout_budget = 16L,
  rollout_policy = c("random", "aggressive", "defensive"),
  max_rollout_turns = 1000L,
  seed = NULL
)

bg_thompson_rollout_move(
  board,
  legal_moves,
  rollout_budget = 16L,
  rollout_policy = c("random", "aggressive", "defensive"),
  max_rollout_turns = 1000L,
  seed = NULL
)

bg_play_turn_thompson_rollout_player(
  board,
  roll = NULL,
  seed = NULL,
  rollout_budget = 16L,
  rollout_policy = c("random", "aggressive", "defensive"),
  max_rollout_turns = 1000L
)

bg_play_game_thompson_rollout_players(
  board = bg_initial_board(),
  roll_sequence = NULL,
  max_turns = 1000L,
  seed = NULL,
  rollout_budget = 16L,
  rollout_policy = c("random", "aggressive", "defensive"),
  max_rollout_turns = 1000L
)
}
\arguments{
  \item{board}{A \code{bg_board} object.}
  \item{legal_moves}{List of legal \code{bg_move_sequence} objects.}
  \item{rollout_budget}{Fixed total rollout budget.}
  \item{rollout_policy}{Baseline policy used inside rollouts.}
  \item{max_rollout_turns}{Maximum number of turns allowed in each rollout game.}
  \item{seed}{Optional nonnegative integer-like scalar used to seed the C++ RNG.}
  \item{roll}{Optional \code{bg_roll}.}
  \item{roll_sequence}{Optional list of pre-specified \code{bg_roll} objects.}
  \item{max_turns}{Maximum number of turns allowed in a game.}
}
\value{
Evaluation functions return data frames of move-level summaries. Choice helpers
return a selected \code{bg_move_sequence}. Turn and game wrappers return the
same classes as the main engine.
}
\description{
Backward-compatible wrappers around the new fixed-budget allocation framework.
These functions preserve the original package interface while using the refactored
equal-allocation or Thompson-sampling core under the hood.
}

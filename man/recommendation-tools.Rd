\name{recommendation-tools}
\alias{recommendation-tools}
\alias{bg_rank_moves}
\alias{bg_recommend_move}
\alias{bg_explain_recommendation}
\alias{print.bg_move_recommendation}
\title{Rank moves, recommend a move, and explain the recommendation}
\usage{
bg_rank_moves(
  board,
  roll,
  method = c("thompson", "equal", "greedy", "ucb"),
  total_budget = 32L,
  rollout_policy = c("random", "aggressive", "defensive"),
  max_rollout_turns = 1000L,
  unresolved_value = 0.5,
  initial_allocations = 1L,
  ucb_exploration = 1,
  prior_alpha = 1,
  prior_beta = 1,
  seed = NULL
)

bg_recommend_move(
  board,
  roll,
  method = c("thompson", "equal", "greedy", "ucb"),
  total_budget = 32L,
  rollout_policy = c("random", "aggressive", "defensive"),
  max_rollout_turns = 1000L,
  unresolved_value = 0.5,
  initial_allocations = 1L,
  ucb_exploration = 1,
  prior_alpha = 1,
  prior_beta = 1,
  seed = NULL
)

bg_explain_recommendation(
  x,
  roll = NULL,
  method = c("thompson", "equal", "greedy", "ucb"),
  total_budget = 32L,
  rollout_policy = c("random", "aggressive", "defensive"),
  max_rollout_turns = 1000L,
  unresolved_value = 0.5,
  initial_allocations = 1L,
  ucb_exploration = 1,
  prior_alpha = 1,
  prior_beta = 1,
  seed = NULL
)

\method{print}{bg_move_recommendation}(x, ...)
}
\arguments{
  \item{board}{A \code{bg_board} object.}
  \item{roll}{A \code{bg_roll}.}
  \item{method}{Allocation rule used for move evaluation.}
  \item{total_budget}{Fixed total rollout budget.}
  \item{rollout_policy}{Baseline policy used inside rollout games.}
  \item{max_rollout_turns}{Maximum number of turns allowed in each rollout game.}
  \item{unresolved_value}{Numeric value in \code{[0, 1]} assigned to unresolved rollouts.}
  \item{initial_allocations}{Number of initial allocations per move for adaptive methods.}
  \item{ucb_exploration}{Exploration constant used by UCB.}
  \item{prior_alpha, prior_beta}{Positive Beta prior parameters used by Bayesian summaries.}
  \item{seed}{Optional nonnegative integer-like scalar used to seed the C++ RNG.}
  \item{x}{Either a \code{bg_move_recommendation} object or a \code{bg_board}.}
  \item{...}{Unused.}
}
\value{
\code{bg_rank_moves()} returns a ranked data frame with move labels, estimated
values, uncertainty summaries, and a \code{move} list-column of
\code{bg_move_sequence} objects. \code{bg_recommend_move()} returns a
\code{"bg_move_recommendation"} object containing the selected move, ranking,
and a short explanation. \code{bg_explain_recommendation()} returns a character
scalar and the print method returns the input invisibly.
}
\description{
Gamer-facing recommendation helpers that generate legal moves, rank them, select
a move under a chosen rollout-allocation rule, and explain the decision in
plain language.
}
\examples{
points <- integer(24)
points[8] <- 1L
points[17] <- -1L
board <- bg_position(points = points, off = c(14L, 14L), turn = 1L)
roll <- bg_roll(3, 2)

ranking <- bg_rank_moves(board, roll, method = "thompson", total_budget = 8L, seed = 123)
ranking

rec <- bg_recommend_move(board, roll, method = "thompson", total_budget = 8L, seed = 123)
rec
bg_explain_recommendation(rec)
}

\name{allocation-methods}
\alias{allocation-methods}
\alias{evaluate_actions_equal}
\alias{evaluate_actions_greedy}
\alias{evaluate_actions_ucb}
\alias{evaluate_actions_thompson}
\alias{approximate_action_truth}
\alias{compute_regret}
\alias{compute_best_action_accuracy}
\alias{compute_mse}
\alias{stratify_positions_by_difficulty}
\alias{print.bg_action_evaluation}
\title{Evaluate legal moves under fixed-budget rollout-allocation rules}
\usage{
evaluate_actions_equal(
  board,
  roll = NULL,
  legal_moves = NULL,
  total_budget = 16L,
  rollout_policy = c("random", "aggressive", "defensive"),
  max_rollout_turns = 1000L,
  unresolved_value = 0.5,
  initial_allocations = 1L,
  ucb_exploration = 1,
  prior_alpha = 1,
  prior_beta = 1,
  seed = NULL
)

evaluate_actions_greedy(
  board,
  roll = NULL,
  legal_moves = NULL,
  total_budget = 16L,
  rollout_policy = c("random", "aggressive", "defensive"),
  max_rollout_turns = 1000L,
  unresolved_value = 0.5,
  initial_allocations = 1L,
  ucb_exploration = 1,
  prior_alpha = 1,
  prior_beta = 1,
  seed = NULL
)

evaluate_actions_ucb(
  board,
  roll = NULL,
  legal_moves = NULL,
  total_budget = 16L,
  rollout_policy = c("random", "aggressive", "defensive"),
  max_rollout_turns = 1000L,
  unresolved_value = 0.5,
  initial_allocations = 1L,
  ucb_exploration = 1,
  prior_alpha = 1,
  prior_beta = 1,
  seed = NULL
)

evaluate_actions_thompson(
  board,
  roll = NULL,
  legal_moves = NULL,
  total_budget = 16L,
  rollout_policy = c("random", "aggressive", "defensive"),
  max_rollout_turns = 1000L,
  unresolved_value = 0.5,
  initial_allocations = 1L,
  ucb_exploration = 1,
  prior_alpha = 1,
  prior_beta = 1,
  seed = NULL
)

approximate_action_truth(
  board,
  roll = NULL,
  legal_moves = NULL,
  truth_budget = 512L,
  rollout_policy = c("random", "aggressive", "defensive"),
  max_rollout_turns = 1000L,
  unresolved_value = 0.5,
  prior_alpha = 1,
  prior_beta = 1,
  seed = NULL
)

compute_regret(selected_value, best_value)

compute_best_action_accuracy(chosen_index, truth_best_index)

compute_mse(estimate, truth)

stratify_positions_by_difficulty(
  x,
  breaks = c(-Inf, 0.02, 0.05, Inf),
  labels = c("hard", "moderate", "easy")
)

\method{print}{bg_action_evaluation}(x, ...)
}
\arguments{
  \item{board}{A \code{bg_board} object.}
  \item{roll}{Optional \code{bg_roll}. If omitted, \code{legal_moves} must be supplied.}
  \item{legal_moves}{Optional list of legal \code{bg_move_sequence} objects.}
  \item{total_budget}{Fixed total number of rollout simulations available across all legal moves.}
  \item{truth_budget}{Large rollout budget used to approximate the true ordering of the legal moves.}
  \item{rollout_policy}{Baseline policy used inside rollout games.}
  \item{max_rollout_turns}{Maximum number of turns allowed in each rollout game.}
  \item{unresolved_value}{Numeric value in \code{[0, 1]} used for unresolved rollout paths.}
  \item{initial_allocations}{Number of initial allocations per move for adaptive methods.}
  \item{ucb_exploration}{Exploration constant used by the UCB rule.}
  \item{prior_alpha, prior_beta}{Positive Beta prior parameters used by the Bayesian summaries.}
  \item{seed}{Optional nonnegative integer-like scalar used to seed the C++ RNG.}
  \item{selected_value, best_value}{Numeric values used to compute simple regret.}
  \item{chosen_index, truth_best_index}{Chosen and truly best move indices used to compute correct-selection indicators.}
  \item{estimate, truth}{Numeric vectors of estimated and truth values.}
  \item{x}{Numeric difficulty gaps or an object to print.}
  \item{breaks, labels}{Breakpoints and labels used to classify position difficulty.}
  \item{...}{Unused.}
}
\value{
Evaluation functions return \code{"bg_action_evaluation"} objects containing the
board, roll, legal moves, ranked move-level results, the recommended move index,
and settings. The metric helpers return numeric scalars. Difficulty
stratification returns a factor. The print method returns the input invisibly.
}
\description{
Research-facing action-evaluation tools centered on the question:
under a fixed simulation budget, how should rollouts be allocated across legal
moves to identify the best move?

The equal-allocation baseline spreads the budget evenly, the greedy method
allocates to the currently best posterior mean, UCB adds an exploration bonus,
and Thompson sampling samples from move-specific Beta posteriors.
}
\examples{
points <- integer(24)
points[8] <- 1L
points[17] <- -1L
board <- bg_position(points = points, off = c(14L, 14L), turn = 1L)

eval <- evaluate_actions_thompson(
  board = board,
  roll = bg_roll(3, 2),
  total_budget = 8L,
  seed = 123
)
eval
compute_regret(0.6, 0.75)
}

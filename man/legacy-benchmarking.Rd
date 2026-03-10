\name{legacy-benchmarking}
\alias{legacy-benchmarking}
\alias{benchmark_matchup}
\alias{benchmark_move_evaluators}
\alias{bg_benchmark_case}
\alias{is_bg_benchmark_case}
\alias{is_bg_matchup_benchmark}
\alias{is_bg_move_evaluation_benchmark}
\alias{print.bg_matchup_benchmark}
\alias{summary.bg_matchup_benchmark}
\alias{print.bg_move_evaluation_benchmark}
\alias{summary.bg_move_evaluation_benchmark}
\title{Legacy matchup and move-evaluation benchmarking helpers}
\usage{
benchmark_matchup(
  player1 = c("random", "aggressive", "defensive", "rollout", "equal_rollout",
              "greedy_rollout", "ucb_rollout", "thompson_rollout"),
  player2 = c("random", "aggressive", "defensive", "rollout", "equal_rollout",
              "greedy_rollout", "ucb_rollout", "thompson_rollout"),
  n_games = 100L,
  board = bg_initial_board(),
  max_turns = 1000L,
  roll_sequence = NULL,
  seed = NULL,
  rollout_budget = 16L,
  rollout_policy = c("random", "aggressive", "defensive"),
  max_rollout_turns = 1000L
)

bg_benchmark_case(board, roll, case_id = NULL)

benchmark_move_evaluators(
  cases,
  methods = c("random", "aggressive", "defensive", "rollout", "equal_rollout",
              "greedy_rollout", "ucb_rollout", "thompson_rollout"),
  reference_method = NULL,
  rollout_budget = 16L,
  rollout_policy = c("random", "aggressive", "defensive"),
  max_rollout_turns = 1000L,
  reference_rollout_budget = rollout_budget,
  reference_rollout_policy = rollout_policy,
  reference_max_rollout_turns = max_rollout_turns,
  seed = NULL
)

is_bg_benchmark_case(x)

is_bg_matchup_benchmark(x)

is_bg_move_evaluation_benchmark(x)

\method{print}{bg_matchup_benchmark}(x, ...)

\method{summary}{bg_matchup_benchmark}(object, ...)

\method{print}{bg_move_evaluation_benchmark}(x, ...)

\method{summary}{bg_move_evaluation_benchmark}(object, ...)
}
\arguments{
  \item{player1, player2}{Player archetypes.}
  \item{n_games}{Number of games to simulate.}
  \item{board}{Initial \code{bg_board}.}
  \item{max_turns}{Maximum number of turns per game.}
  \item{roll_sequence}{Optional scripted roll sequence.}
  \item{seed}{Optional nonnegative integer-like scalar used to seed the C++ RNG.}
  \item{rollout_budget, reference_rollout_budget}{Fixed total rollout budgets.}
  \item{rollout_policy, reference_rollout_policy}{Baseline rollout policies.}
  \item{max_rollout_turns, reference_max_rollout_turns}{Maximum rollout lengths.}
  \item{roll}{A \code{bg_roll}.}
  \item{case_id}{Optional identifier for the benchmark case.}
  \item{cases}{List of benchmark cases.}
  \item{methods}{Methods to benchmark.}
  \item{reference_method}{Optional reference method for move-evaluation benchmarking.}
  \item{x, object}{Objects to test, print, or summarize.}
  \item{...}{Unused.}
}
\value{
Benchmark constructors and simulators return S3 objects containing benchmark
results and summaries. Predicate helpers return logical scalars.
}
\description{
Legacy benchmarking helpers retained for compatibility with the original package
API. The package also provides newer research-focused benchmarking tools through
\code{\link{benchmark_allocation_methods}}.
}

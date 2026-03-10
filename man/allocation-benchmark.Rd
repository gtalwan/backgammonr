\name{allocation-benchmark}
\alias{allocation-benchmark}
\alias{benchmark_allocation_methods}
\alias{print.bg_allocation_benchmark}
\title{Benchmark fixed-budget rollout-allocation methods}
\usage{
benchmark_allocation_methods(
  cases,
  methods = c("equal", "greedy", "ucb", "thompson"),
  total_budget = 32L,
  truth_budget = 256L,
  rollout_policy = c("random", "aggressive", "defensive"),
  max_rollout_turns = 1000L,
  unresolved_value = 0.5,
  initial_allocations = 1L,
  ucb_exploration = 1,
  prior_alpha = 1,
  prior_beta = 1,
  seed = NULL
)

\method{print}{bg_allocation_benchmark}(x, ...)
}
\arguments{
  \item{cases}{List of \code{bg_benchmark_case} objects.}
  \item{methods}{Allocation rules to benchmark.}
  \item{total_budget}{Fixed rollout budget for each tested method.}
  \item{truth_budget}{Large rollout budget used to approximate the truth.}
  \item{rollout_policy}{Baseline policy used inside rollout games.}
  \item{max_rollout_turns}{Maximum number of turns allowed in each rollout game.}
  \item{unresolved_value}{Numeric value in \code{[0, 1]} assigned to unresolved rollouts.}
  \item{initial_allocations}{Number of initial allocations per move for adaptive methods.}
  \item{ucb_exploration}{Exploration constant used by UCB.}
  \item{prior_alpha, prior_beta}{Positive Beta prior parameters used by Bayesian summaries.}
  \item{seed}{Optional nonnegative integer-like scalar used to seed the C++ RNG.}
  \item{x}{An object to print.}
  \item{...}{Unused.}
}
\value{
\code{benchmark_allocation_methods()} returns a \code{"bg_allocation_benchmark"}
object with case-level results, method summaries, truth approximations, and
settings. The print method returns the input invisibly.
}
\description{
Benchmarks equal allocation, greedy allocation, UCB, and Thompson sampling on a
collection of backgammon move-evaluation problems under common compute
constraints. Reported metrics include probability of correct selection, simple
regret, MSE, runtime, and difficulty labels derived from approximate truth gaps.
}
\examples{
points <- integer(24)
points[8] <- 1L
points[17] <- -1L
case <- bg_benchmark_case(
  board = bg_position(points = points, off = c(14L, 14L), turn = 1L),
  roll = bg_roll(3, 2),
  case_id = "demo"
)

bm <- benchmark_allocation_methods(
  cases = list(case),
  methods = c("equal", "thompson"),
  total_budget = 8L,
  truth_budget = 32L,
  seed = 123
)
bm
}

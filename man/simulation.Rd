\name{simulation-tools}
\alias{simulation-tools}
\alias{simulate_matchup}
\alias{is_bg_matchup_simulation}
\alias{print.bg_matchup_simulation}
\alias{summary.bg_matchup_simulation}
\title{Simulate repeated matchups between player archetypes}
\usage{
simulate_matchup(
  player1 = c("random", "aggressive", "defensive", "rollout", "equal_rollout",
              "greedy_rollout", "ucb_rollout", "thompson_rollout"),
  player2 = c("random", "aggressive", "defensive", "rollout", "equal_rollout",
              "greedy_rollout", "ucb_rollout", "thompson_rollout"),
  n_games = 100L,
  board = bg_initial_board(),
  max_turns = 1000L,
  roll_sequence = NULL,
  seed = NULL,
  include_summary = TRUE,
  rollout_budget = 16L,
  rollout_policy = c("random", "aggressive", "defensive"),
  max_rollout_turns = 1000L
)

is_bg_matchup_simulation(x)

\method{print}{bg_matchup_simulation}(x, ...)

\method{summary}{bg_matchup_simulation}(object, ...)
}
\arguments{
  \item{player1, player2}{Archetypes for the two sides.}
  \item{n_games}{Number of games to simulate.}
  \item{board}{Initial \code{bg_board}.}
  \item{max_turns}{Maximum number of turns per game.}
  \item{roll_sequence}{Optional scripted roll sequence.}
  \item{seed}{Optional nonnegative integer-like scalar used to seed the C++ RNG.}
  \item{include_summary}{Logical scalar indicating whether to compute a summary table.}
  \item{rollout_budget}{Fixed total rollout budget used by rollout-based players.}
  \item{rollout_policy}{Baseline policy used inside rollouts.}
  \item{max_rollout_turns}{Maximum number of turns allowed in each rollout game.}
  \item{x, object}{Objects to test, print, or summarize.}
  \item{...}{Unused.}
}
\value{
\code{simulate_matchup()} returns a \code{"bg_matchup_simulation"} object with
game-level results and, optionally, summary statistics. The predicate returns a
logical scalar and the print and summary methods return useful displays.
}
\description{
Repeated-game simulation for comparing player archetypes under common settings.
}
\examples{
sim <- simulate_matchup(
  player1 = "random",
  player2 = "defensive",
  n_games = 5L,
  seed = 123
)
sim
summary(sim)
}

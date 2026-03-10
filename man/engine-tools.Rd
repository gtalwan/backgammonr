\name{engine-tools}
\alias{engine-tools}
\alias{bg_play_turn}
\alias{bg_play_game}
\alias{bg_play_game_matchup}
\alias{is_bg_turn_result}
\alias{is_bg_game_result}
\alias{print.bg_turn_result}
\alias{print.bg_game_result}
\title{Play turns and games with the backgammon engine}
\usage{
bg_play_turn(
  board,
  roll = NULL,
  selection = c("first", "random", "aggressive", "defensive", "rollout",
                "equal_rollout", "greedy_rollout", "ucb_rollout", "thompson_rollout"),
  seed = NULL,
  rollout_budget = 16L,
  rollout_policy = c("random", "aggressive", "defensive"),
  max_rollout_turns = 1000L
)

bg_play_game(
  board = bg_initial_board(),
  roll_sequence = NULL,
  max_turns = 1000L,
  selection = c("first", "random", "aggressive", "defensive", "rollout",
                "equal_rollout", "greedy_rollout", "ucb_rollout", "thompson_rollout"),
  seed = NULL,
  rollout_budget = 16L,
  rollout_policy = c("random", "aggressive", "defensive"),
  max_rollout_turns = 1000L
)

bg_play_game_matchup(
  board = bg_initial_board(),
  player1 = c("first", "random", "aggressive", "defensive", "rollout",
              "equal_rollout", "greedy_rollout", "ucb_rollout", "thompson_rollout"),
  player2 = c("first", "random", "aggressive", "defensive", "rollout",
              "equal_rollout", "greedy_rollout", "ucb_rollout", "thompson_rollout"),
  roll_sequence = NULL,
  max_turns = 1000L,
  seed = NULL,
  rollout_budget = 16L,
  rollout_policy = c("random", "aggressive", "defensive"),
  max_rollout_turns = 1000L
)

is_bg_turn_result(x)

is_bg_game_result(x)

\method{print}{bg_turn_result}(x, ...)

\method{print}{bg_game_result}(x, ...)
}
\arguments{
  \item{board}{A \code{bg_board} object.}
  \item{roll}{Optional \code{bg_roll}. If \code{NULL}, dice are rolled in C++.}
  \item{selection, player1, player2}{Decision rule used to choose among legal moves.}
  \item{seed}{Optional nonnegative integer-like scalar used to seed the C++ RNG.}
  \item{rollout_budget}{Fixed total rollout budget used by rollout-based selectors.}
  \item{rollout_policy}{Baseline policy used inside rollouts.}
  \item{max_rollout_turns}{Maximum number of turns allowed in each rollout game.}
  \item{roll_sequence}{Optional list of pre-specified \code{bg_roll} objects.}
  \item{max_turns}{Maximum number of turns allowed in the played game.}
  \item{x}{An object to test or print.}
  \item{...}{Unused.}
}
\value{
\code{bg_play_turn()} returns a \code{"bg_turn_result"} object and
\code{bg_play_game()} / \code{bg_play_game_matchup()} return
\code{"bg_game_result"} objects. Predicate helpers return logical scalars and
print methods return their input invisibly.
}
\description{
Backgammon engine entry points for single turns, full games, and fixed-player
matchups. Rollout-based selection rules use a fixed total simulation budget.
}
\examples{
turn <- bg_play_turn(bg_initial_board(), selection = "random", seed = 123)
turn

game <- bg_play_game(selection = "aggressive", max_turns = 10L, seed = 123)
game
}

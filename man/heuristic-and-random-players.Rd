\name{heuristic-and-random-players}
\alias{heuristic-and-random-players}
\alias{bg_random_move}
\alias{bg_play_turn_random_player}
\alias{bg_play_game_random_players}
\alias{bg_score_board_aggressive}
\alias{bg_score_board_defensive}
\alias{bg_aggressive_move}
\alias{bg_defensive_move}
\alias{bg_play_turn_aggressive_player}
\alias{bg_play_turn_defensive_player}
\alias{bg_play_game_heuristic_players}
\alias{bg_play_game_aggressive_players}
\alias{bg_play_game_defensive_players}
\title{Random and heuristic player helpers}
\usage{
bg_random_move(legal_moves, seed = NULL)

bg_play_turn_random_player(board, roll = NULL, seed = NULL)

bg_play_game_random_players(board = bg_initial_board(), roll_sequence = NULL, max_turns = 1000L, seed = NULL)

bg_score_board_aggressive(board, player = NULL)

bg_score_board_defensive(board, player = NULL)

bg_aggressive_move(board, legal_moves)

bg_defensive_move(board, legal_moves)

bg_play_turn_aggressive_player(board, roll = NULL, seed = NULL)

bg_play_turn_defensive_player(board, roll = NULL, seed = NULL)

bg_play_game_heuristic_players(
  board = bg_initial_board(),
  player1 = c("aggressive", "defensive"),
  player2 = c("aggressive", "defensive"),
  roll_sequence = NULL,
  max_turns = 1000L,
  seed = NULL
)

bg_play_game_aggressive_players(board = bg_initial_board(), roll_sequence = NULL, max_turns = 1000L, seed = NULL)

bg_play_game_defensive_players(board = bg_initial_board(), roll_sequence = NULL, max_turns = 1000L, seed = NULL)
}
\arguments{
  \item{legal_moves}{List of legal \code{bg_move_sequence} objects.}
  \item{seed}{Optional nonnegative integer-like scalar used to seed the C++ RNG.}
  \item{board}{A \code{bg_board} object.}
  \item{roll}{Optional \code{bg_roll}. If omitted, dice are rolled in C++.}
  \item{roll_sequence}{Optional list of pre-specified \code{bg_roll} objects.}
  \item{max_turns}{Maximum number of turns allowed in a game.}
  \item{player}{Optional player identifier; by default the board turn is used.}
  \item{player1, player2}{Heuristic archetypes for the two sides.}
}
\value{
Move-selection helpers return a \code{bg_move_sequence}. Board-scoring helpers
return numeric scores. Turn and game helpers return the same \code{bg_turn_result}
and \code{bg_game_result} objects used by the main engine.
}
\description{
Convenience interfaces for random, aggressive, and defensive play. The
aggressive heuristic favors hitting and blocking, while the defensive heuristic
favors safer structures and fewer exposed blots.
}
\examples{
board <- bg_initial_board()
roll <- bg_roll(3, 2)
moves <- bg_legal_moves(board, roll)

bg_random_move(moves, seed = 123)
bg_score_board_aggressive(board)
bg_score_board_defensive(board)
}

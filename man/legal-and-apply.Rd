\name{legal-and-apply}
\alias{legal-and-apply}
\alias{bg_legal_moves}
\alias{bg_apply_move_sequence}
\title{Generate legal moves and apply selected move sequences}
\usage{
bg_legal_moves(board, roll, player = NULL)

bg_apply_move_sequence(board, move_sequence)
}
\arguments{
  \item{board}{A \code{bg_board} object.}
  \item{roll}{A \code{bg_roll} object.}
  \item{player}{Optional player identifier; by default the board turn is used.}
  \item{move_sequence}{A \code{bg_move_sequence} object.}
}
\value{
\code{bg_legal_moves()} returns a list of \code{bg_move_sequence} objects
representing legal full-turn plays for the supplied roll. \code{bg_apply_move_sequence()}
returns the resulting \code{bg_board}.
}
\description{
Compiled legal-move generation and state transition helpers. The move generator
handles normal movement, blocked points, hitting blots, entry from the bar,
bearing off, doubles, and forced-use rules.
}
\examples{
points <- integer(24)
points[8] <- 1L
points[17] <- -1L
board <- bg_board(points = points, off = c(14L, 14L), turn = 1L)
roll <- bg_roll(3, 2)

moves <- bg_legal_moves(board, roll)
moves[[1]]
bg_apply_move_sequence(board, moves[[1]])
}

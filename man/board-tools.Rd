\name{board-tools}
\alias{board-tools}
\alias{bg_board}
\alias{bg_position}
\alias{bg_initial_board}
\alias{bg_clone_board}
\alias{bg_validate_board}
\alias{bg_inspect_board}
\alias{is_bg_board}
\alias{print.bg_board}
\title{Create, validate, inspect, and print backgammon boards}
\usage{
bg_board(points, bar = c(0, 0), off = c(0, 0), turn = 1L, validate = TRUE)

bg_position(points, bar = c(0, 0), off = c(0, 0), turn = 1L, validate = TRUE)

bg_initial_board(turn = 1L)

bg_clone_board(board)

bg_validate_board(board, error = TRUE)

bg_inspect_board(board)

is_bg_board(x)

\method{print}{bg_board}(x, ...)
}
\arguments{
  \item{points}{Integer-like vector of length 24 giving signed point counts. Positive values belong to player 1 and negative values belong to player 2.}
  \item{bar}{Integer-like vector of length 2 giving bar counts for players 1 and 2.}
  \item{off}{Integer-like vector of length 2 giving borne-off counts for players 1 and 2.}
  \item{turn}{Integer-like scalar equal to \code{1L} or \code{-1L}.}
  \item{validate}{Logical scalar; if \code{TRUE}, validate the board before returning it.}
  \item{board}{A \code{bg_board} object or list-like board representation.}
  \item{error}{Logical scalar; if \code{TRUE}, invalid boards raise an error, otherwise a validation report is returned.}
  \item{x}{An object to test or print.}
  \item{...}{Unused.}
}
\value{
\itemize{
  \item \code{bg_board()}, \code{bg_position()}, \code{bg_initial_board()}, and \code{bg_clone_board()} return objects of class \code{"bg_board"}.
  \item \code{bg_validate_board()} returns \code{TRUE} invisibly for valid boards when \code{error = TRUE}; otherwise it returns a list with \code{valid} and \code{messages}.
  \item \code{bg_inspect_board()} returns a compact named list for debugging and testing.
  \item \code{is_bg_board()} returns a logical scalar.
  \item The print method returns the input invisibly.
}
}
\description{
Board tools for constructing custom positions, creating the standard opening
position, validating structural consistency, and printing or inspecting a board.
Validation rejects impossible states in which both players have already borne
off all 15 checkers.
}
\examples{
board <- bg_initial_board()
board

points <- integer(24)
points[8] <- 1L
points[17] <- -1L
pos <- bg_position(points = points, off = c(14L, 14L), turn = 1L)
bg_validate_board(pos)
bg_inspect_board(pos)
}

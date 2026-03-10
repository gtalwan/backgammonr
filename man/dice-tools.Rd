\name{dice-tools}
\alias{dice-tools}
\alias{bg_roll}
\alias{bg_roll_dice}
\alias{is_bg_roll}
\alias{print.bg_roll}
\title{Create, simulate, and print backgammon dice rolls}
\usage{
bg_roll(die1, die2)

bg_roll_dice(n = 1L, seed = NULL)

is_bg_roll(x)

\method{print}{bg_roll}(x, ...)
}
\arguments{
  \item{die1, die2}{Integer-like scalars in \code{1:6}.}
  \item{n}{Integer-like scalar giving the number of rolls to draw.}
  \item{seed}{Optional nonnegative integer-like scalar used to seed the C++ RNG.}
  \item{x}{An object to test or print.}
  \item{...}{Unused.}
}
\value{
\code{bg_roll()} returns an object of class \code{"bg_roll"} with fields
\code{dice}, \code{is_double}, and \code{expanded}. \code{bg_roll_dice()}
returns either a single \code{bg_roll} or a list of \code{bg_roll} objects.
\code{is_bg_roll()} returns a logical scalar and the print method returns the
input invisibly.
}
\description{
Utilities for explicit roll construction and reproducible dice simulation using
the compiled C++ core.
}
\examples{
bg_roll(3, 2)
bg_roll_dice(seed = 123)
bg_roll_dice(n = 3, seed = 123)
}

\name{move-tools}
\alias{move-tools}
\alias{bg_move_step}
\alias{bg_move_sequence}
\alias{is_bg_move_step}
\alias{is_bg_move_sequence}
\alias{print.bg_move_step}
\alias{print.bg_move_sequence}
\title{Create, test, and print move steps and move sequences}
\usage{
bg_move_step(from, to, die, hit = FALSE)

bg_move_sequence(player, steps = list(), roll = NULL)

is_bg_move_step(x)

is_bg_move_sequence(x)

\method{print}{bg_move_step}(x, ...)

\method{print}{bg_move_sequence}(x, ...)
}
\arguments{
  \item{from, to}{Integer-like point indices. Use \code{0L} for the bar and \code{25L} for bearing off.}
  \item{die}{Integer-like die value used by the step.}
  \item{hit}{Logical scalar indicating whether the destination blot is hit.}
  \item{player}{Integer-like scalar equal to \code{1L} or \code{-1L}.}
  \item{steps}{List of move-step objects. A sequence must contain at least one step.}
  \item{roll}{Optional \code{bg_roll} object recording the roll that generated the sequence.}
  \item{x}{An object to test or print.}
  \item{...}{Unused.}
}
\value{
\code{bg_move_step()} returns an object of class \code{"bg_move_step"}.
\code{bg_move_sequence()} returns an object of class \code{"bg_move_sequence"}.
Predicate functions return logical scalars and print methods return their input
invisibly.
}
\description{
Data structures for individual checker movements and full-turn move sequences.
These objects are used by the legal-move generator, recommendation routines,
and the game engine.
}
\examples{
step <- bg_move_step(from = 8, to = 5, die = 3, hit = FALSE)
step

seq <- bg_move_sequence(
  player = 1L,
  steps = list(step),
  roll = bg_roll(3, 2)
)
seq
}

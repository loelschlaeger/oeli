% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_probability_vector.R
\name{check_probability_vector}
\alias{check_probability_vector}
\alias{assert_probability_vector}
\alias{test_probability_vector}
\title{Check if an argument is a probability vector}
\usage{
check_probability_vector(x, len = NULL, tolerance = sqrt(.Machine$double.eps))

assert_probability_vector(
  x,
  len = NULL,
  tolerance = sqrt(.Machine$double.eps),
  .var.name = checkmate::vname(x),
  add = NULL
)

test_probability_vector(x, len = NULL, tolerance = sqrt(.Machine$double.eps))
}
\arguments{
\item{x}{Object to check.}

\item{len}{[\code{integer(1)}]\cr
Exact expected length of \code{x}.}

\item{tolerance}{A non-negative \code{numeric} tolerance value.}

\item{.var.name}{[\code{character(1)}]\cr
Name of the checked object to print in assertions. Defaults to
the heuristic implemented in \code{\link[checkmate]{vname}}.}

\item{add}{[\code{AssertCollection}]\cr
Collection to store assertion messages. See \code{\link[checkmate]{AssertCollection}}.}
}
\value{
Compare to \code{\link[checkmate]{check_numeric}}.
}
\description{
This function checks whether the input is a real vector with non-negative
entries that add up to one.
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chunk_vector.R
\name{chunk_vector}
\alias{chunk_vector}
\title{Split a vector into chunks}
\usage{
chunk_vector(x, n, type = 1, strict = FALSE)
}
\arguments{
\item{x}{A \code{vector}.}

\item{n}{An \code{integer} smaller or equal \code{length(x)}.}

\item{type}{Either \code{1} (default) to split \code{x} into \code{n} chunks of equal
size or \code{2} to split \code{x} into chunks of size \code{n}.}

\item{strict}{Set to \code{TRUE} to fail if \code{length(x)} is not a multiple of \code{n},
or \code{FALSE} (default), else.}
}
\value{
A \code{list}.
}
\description{
This function either
\itemize{
\item splits a vector into \code{n} chunks of equal size (\code{type = 1}),
\item splits a vector into chunks of size \code{n} (\code{type = 2}).
}
}
\examples{
x <- 1:12
chunk_vector(x, n = 3, type = 1)
chunk_vector(x, n = 3, type = 2)
try(chunk_vector(x, n = 5, strict = TRUE))
}

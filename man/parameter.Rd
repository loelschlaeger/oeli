% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hidden_markov.R
\name{parameter}
\alias{parameter}
\alias{parameter_template}
\alias{print.parameter}
\alias{validate.parameter}
\alias{as.vector.parameter}
\alias{as.list.parameter}
\title{Define HMM parameter}
\usage{
parameter(hmm, ..., validate = FALSE)

parameter_template(hmm)

\method{print}{parameter}(x, ...)

validate.parameter(x, hmm)

\method{as.vector}{parameter}(x, mode = "numeric")

\method{as.list}{parameter}(x, hmm, ..., validate = FALSE)
}
\arguments{
\item{hmm}{An object of class \code{\link{hmm}}.}

\item{...}{Different HMM parameters:
\itemize{
\item \code{"tpm"}, the transition probability matrix (optional)
\item \code{"init"}, the initial state distribution or \code{"stationary"}
for the stationary distribution of \code{"tpm"} (optional)
\item parameters of the state-dependent distribution, see \code{hmm$sdd}
(required)
}}

\item{validate}{Either \code{TRUE} to validate the parameters (i.e., check for completeness
and restrictions) or \code{FALSE}, else.}

\item{x}{An object of class \code{\link{parameter}}.}

\item{mode}{Ignored.}
}
\value{
An object of class \code{\link{parameter}}.
}
\description{
This function defines parameters of a hidden Markov model.
}

#' Stationary distribution
#'
#' @description
#' This function computes the stationary distribution corresponding to a
#' transition probability matrix.
#'
#' @param tpm
#' A transition probability \code{matrix}.
#' @param soft_fail
#' Either \code{TRUE} to return the discrete uniform distribution if the
#' computation of the stationary distribution fails for some reason, or
#' \code{FALSE} to throw an error.
#'
#' @return
#' A \code{numeric} vector.
#'
#' @examples
#' tpm <- matrix(0.05, nrow = 3, ncol = 3)
#' diag(tpm) <- 0.9
#' stationary_distribution(tpm)
#'
#' @export

stationary_distribution <- function(tpm, soft_fail = FALSE) {
  assert_transition_probability_matrix(tpm)
  checkmate::assert_flag(soft_fail)
  dim <- nrow(tpm)
  stat_distr <- try(solve(t(diag(dim) - tpm + 1), rep(1, dim)), silent = TRUE)
  if (inherits(stat_distr, "try-error")) {
    if (soft_fail) {
      stat_distr <- rep(1, dim) / dim
    } else {
      stop(as.character(stat_distr))
    }
  }
  return(stat_distr)
}

#' Simulate Markov chain
#'
#' @description
#' This function simulates a Markov chain.
#'
#' @param Gamma
#' A transition probability \code{matrix}.
#' @param T
#' An \code{integer}, the length of the Markov chain.
#' @param delta
#' A \code{numeric} probability vector, the initial distribution.
#' If not specified, \code{delta} is set to the stationary distribution of
#' \code{Gamma}.
#'
#' @return
#' A \code{numeric} vector of length \code{T} with states.
#'
#' @examples
#' Gamma <- sample_transition_probability_matrix(dim = 3)
#' simulate_markov_chain(Gamma = Gamma, T = 10)
#'
#' @export

simulate_markov_chain <- function(
    Gamma, T, delta = stationary_distribution(Gamma)
  ) {
  assert_transition_probability_matrix(Gamma)
  checkmate::assert_int(T, lower = 1)
  assert_probability_vector(delta, len = nrow(Gamma))
  N <- length(delta)
  markov_chain <- numeric(T)
  markov_chain[1] <- sample(1:N, 1, prob = delta)
  for (t in 2:T) {
    markov_chain[t] <- sample(1:N, 1, prob = Gamma[markov_chain[t - 1], ])
  }
  markov_chain
}

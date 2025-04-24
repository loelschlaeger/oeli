#' Simulate Markov chain
#'
#' @description
#' This function simulates a Markov chain.
#'
#' @param Gamma \[`matrix()`\]\cr
#' A transition probability matrix.
#'
#' @param T \[`integer(1)`\]\cr
#' The length of the Markov chain.
#'
#' @param delta \[`numeric()`\]\cr
#' A probability vector, the initial distribution.
#'
#' @return
#' A \code{numeric} vector of length \code{T} with states.
#'
#' @keywords simulation
#' @family simulation helpers
#' @export
#'
#' @examples
#' Gamma <- matrix(c(0.8, 0.2, 0.3, 0.7), byrow = TRUE, nrow = 2)
#' delta <- c(0.6, 0.4)
#' simulate_markov_chain(Gamma = Gamma, T = 20, delta = delta)

simulate_markov_chain <- function(Gamma, T, delta) {
  input_check_response(
    check = check_transition_probability_matrix(Gamma),
    var_name = "Gamma"
  )
  input_check_response(
    check = checkmate::check_int(T, lower = 1),
    var_name = "T"
  )
  input_check_response(
    check = check_probability_vector(delta, len = nrow(Gamma)),
    var_name = "delta"
  )
  N <- length(delta)
  markov_chain <- numeric(T)
  markov_chain[1] <- sample(1:N, 1, prob = delta)
  for (t in 2:T) {
    markov_chain[t] <- sample(1:N, 1, prob = Gamma[markov_chain[t - 1], ])
  }
  markov_chain
}

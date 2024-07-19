#' Sample transition probability matrices
#'
#' @description
#' This function returns a random, squared matrix of dimension \code{dim}
#' that fulfills the properties of a transition probability matrix.
#'
#' @inheritParams sample_correlation_matrix
#'
#' @param state_persistent \[`logical(1)`\]\cr
#' Put more probability on the diagonal?
#'
#' @return
#' A transition probability \code{matrix}.
#'
#' @keywords simulation
#' @family matrix helpers
#' @export
#'
#' @examples
#' sample_transition_probability_matrix(dim = 3)

sample_transition_probability_matrix <- function(dim, state_persistent = TRUE) {
  checkmate::assert_int(dim, lower = 1)
  checkmate::assert_flag(state_persistent)
  Gamma <- matrix(stats::runif(dim^2), dim, dim)
  if (state_persistent) Gamma <- Gamma + diag(dim)
  Gamma / rowSums(Gamma)
}

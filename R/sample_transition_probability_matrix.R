#' Sample transition probability matrices
#'
#' @description
#' This function returns a random, squared matrix of dimension \code{dim}
#' that fulfills the properties of a transition probability matrix.
#'
#' @param dim
#' An \code{integer}, the matrix dimension.
#' @param state_persistent
#' Set to \code{TRUE} (default) to put more probability on the diagonal.
#'
#' @return
#' A transition probability \code{matrix}.
#'
#' @importFrom stats runif
#'
#' @examples
#' sample_transition_probability_matrix(dim = 3)
#'
#' @export

sample_transition_probability_matrix <- function(dim, state_persistent = TRUE) {
  checkmate::assert_int(dim, lower = 1)
  checkmate::assert_flag(state_persistent)
  Gamma <- matrix(stats::runif(dim^2), dim, dim)
  if (state_persistent) Gamma <- Gamma + diag(dim)
  Gamma / rowSums(Gamma)
}

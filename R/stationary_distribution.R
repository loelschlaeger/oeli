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

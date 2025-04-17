#' Stationary distribution
#'
#' @description
#' This function computes the stationary distribution corresponding to a
#' transition probability matrix.
#'
#' @param tpm \[`matrix()`\]\cr
#' A transition probability matrix.
#'
#' @param soft_fail \[`logical(1)`\]\cr
#' Return the discrete uniform distribution if the computation of the stationary
#' distribution fails for some reason? Else, throw an error.
#'
#' @return
#' A \code{numeric} vector.
#'
#' @keywords transformation
#' @family matrix helpers
#' @export
#'
#' @examples
#' tpm <- matrix(0.05, nrow = 3, ncol = 3)
#' diag(tpm) <- 0.9
#' stationary_distribution(tpm)

stationary_distribution <- function(tpm, soft_fail = FALSE) {
  input_check_response(
    check = check_transition_probability_matrix(tpm),
    var_name = "tpm"
  )
  input_check_response(
    check = checkmate::check_flag(soft_fail),
    var_name = "soft_fail"
  )
  dim <- nrow(tpm)
  stat_distr <- try(solve(t(diag(dim) - tpm + 1), rep(1, dim)), silent = TRUE)
  if (inherits(stat_distr, "try-error")) {
    if (soft_fail) {
      stat_distr <- rep(1, dim) / dim
    } else {
      stop("singular system")
    }
  }
  return(stat_distr)
}

#' Generate vector subsets
#'
#' @description
#' This function generates subsets of a vector.
#'
#' @param v \[atomic()`\]\cr
#' A vector of elements.
#'
#' @param n \[integer(1)`\]\cr
#' The requested subset sizes.
#'
#' @return
#' A \code{list}, each element is a subset of \code{v}.
#'
#' @export
#' @keywords indexing
#' @family vector helpers
#'
#' @examples
#' v <- 1:3
#' subsets(v)
#' subsets(v, c(1, 3)) # only subsets of length 1 or 3
#' subsets(integer())  # trivial case works

subsets <- function(v, n = seq_along(v)) {
  input_check_response(
    check = checkmate::check_atomic_vector(v),
    var_name = "v"
  )
  input_check_response(
    check = checkmate::check_integerish(
      n, lower = 1, upper = length(v), any.missing = FALSE, unique = TRUE
    ),
    var_name = "n"
  )
  if (length(v) == 0 || length(n) == 0) {
    return(list())
  }
  unlist(lapply(n, function(i) {
    utils::combn(v, i, simplify = FALSE)
  }), recursive = FALSE)
}

#' Generate vector subsets
#'
#' @description
#' This function generates subsets of a vector.
#'
#' @param v
#' A \code{vector} (\code{numeric} or \code{character}).
#'
#' @param n
#' An \code{integer} vector with the required subset sizes.
#' By default, \code{n = seq_along(v)}, i.e., all subsets of \code{v} are
#' generated.
#'
#' @return
#' A \code{list}, each element is a subset of \code{v}.
#'
#' @export
#'
#' @examples
#' v <- 1:3
#' subsets(v)
#' subsets(v, c(1, 3)) # only subsets of length 1 or 3
#' subsets(integer())  # trivial case works

subsets <- function(v, n = seq_along(v)) {
  checkmate::assert_atomic_vector(v)
  checkmate::assert_integerish(
    n, lower = 1, upper = length(v), any.missing = FALSE, unique = TRUE
  )
  if (length(v) == 0 || length(n) == 0) {
    return(list())
  }
  unlist(lapply(n, function(i) {
    utils::combn(v, i, simplify = FALSE)
  }), recursive = FALSE)
}

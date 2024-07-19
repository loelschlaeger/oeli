#' Insert entry in vector
#'
#' @description
#' This function inserts a value into a vector.
#'
#' @param v \[`atomic()`\]\cr
#' A \code{vector}.
#'
#' @param x \[`atomic(1)`\]\cr
#' The entry to be added.
#'
#' @param p \[`integer())`\]\cr
#' The position(s) where to add the value, one or more of:
#' - \code{p = 0} appends the value left
#' - \code{p = length(v)} appends the value right
#' - \code{p = n} inserts the value between the \code{n}-th and
#'   \code{(n + 1)}-th entry of \code{v}.
#'
#' @return
#' A \code{vector}.
#'
#' @export
#' @keywords transformation
#' @family vector helpers
#'
#' @examples
#' v <- 1:3
#' x <- 0
#' insert_vector_entry(v, x, 0)
#' insert_vector_entry(v, x, 1)
#' insert_vector_entry(v, x, 2)
#' insert_vector_entry(v, x, 3)
#'
#' ### also multiple positions
#' insert_vector_entry(v, x, 0:3)
#'
#' ### also trivial case
#' insert_vector_entry(integer(), integer(), integer())

insert_vector_entry <- function(v, x, p) {
  checkmate::assert_atomic_vector(v)
  n <- length(v)
  checkmate::assert_atomic_vector(x, max.len = 1)
  checkmate::assert_integerish(
    p, lower = 0, upper = n, unique = TRUE, max.len = n + 1
  )
  if (length(p) == 0) {
    return(v)
  }
  c(v, rep(x, length(p)))[order(c(seq_along(v), p + 0.5))]
}

#' Best-possible match of two numeric vectors
#'
#' @description
#' This function matches the indices of two numeric vectors as good as possible
#' (that means with the smallest possible sum of deviations).
#'
#' @param x,y \[`numeric()`\]\cr
#' Two vectors of the same length.
#'
#' @return
#' An \code{integer} vector of length \code{length(x)} with the positions of
#' \code{y} in \code{x}.
#'
#' @export
#' @keywords indexing
#' @family vector helpers
#'
#' @examples
#' x <- c(-1, 0, 1)
#' y <- c(0.1, 1.5, -1.2)
#' match_numerics(x, y)

match_numerics <- function(x, y) {
  checkmate::assert_atomic_vector(x)
  checkmate::assert_numeric(x)
  checkmate::assert_atomic_vector(y, len = length(x))
  checkmate::assert_numeric(y)
  matches <- numeric(length(x))
  distances <- unique(sort(stats::dist(c(x, y)))) + sqrt(.Machine$double.eps)
  for (d in distances) {
    if (any(c(!is.na(x), !is.na(y)))) {
      for (i_x in 1:length(x)) {
        for (i_y in 1:length(y)) {
          if (!is.na(x[i_x]) && !is.na(y[i_y])) {
            if (isTRUE(all.equal(x[i_x], y[i_y], d))) {
              matches[i_y] <- i_x
              x[i_x] <- NA_integer_
              y[i_y] <- NA_integer_
            }
          }
        }
      }
    }
  }
  return(matches)
}

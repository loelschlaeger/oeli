#' Best-possible match of two numeric vectors
#'
#' @description
#' This function matches the indices of two numeric vectors as good as possible
#' (that means with the smallest possible sum of absolute deviations).
#'
#' @param x,y \[`numeric()`\]\cr
#' Two vectors of the same length without missing values.
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
  input_check_response(
    check = check_numeric_vector(x, any.missing = FALSE),
    var_name = "x"
  )
  input_check_response(
    check = check_numeric_vector(y, any.missing = FALSE, len = length(x)),
    var_name = "y"
  )
  order(x)[rank(y, ties.method = "first")]
}

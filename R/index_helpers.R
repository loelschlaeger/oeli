#' Build permutations
#'
#' @description
#' This function creates all permutations of a given \code{vector}.
#'
#' @references
#' Modified version of <https://stackoverflow.com/a/20199902/15157768>.
#'
#' @param x
#' Any \code{vector}.
#'
#' @return
#' A \code{list} of all permutations of \code{x}.
#'
#' @examples
#' permutations(1:3)
#' permutations(LETTERS[1:3])
#'
#' @export

permutations <- function(x) {
  checkmate::assert_atomic_vector(x, min.len = 1)
  permutation_index <- function(n) {
    if (n == 1) {
      return(matrix(1))
    } else {
      sp <- permutation_index(n - 1)
      p <- nrow(sp)
      A <- matrix(nrow = n * p, ncol = n)
      for (i in 1:n) {
        A[(i - 1) * p + 1:p, ] <- cbind(i, sp + (sp >= i))
      }
      return(A)
    }
  }
  p <- permutation_index(length(x))
  apply(p, 1, function(p) x[p], simplify = FALSE)
}

#' Best-possible match of two numeric vectors
#'
#' @description
#' This function matches the indices of two numeric vectors as good as possible
#' (that means with the smallest possible sum of deviations).
#'
#' @param x
#' A \code{numeric} vector.
#' @param y
#' Another \code{numeric} vector of the same length as \code{x}.
#'
#' @return
#' An \code{integer} vector of length \code{length(x)} with the positions of
#' \code{y} in \code{x}.
#'
#' @importFrom stats dist
#'
#' @examples
#' x <- c(-1, 0, 1)
#' y <- c(0.1, 1.5, -1.2)
#' match_numerics(x, y)
#'
#' @export

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

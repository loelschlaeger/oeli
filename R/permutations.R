#' Build permutations
#'
#' @description
#' This function creates all permutations of a given \code{vector}.
#'
#' @references
#' Modified version of <https://stackoverflow.com/a/20199902/15157768>.
#'
#' @param x \[`atomic()`\]\cr
#' Any \code{vector}.
#'
#' @return
#' A \code{list} of all permutations of \code{x}.
#'
#' @export
#' @keywords indexing
#' @family vector helpers
#'
#' @examples
#' permutations(1:3)
#' permutations(LETTERS[1:3])

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

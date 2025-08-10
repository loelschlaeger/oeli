#' Generate equidistant vectors in Euclidean space
#'
#' @description
#' This function constructs the coordinates of vertices of a regular simplex
#' in \eqn{\mathbb{R}^{\code{dim}}} and returns the first \code{n} of them,
#'
#' * scaled so that the pairwise Euclidean distance between any two vertices
#'   equals \code{dist},
#'
#' * and centered so their centroid is at \code{center}.
#'
#' @param dim \[`integer(1)`\]\cr
#' The dimension.
#'
#' @param n \[`integer(1)`\]\cr
#' The number of vertices to return. Cannot be larger than \code{dim + 1}.
#'
#' @param dist \[`numeric(1)`\]\cr
#' Desired pairwise Euclidean distance between any two vertices.
#'
#' @param center \[`numeric(dim)`\]\cr
#' Desired center.
#'
#' @return
#' A matrix, where each column is a vertex of the simplex.
#'
#' @keywords simulation
#' @family vector helpers
#' @export
#'
#' @examples
#' dim <- n <- 3
#' (dist <- runif(1))
#' (center <- rnorm(dim))
#' (V <- equidistant_vectors(dim = dim, n = n, dist = dist, center = center))
#' rowMeans(V)
#' dist(t(V))

equidistant_vectors <- function(
    dim, n = dim + 1, dist = 1, center = rep(0, dim)
  ) {
  input_check_response(
    check = checkmate::check_count(dim, positive = TRUE),
    var_name = "dim"
  )
  input_check_response(
    check = checkmate::check_int(n, lower = 1, upper = dim + 1),
    var_name = "n"
  )
  input_check_response(
    check = checkmate::check_number(dist, lower = 0, finite = TRUE),
    var_name = "dist"
  )
  input_check_response(
    check = check_numeric_vector(
      center, finite = TRUE, any.missing = FALSE, len = dim
    ),
    var_name = "center"
  )
  b <- matrix(c(1, -1), ncol = 2)
  for (k in seq_len(dim)[-1]) {
    b <- rbind(
      cbind(1, matrix(-1 / k, ncol = k)),
      cbind(0, sqrt(1 - 1 / k^2) * b)
    )
  }
  b <- b * dist / mean(as.vector(dist(t(b))))
  b_sub <- b[, seq_len(n), drop = FALSE]
  b_sub <- sweep(b_sub, 1, rowMeans(b_sub))
  b_sub <- sweep(b_sub, 1, center, FUN = "+")
  return(b_sub)
}

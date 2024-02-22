#' Get \code{matrix} indices
#'
#' @description
#' This function returns \code{matrix} indices as \code{character}.
#'
#' @param x
#' A \code{matrix}.
#' @param prefix
#' A \code{character} as prefix for the indices.
#' @param exclude_diagonal
#' Either \code{TRUE} to exclude indices where row equals column, or
#' \code{FALSE} to include those.
#'
#' @return
#' A \code{character} \code{vector}.
#'
#' @examples
#' M <- diag(3)
#' matrix_indices(M)
#' matrix_indices(M, "M_")
#' matrix_indices(M, "M_", TRUE)
#'
#' @export

matrix_indices <- function(x, prefix = "", exclude_diagonal = FALSE) {
  checkmate::assert_matrix(x)
  checkmate::assert_string(prefix)
  checkmate::assert_flag(exclude_diagonal)
  rows <- nrow(x)
  cols <- ncol(x)
  indices <- expand.grid(seq_len(rows), seq_len(cols))
  if (exclude_diagonal) {
    indices <- indices[apply(indices, 1, function(x) length(unique(x)) != 1), ]
  }
  indices <- apply(indices, 1, paste, collapse = "")
  paste0(prefix, indices)
}

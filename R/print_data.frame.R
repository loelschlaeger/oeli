#' Print (abbreviated) \code{data.frame}
#'
#' @description
#' This function prints a (possibly abbreviated) \code{data.frame}.
#'
#' @param x \[`data.frame`\]\cr
#' A \code{data.frame}.
#'
#' @param rows \[`integer(1)` | `NULL` \]\cr
#' The number of rows to be printed. Printing is abbreviated in the middle.
#'
#' Can be `NULL` to print everything.
#'
#' @param row.names \[`logical(1)`\]\cr
#' Print row names?
#'
#' @inheritParams round_data.frame
#'
#' @return
#' Invisibly returns \code{x}.
#'
#' @export
#' @keywords packaging
#' @family package helpers
#'
#' @examples
#' x <- data.frame("a" = rnorm(20), "b" = rnorm(20))
#' print_data.frame(x, rows = 7)
#' print_data.frame(x, rows = 7, digits = 1)
#' print_data.frame(x, rows = 7, digits = 1, row.names = FALSE)

print_data.frame <- function(
    x, rows = NULL, digits = NULL, row.names = TRUE
  ) {

  ### input checks
  input_check_response(
    check = checkmate::check_data_frame(x),
    var_name = "x"
  )
  input_check_response(
    check = checkmate::check_int(rows, lower = 2, null.ok = TRUE),
    var_name = "rows"
  )
  input_check_response(
    check = checkmate::check_int(digits, lower = 0, null.ok = TRUE),
    var_name = "digits"
  )
  input_check_response(
    check = checkmate::check_flag(row.names),
    var_name = "row.names"
  )

  ### printing
  r <- nrow(x)
  c <- ncol(x)
  if (c == 0L) {
    cli::cli_text("< data.frame with 0 columns and {r} row{?s} >\n")
  } else if (r == 0L) {
    cli::cli_text("< data.frame with {c} column{?s} and 0 rows >\n")
  } else {
    if (!is.null(digits)) {
      x <- round_data.frame(x, digits = digits)
    }
    m <- as.matrix(format.data.frame(x, na.encode = FALSE))
    if (isFALSE(row.names)) {
      dimnames(m)[[1L]] <- rep.int("", r)
    }
    if (is.null(rows) || rows >= r) {
      print(m, quote = FALSE)
    } else {
      ur <- ceiling(rows / 2)
      br <- floor(rows / 2)
      hr <- r - (ur + br)
      rowlab <- sprintf(paste0("%-", nchar(r), "s"), seq_len(r))
      prmatrix(
        utils::head(m, ur), quote = FALSE,
        rowlab = if (isFALSE(row.names)) rep("", ur) else rowlab[seq_len(ur)]
      )
      cat("\n"); cli::cli_text("< {hr} row{?s} hidden >\n")
      prmatrix(utils::tail(m, br), quote = FALSE, collab = rep("", c))
    }
  }
  invisible(x)
}




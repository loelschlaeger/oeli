#' Print (abbreviated) \code{data.frame}
#'
#' @description
#' This function prints a (possibly abbreviated) \code{data.frame}.
#'
#' @param x \[`data.frame`\]\cr
#' A \code{data.frame}.
#'
#' @param rows,cols \[`integer(1)` | `NULL` \]\cr
#' The number of rows or columns to be printed, greater or equal `2`.
#'
#' Printing is abbreviated in the middle.
#'
#' Can be `NULL` to print everything.
#'
#' @param row.names,col.names \[`logical(1)`\]\cr
#' Print row names or column names?
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
#' x <- data.frame(1:10, LETTERS[1:10], stats::rnorm(10))
#' print_data.frame(x, rows = 7)
#' print_data.frame(x, rows = 7, cols = 2)
#' print_data.frame(x, rows = 7, cols = 2, digits = 1)
#' print_data.frame(x, rows = 7, cols = 2, digits = 1, row.names = FALSE)
#' print_data.frame(x, rows = 7, cols = 2, digits = 1, col.names = FALSE)

print_data.frame <- function(
    x, rows = NULL, cols = NULL, digits = NULL,
    row.names = TRUE, col.names = TRUE
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
    check = checkmate::check_int(cols, lower = 2, null.ok = TRUE),
    var_name = "cols"
  )
  input_check_response(
    check = checkmate::check_int(digits, lower = 0, null.ok = TRUE),
    var_name = "digits"
  )
  input_check_response(
    check = checkmate::check_flag(row.names),
    var_name = "row.names"
  )
  input_check_response(
    check = checkmate::check_flag(col.names),
    var_name = "col.names"
  )

  ### printing
  r <- nrow(x)
  c <- ncol(x)
  if (c == 0L) {
    cli::cli_text("< data.frame with 0 columns and {r} row{?s} >\n")
  } else if (r == 0L) {
    cli::cli_text("< data.frame with {c} column{?s} and 0 rows >\n")
  } else {
    if (!col.names) {
      colnames(x) <- NULL
    }
    if (!is.null(digits)) {
      x <- round_data.frame(x, digits = digits)
    }
    m <- as.matrix(format.data.frame(x, na.encode = FALSE))
    if (!is.null(cols) && cols < c) {
      lc <- ceiling(cols / 2)
      rc <- floor(cols / 2)
      hc <- c - (lc + rc)
      hc_colname <- paste0(
        "<", hc, " col", ifelse(hc > 1, "s", ""), " hidden>"
      )
      hc_value <- strrep(" ", nchar(hc_colname))
      hc_value_bar <- floor(nchar(hc_value) / 2)
      substr(hc_value, hc_value_bar, hc_value_bar) <- "-"
      m <- cbind(
        m[, seq_len(lc), drop = FALSE],
        hc_value,
        m[, utils::tail(seq_len(c), rc), drop = FALSE]
      )
      colnames(m)[lc + 1L] <- hc_colname
    }
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
      cat("\n"); cli::cli_text("<{hr} row{?s} hidden>\n")
      prmatrix(
        utils::tail(m, br), quote = FALSE,
        collab = sapply(sapply(colnames(m), nchar), function(x) strrep(" ", x))
      )
    }
  }
  invisible(x)
}




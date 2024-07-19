#' Print (abbreviated) \code{matrix}
#'
#' @description
#' This function prints a (possibly abbreviated) \code{matrix}.
#'
#' @references
#' This function is a modified version of \code{ramify::pprint()}.
#'
#' @param x \[`atomic()` | `matrix`\]\cr
#' The object to be printed.
#'
#' @param rowdots \[`integer(1)`\]\cr
#' The row number which is replaced by \code{...}.
#'
#' @param coldots \[`integer(1)`\]\cr
#' The column number which is replaced by \code{...}.
#'
#' @param digits \[`integer(1)`\]\cr
#' The number of printed decimal places if input \code{x} is \code{numeric}.
#'
#' @param label \[`character(1)`\]\cr
#' A label for \code{x}. Only printed if \code{simplify = FALSE}.
#'
#' @param simplify \[`logical(1)`\]\cr
#' Simplify the output?
#'
#' @param details \[`logical(1)`\]\cr
#' Print the type and dimension of \code{x}?
#'
#' @return
#' Invisibly returns \code{x}.
#'
#' @export
#' @keywords packaging
#' @family package helpers
#'
#' @examples
#' print_matrix(x = 1, label = "single numeric")
#' print_matrix(x = LETTERS[1:26], label = "letters")
#' print_matrix(x = 1:3, coldots = 2)
#' print_matrix(x = matrix(rnorm(99), ncol = 1), label = "single column matrix")
#' print_matrix(x = matrix(1:100, nrow = 1), label = "single row matrix")
#' print_matrix(x = matrix(LETTERS[1:24], ncol = 6), label = "big matrix")
#' print_matrix(x = diag(5), coldots = 2, rowdots = 2, simplify = TRUE)

print_matrix <- function(
    x, rowdots = 4, coldots = 4, digits = 2, label = NULL, simplify = FALSE,
    details = !simplify
  ) {
  checkmate::assert_int(rowdots, lower = 1)
  checkmate::assert_int(coldots, lower = 1)
  checkmate::assert_int(digits)
  checkmate::assert_flag(details)
  checkmate::assert_flag(simplify)
  if (!is.null(label)) {
    label <- as.character(label)
    stopifnot(length(label) == 1)
  }
  add_dots <- function(x, pos) {
    if (length(x) > pos) c(x[seq_len(pos - 1)], "...", x[length(x)]) else x
  }
  if (is.numeric(x)) x <- round(x, digits)
  if (!is.null(label)) cat(label, ": ")
  if (length(x) == 1) {
    cat(x)
  } else if (!is.matrix(x)) {
    if (details) {
      cat(typeof(x), "vector of length", length(x), "\n")
    }
    cat(noquote(add_dots(x, coldots)))
  } else {
    row_labs <- if (is.null(rownames(x))) {
      paste0("[", seq_len(nrow(x)), ",]")
    } else {
      rownames(x)
    }
    col_labs <- if (is.null(colnames(x))) {
      paste0("[,", seq_len(ncol(x)), "]")
    } else {
      colnames(x)
    }
    coldots <- max(1, min(ncol(x) - 1, coldots))
    rowdots <- max(1, min(nrow(x) - 1, rowdots))
    if (coldots == 1 && rowdots == 1) {
      if (nrow(x) <= 2 && ncol(x) <= 2) {
        res <- x
      } else {
        res <- if (nrow(x) == 1) {
          matrix(c("...", x[1, ncol(x)]), 1, 2)
        } else if (ncol(x) == 1) {
          matrix(c("...", x[nrow(x), 1]), 2, 1)
        } else {
          matrix(c("...", "...", "...", x[nrow(x), ncol(x)]), 2, 2)
        }
        row_labs <- add_dots(row_labs, 1)
        col_labs <- add_dots(col_labs, 1)
      }
    } else {
      x2 <- if (nrow(x) == 1) {
        cbind(x[1, 1:coldots, drop = FALSE], x[1, ncol(x), drop = FALSE])
      } else if (ncol(x) == 1) {
        rbind(x[1:rowdots, 1, drop = FALSE], x[nrow(x), 1, drop = FALSE])
      } else {
        rbind(
          cbind(
            x[1:rowdots, 1:coldots, drop = FALSE],
            x[1:rowdots, ncol(x), drop = FALSE]
          ),
          cbind(
            x[nrow(x), 1:coldots, drop = FALSE],
            x[nrow(x), ncol(x), drop = FALSE]
          )
        )
      }
      charx <- as.character(x2)
      dim(charx) <- dim(x2)
      if (nrow(x) <= rowdots + 1 && ncol(x) <= coldots + 1) {
        res <- charx
      } else if (nrow(x) > rowdots + 1 && ncol(x) <= coldots + 1) {
        res <- rbind(
          as.matrix(charx[seq_len(rowdots - 1), ]),
          rep("...", ncol(charx)),
          charx[nrow(charx), ]
        )
        row_labs <- add_dots(row_labs, pos = rowdots)
      } else if (nrow(x) <= rowdots + 1 && ncol(x) > coldots + 1) {
        res <- t(apply(charx, 1, add_dots, pos = coldots))
        col_labs <- add_dots(col_labs, pos = coldots)
      } else if (nrow(x) > rowdots + 1 && ncol(x) > coldots + 1) {
        smallx <- t(apply(charx[seq_len(rowdots - 1), , drop = FALSE], 1,
                          add_dots,
                          pos = coldots
        ))
        res <- rbind(
          smallx,
          rep("...", ncol(smallx)),
          add_dots(charx[nrow(charx), ], pos = coldots)
        )
        row_labs <- add_dots(row_labs, pos = rowdots)
        col_labs <- add_dots(col_labs, pos = coldots)
      }
    }
    if (details) {
      cat(paste(dim(x), collapse = " x "), "matrix of", paste0(typeof(x), "s"), "\n")
    }
    if (simplify) {
      cat(paste("[", paste(apply(res, 1, paste, collapse = " "),
                           collapse = "; "
      ), "]"))
    } else {
      prmatrix(res,
               rowlab = row_labs, collab = col_labs, quote = FALSE,
               right = TRUE
      )
    }
  }
  return(invisible(x))
}

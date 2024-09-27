#' Round \code{numeric} columns of a \code{data.frame}
#'
#' @description
#' This function rounds (only) the \code{numeric} columns of a
#' \code{data.frame}.
#'
#' @param df \[`data.frame`\]\cr
#' A \code{data.frame}.
#'
#' @param digits \[`integer(1)` | `NULL` \]\cr
#' The number of decimal places to be used.
#'
#' Negative values are allowed, resulting in rounding to a power of ten.
#'
#' Can be `NULL` to not round.
#'
#' @return
#' A \code{data.frame}.
#'
#' @keywords transformation
#' @family data.frame helpers
#' @export
#'
#' @examples
#' df <- data.frame("label" = c("A", "B"), "number" = rnorm(10))
#' round_data.frame(df, digits = 1)

round_data.frame <- function(df, digits = 0) {

  ### input checks
  input_check_response(
    check = checkmate::check_data_frame(df),
    var_name = "df"
  )
  input_check_response(
    check = checkmate::check_int(digits, null.ok = TRUE),
    var_name = "digits"
  )

  ### rounding
  if (!is.null(digits)) {
    df[] <- lapply(df, function(x) if (is.numeric(x)) round(x, digits) else x)
  }
  return(df)
}


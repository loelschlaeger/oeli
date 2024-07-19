#' Handling of an unexpected error
#'
#' @description
#' This function reacts to an unexpected error by throwing an error and linking
#' to an issue site with the request to submit an issue.
#'
#' @param msg \[`character(1)`\]\cr
#' An error message.
#'
#' @param issue_link \[`character(1)`\]\cr
#' The URL to an issues site.
#'
#' @return
#' No return value, but it throws an error.
#'
#' @keywords packaging
#' @family package helpers
#' @export

unexpected_error <- function(
    msg = "Ups, an unexpected error occured.",
    issue_link = "https://github.com/loelschlaeger/oeli/issues") {
  checkmate::assert_string(msg, min.chars = 1)
  checkmate::assert_string(
    issue_link,
    pattern = "^https://github.com/[[:alpha:]]*/[[:alpha:]]*/issues$"
  )
  cli::cli_abort(
    c(
      msg,
      "i" = paste(
        "Please submit an issue here:",
        cli::style_hyperlink(issue_link, issue_link)
      )
    ),
    call = NULL
  )
}

#' Using development packages when working with \code{{renv}}
#'
#' @description
#' This function creates a file that loads development packages so that
#' \code{{renv}} can detect and write them to the lockfile.
#'
#' @param packages \[`character()`\]\cr
#' Package names.
#'
#' @param file_name \[`character(1)`\]\cr
#' The name for the \code{.R} file.
#'
#' @return
#' No return value, but it writes a file to the project root and adds an
#' entry to the \code{.Rbuildignore} file.
#'
#' @export
#' @keywords packaging
#' @family package helpers

renv_development_packages <- function(
    packages = c("covr", "devtools", "DT", "markdown", "R.utils", "yaml"),
    file_name = "development_packages"
  ) {
  is_package <- tryCatch(
    rprojroot::find_package_root_file(),
    error = function(e) FALSE
  )
  if (isFALSE(is_package)) {
    cli::cli_abort(
      c("Please check that you are inside a package directory."),
      call = NULL
    )
  } else {
    checkmate::assert_names(packages)
    checkmate::assert_character(file_name)
    file_path <- rprojroot::find_package_root_file(paste0(file_name, ".R"))
    if (file.exists(file_path)) {
      overwrite <- user_confirm(
        question = paste(
          "The file", file_path, "already exists, may I overwrite?"
        ),
        default = FALSE
      )
      if (!overwrite) {
        cli_inform(
          "Okay, nothing is changed."
        )
        return(invisible(NULL))
      }
    }
    file_connection <- file(file_path, open = "wt")
    for (package in packages) {
      writeLines(
        c(
          paste0("if (!require(\"", package, "\", quietly = TRUE)) {"),
          paste0("  renv::install(\"", package, "\", prompt = FALSE)"),
          "}"
        ),
        con = file_connection
      )
    }
  }
  close(file_connection)
  usethis::use_build_ignore(paste0(file_name, ".R"))
  return(invisible(NULL))
}

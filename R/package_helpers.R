#' Handling of an unexpected error
#'
#' @description
#' This function reacts to an unexpected error by throwing an error and linking
#' to a GitHub issues site with the request to submit an issue.
#'
#' @param msg
#' A \code{character}, an error message.
#' @param issue_link
#' A \code{character}, the URL to a GitHub issues site.
#'
#' @export
#'
#' @return
#' No return value, but it throws an error.

unexpected_error <- function(
    msg = "We are sorry, an unexpected error occured.",
    issue_link = "https://github.com/loelschlaeger/oeli/issues") {
  checkmate::assert_string(msg, min.chars = 1)
  checkmate::assert_string(
    issue_link,
    pattern = "^https://github.com/[[:alpha:]]*/[[:alpha:]]*/issues$"
  )
  cli::cli_abort(
    c(
      msg,
      "i" = paste("Please submit an issue here:", cli::style_hyperlink(issue_link, issue_link))
    ),
    call = NULL
  )
}

#' Using development packages when working with \code{{renv}}
#'
#' @description
#' This function creates a file that loads development packages so that
#' \code{{renv}} can detect and write them to the lockfile.
#'
#' @param packages
#' A \code{character} \code{vector} of package names.
#' @param file_name
#' A single \code{character}, the name for the \code{.R} file.
#'
#' @export
#'
#' @return
#' No return value, but it writes a file to the project root and adds an
#' entry to the \code{.Rbuildignore} file.

renv_development_packages <- function(
    packages = c("covr", "devtools", "DT", "markdown", "R.utils", "yaml"),
    file_name = "development_packages") {
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

#' General system level information
#'
#' @description
#' This function returns a \code{list} of general system level information.
#'
#' @export
#'
#' @return
#' A \code{list} with elements:
#' \itemize{
#'   \item \code{maschine}, the model name of the device
#'   \item \code{cores}, the number of cores
#'   \item \code{ram}, the size of the RAM
#'   \item \code{os}, the operating system
#'   \item \code{rversion}, the R version used
#' }
#'
#' @examples
#' system_information()

system_information <- function() {

  ### get information
  cpu <- try(benchmarkme::get_cpu(), silent = TRUE)
  if (inherits(cpu, "try-error") || !checkmate::test_list(cpu)) {
    cpu <- list()
  }
  machine <- cpu$model_name
  if (!checkmate::test_string(machine, na.ok = FALSE, null.ok = FALSE)) {
    machine <- NA_character_
  }
  cores <- cpu$no_of_cores
  if (!checkmate::test_int(cores, lower = 0, na.ok = FALSE, null.ok = FALSE)) {
    cores <- NA_integer_
  }
  ram <- try(benchmarkme::get_ram(), silent = TRUE)
  if (!checkmate::test_class(ram, "ram")) {
    ram <- NA
  }
  os <- .Platform$OS.type
  if (!checkmate::test_string(os, na.ok = FALSE, null.ok = FALSE)) {
    os <- NA_character_
  }
  rversion <- try(getRversion(), silent = TRUE)
  if (!inherits(rversion, "R_system_version")) {
    rversion <- NA
  }

  ### build and return output
  list(
    "machine" = machine,
    "cores" = cores,
    "ram" = ram,
    "os" = os,
    "rversion" = rversion
  )
}



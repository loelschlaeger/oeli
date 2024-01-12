#' Creating and initializing a basic package sticker
#'
#' @description
#' This function sets up a basic package sticker. The sticker has a white
#' background and the package name (with or without curly brackets) in the
#' center. The font size for the package name is scaled such that it fits inside
#' the sticker. Type \code{?oeli} to see an example.
#'
#' @param package_name
#' A \code{character}, the package name.
#' @param brackets
#' Either \code{TRUE} to have curly brackets around the package name (default)
#' or \code{FALSE} if not.
#'
#' @export
#'
#' @return
#' No return value, but it runs \code{\link[usethis]{use_logo}} in the end.

basic_package_sticker <- function(package_name, brackets = TRUE) {
  ### input checks
  checkmate::assert_string(package_name)
  checkmate::assert_flag(brackets)

  ### define font
  sysfonts::font_add_google("Martel", "my_font")
  showtext::showtext_auto()

  ### define path
  filename <- tempfile(
    pattern = paste("sticker", package_name, sep = "_"),
    fileext = ".png"
  )

  ### optionally add brackets to package name
  if (brackets) {
    package_name <- paste0("{", package_name, "}")
  }

  ### determine font size for package name
  chars <- nchar(package_name)
  p_size <- 56 * exp(-0.09 * chars)

  ### build sticker
  sticker_file <- hexSticker::sticker(

    ### image
    subplot = ggplot2::ggplot() +
      ggplot2::theme_void(),
    s_x = 1,
    s_y = 1,
    s_width = 2,
    s_height = 2,

    ### package name
    package = package_name,
    p_x = 1,
    p_y = 1,
    p_color = "black",
    p_family = "my_font",
    p_fontface = "plain",
    p_size = p_size,

    ### sticker
    h_size = 1.2,
    h_fill = "white",
    h_color = "black",
    spotlight = TRUE,
    l_x = 0.9,
    l_y = 1.4,
    l_width = 2,
    l_height = 1,
    l_alpha = 0.8,
    white_around_sticker = FALSE,

    ### URL
    url = "",
    u_x = 1,
    u_y = 0.1,
    u_color = "black",
    u_family = "my_font",
    u_size = 7,
    u_angle = 30,

    ### save file
    filename = filename,
    asp = 1,
    dpi = 300
  )
  sticker_file

  ### message
  message("path:", filename)

  ### set sticker
  usethis::use_logo(img = filename)
}

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

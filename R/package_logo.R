#' Creating a basic logo for an R package
#'
#' @description
#' This function creates a basic R package logo. The logo has a white
#' background and the package name (with or without curly brackets) in the
#' center. The font size for the package name is scaled such that it fits inside
#' the logo. Type \code{?oeli} to see an example.
#'
#' @references
#' - This function builds upon \code{\link[hexSticker]{sticker}}.
#' - Use \code{\link[usethis]{use_logo}} to set up the logo for a package.
#'
#' @param package_name \[`character(1)`\]\cr
#' The package name.
#'
#' @param brackets \[`logical(1)`\]\cr
#' Curly brackets around the package name?
#'
#' @return
#' A \code{ggplot} object.
#'
#' @export
#' @keywords packaging
#' @family package helpers
#'
#' @examples
#' package_logo("my_package", brackets = TRUE)

package_logo <- function(package_name, brackets = TRUE) {

  ### input checks
  input_check_response(
    check = checkmate::check_string(package_name),
    var_name = "package_name"
  )
  input_check_response(
    check = checkmate::check_flag(brackets),
    var_name = "brackets"
  )

  ### define font
  sysfonts::font_add_google("Martel", "my_font")
  showtext::showtext_auto()

  ### define path
  filename <- tempfile(
    pattern = paste("logo", package_name, sep = "_"),
    fileext = ".png"
  )

  ### optionally add brackets to package name
  if (brackets) {
    package_name <- paste0("{", package_name, "}")
  }

  ### determine font size for package name
  chars <- nchar(package_name)
  p_size <- 56 * exp(-0.09 * chars)

  ### build logo
  suppressWarnings(
    hexSticker::sticker(

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
  )
}

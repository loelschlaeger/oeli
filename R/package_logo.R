#' Creating a basic logo for an R package
#'
#' @description
#' This function creates a basic R package logo. The logo has a white
#' background and the package name (with or without curly brackets) in the
#' center. The font size for the package name is scaled such that it fits inside
#' the logo. Type \code{?oeli} to see an example.
#'
#' The function optionally calls \code{\link[usethis]{use_logo}} if
#' \code{use_logo = TRUE} to set up the logo for a package.
#'
#' @param package_name
#' A \code{character}, the package name.
#'
#' @param brackets
#' Set to \code{TRUE} (default) to have curly brackets around the package name.
#'
#' @param use_logo
#' Set to \code{TRUE} to run \code{\link[usethis]{use_logo}} in the end.
#'
#' @export
#'
#' @return
#' A \code{ggplot} object.
#'
#' @examples
#' package_logo("my_package", brackets = TRUE, use_logo = FALSE)

package_logo <- function(
    package_name, brackets = TRUE, use_logo = FALSE
) {

  ### input checks
  checkmate::assert_string(package_name)
  checkmate::assert_flag(brackets)
  checkmate::assert_flag(use_logo)

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
    logo_file <- hexSticker::sticker(

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

  if (use_logo) {
    usethis::use_logo(img = filename)
  }

  ### print logo
  return(logo_file)
}

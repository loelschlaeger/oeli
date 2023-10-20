# These functions help when implementing an R package.

#' Creating and initializing a basic package sticker
#'
#' @description
#' This function sets up a basic package sticker.
#'
#' @param package_name
#' A \code{character}, the package name.
#'
#' @return
#' No return value, but runs \code{\link[usethis]{use_logo}} in the end.
#'
#' @export

basic_package_sticker <- function(package_name) {

  ### input checks
  checkmate::assert_string(package_name)

  ### define font
  sysfonts::font_add_google("Martel", "my_font")
  showtext::showtext_auto()

  ### define path
  filename <- tempfile(pattern = "sticker", fileext = ".png")

  ### build sticker
  sticker_file <- hexSticker::sticker(

    ### image
    subplot = ggplot2::ggplot() + ggplot2::theme_void(),
    s_x = 1,
    s_y = 1,
    s_width = 2,
    s_height = 2,

    ### package name
    package = paste0("{", package_name, "}"),
    p_x = 1,
    p_y = 1,
    p_color = "black",
    p_family = "my_font",
    p_fontface = "plain",
    p_size = 30,

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

  ### message
  message("path:", filename)

  ### set sticker
  usethis::use_logo(img = filename)
}

#' @keywords internal
#' @useDynLib oeli, .registration=TRUE
"_PACKAGE"

## usethis namespace: start
#' @importFrom benchmarkme get_cpu
#' @importFrom benchmarkme get_ram
#' @importFrom checkmate assert_character
#' @importFrom checkmate assert_count
#' @importFrom checkmate assert_flag
#' @importFrom checkmate assert_int
#' @importFrom checkmate assert_string
#' @importFrom checkmate check_matrix
#' @importFrom checkmate makeAssertionFunction
#' @importFrom cli cli_abort
#' @importFrom cli cli_inform
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 theme_void
#' @importFrom hexSticker sticker
#' @importFrom latex2exp TeX
#' @importFrom Rcpp evalCpp
#' @importFrom rprojroot find_package_root_file
#' @importFrom showtext showtext_auto
#' @importFrom SimMultiCorrData calc_theory
#' @importFrom SimMultiCorrData rcorrvar
#' @importFrom SimMultiCorrData valid_corr
#' @importFrom stats cor
#' @importFrom stats dist
#' @importFrom stats runif
#' @importFrom sysfonts font_add_google
#' @importFrom usethis use_logo
#' @importFrom utils combn
#' @importFrom utils head
#' @importFrom utils tail
## usethis namespace: end
NULL

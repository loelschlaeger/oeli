#' Plot a number sequence
#'
#' @description
#' This function plots a sequence of numbers.
#'
#' @param definition
#' A \code{function} with a single argument \code{n} that computes the sequence
#' element \eqn{a_n}.
#' @param start
#' An \code{integer}, the first sequence index.
#' @param end
#' An \code{integer}, the last sequence index.
#' @param limits
#' A \code{numeric} \code{vector} of limits to draw as horizontal lines.
#'
#' @return
#' A \code{ggplot2} object.
#'
#' @examples
#' definition <- function(n) ifelse(n %in% factorial(1:10), 1, 1/n)
#' plot_sequence(definition)
#'
#' @export

plot_sequence <- function(
    definition = function(n) 1/n, start = 1, end = 100, limits = numeric()
  ) {

  ### input checks
  checkmate::assert_function(definition, args = "n")
  checkmate::assert_int(start, lower = 0)
  checkmate::assert_int(end, lower = start)
  checkmate::assert_numeric(limits, finite = TRUE, any.missing = FALSE, unique = TRUE)

  ### compute sequence
  indices <- seq.int(from = start, to = end, by = 1)
  sequence <- sapply(indices, definition)

  ### visualize sequence
  data <- data.frame(indices = indices, sequence = sequence)
  if (length(indices) < 10) {
    x_limit <- ggplot2::scale_x_discrete()
    data$indices <- as.factor(data$indices)
  } else {
    x_limit <- ggplot2::scale_x_continuous()
  }
  plot <- ggplot2::ggplot(
    data = data,ggplot2::aes(x = indices, y = sequence, group = 1)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    x_limit +
    ggplot2::xlab(latex2exp::TeX("$n$")) +
    ggplot2::ylab(latex2exp::TeX("$a_n$")) +
    ggplot2::geom_hline(yintercept = limits)

  ### return plot
  return(plot)
}


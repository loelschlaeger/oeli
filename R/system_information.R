#' General system level information
#'
#' @description
#' This function returns a \code{list} of general system level information.
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
#' @keywords validation
#' @family package helpers
#' @export
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



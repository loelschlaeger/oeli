% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_missing.R
\name{check_missing}
\alias{check_missing}
\alias{assert_missing}
\alias{test_missing}
\title{Check missing formal argument}
\usage{
check_missing(x)

assert_missing(x)

test_missing(x)
}
\arguments{
\item{x}{[\code{any}]\cr
A formal argument.}
}
\value{
Depending on the function prefix:
\itemize{
\item If the check is successful, \code{assert_missing()} returns \code{x} invisibly,
whereas \code{check_missing()} and \code{test_missing()} return \code{TRUE}.
\item If the check is not successful, \code{assert_missing()} throws an error message,
\code{test_missing()} returns \code{FALSE}, and \code{check_missing()} returns a string with
the error message.
}
}
\description{
These functions check whether a value was specified as an argument to a
function.
}
\examples{
f <- function(x) {
  check_missing(x)
}
f()

g <- function(x) {
  test_missing(x)
}
g()

h <- function(x) {
  assert_missing(x)
}
\dontrun{
h()
}
}
\seealso{
Other package helpers: 
\code{\link{Dictionary}},
\code{\link{Storage}},
\code{\link{find_namespace_calls}()},
\code{\link{identical_structure}()},
\code{\link{input_check_response}()},
\code{\link{match_arg}()},
\code{\link{package_logo}()},
\code{\link{print_data.frame}()},
\code{\link{print_matrix}()},
\code{\link{system_information}()},
\code{\link{unexpected_error}()},
\code{\link{user_confirm}()}
}
\concept{package helpers}
\keyword{validation}

// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

//' Draw from univariate truncated normal
//'
//' @description
//' \code{rtnorm_cpp} draws from a one-sided truncated univariate normal
//' distribution.
//' \code{rttnorm_cpp} draws from a two-sided truncated univariate normal
//' distribution.
//'
//' @details
//' These function performs no input checks.
//'
//' @param mean
//' A \code{numeric}, the mean.
//' @param sd
//' A \code{numeric}, the standard deviation.
//' @param point
//' A \code{numeric}, the truncation point.
//' @param above
//' A \code{logical}, if \code{TRUE} truncation from above and if \code{FALSE}
//' truncation from below.
//' @param log
//' A \code{logical}, if \code{TRUE} the draw is taken from the log-normal
//' distribution.
//' By default, \code{log = FALSE}.
//'
//' @return
//' A \code{numeric}, the random draw.
//'
//' @keywords internal
//'
// [[Rcpp::export]]
double rtnorm_cpp(
    double mean, double sd, double point, bool above, bool log = false
  ) {
  double a,b;
  if (above) {
    a = 0.0;
    b = R::pnorm((point - mean) / sd, 0, 1, 1, 0);
  } else {
    a = R::pnorm((point - mean) / sd, 0, 1, 1, 0);
    b = 1.0;
  }
  double draw = mean + sd * R::qnorm(R::runif(0.0, 1.0) * (b-a) + a, 0, 1, 1, 0);
  if (log) draw = std::exp(draw);
  return draw;
}

//' @rdname rtnorm_cpp
//' @param lower
//' A \code{numeric}, the lower truncation point.
//' @param upper
//' A \code{numeric}, the upper truncation point.
//'
// [[Rcpp::export]]
double rttnorm_cpp(
    double mean, double sd, double lower, double upper, bool log = false
  ) {
  double a = R::pnorm((lower - mean) / sd, 0, 1, 1, 0);
  double b = R::pnorm((upper - mean) / sd, 0, 1, 1, 0);
  double draw = mean + sd * R::qnorm(R::runif(0.0, 1.0) * (b-a) + a, 0, 1, 1, 0);
  if (log) draw = std::exp(draw);
  return draw;
}

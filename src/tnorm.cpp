// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]

double dtnorm(
  double x, double mean, double sd, double point, bool above, bool log = false
) {
  double z = 0.0;
  double density = 0.0;
  if (above) {
    z = R::pnorm((point - mean) / sd, 0.0, 1.0, 0, 0);
    if (x > point) {
      return 0.0;
    } else {
      density = R::dnorm((x - mean) / sd, 0.0, 1.0, 0) / (sd * z);
    }
  } else {
    z = R::pnorm((point - mean) / sd, 0.0, 1.0, 1, 0);
    if (x < point) {
      return 0.0;
    } else {
      density = R::dnorm((x - mean) / sd, 0.0, 1.0, 0) / (sd * (1 - z));
    }
  }
  if (log) return std::log(density);
  return density;
}

// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]

double dttnorm(
  double x, double mean, double sd, double lower, double upper, bool log = false
) {
  if (x < lower || x > upper) return 0.0;
  double normal_density = R::dnorm(x, mean, sd, false);
  double a = R::pnorm(lower, mean, sd, true, false);
  double b = R::pnorm(upper, mean, sd, true, false);
  double density = normal_density / (b-a);
  if (log) return std::log(density);
  return density;
}

// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]

double rtnorm(
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

// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]

double rttnorm(
  double mean, double sd, double lower, double upper, bool log = false
) {
  double a = R::pnorm((lower - mean) / sd, 0, 1, 1, 0);
  double b = R::pnorm((upper - mean) / sd, 0, 1, 1, 0);
  double draw = mean + sd * R::qnorm(R::runif(0.0, 1.0) * (b-a) + a, 0, 1, 1, 0);
  if (log) draw = std::exp(draw);
  return draw;
}

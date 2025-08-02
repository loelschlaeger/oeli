// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>
#include "../inst/include/dirichlet.h"
#include "../inst/include/mvnorm.h"
#include "../inst/include/tnorm.h"
#include "../inst/include/wishart.h"

//' @rdname ddirichlet
//' @export
// [[Rcpp::export]]

double ddirichlet_cpp(
   arma::vec const& x, arma::vec const& concentration, bool log = false
) {
  return ddirichlet(x, concentration, log);
}

//' @rdname ddirichlet
//' @export
// [[Rcpp::export]]

arma::vec rdirichlet_cpp(
   arma::vec const& concentration
) {
  return rdirichlet(concentration);
}

//' @rdname dmvnorm
//' @export
// [[Rcpp::export]]

double dmvnorm_cpp(
   arma::vec const& x, arma::vec const& mean, arma::mat const& Sigma,
   bool log = false
) {
  return dmvnorm(x, mean, Sigma, log);
}

//' @rdname dmvnorm
//' @export
// [[Rcpp::export]]

arma::vec rmvnorm_cpp(
   arma::vec mean, arma::mat const& Sigma, bool log = false
) {
  return rmvnorm(mean, Sigma, log);
}

//' @rdname dtnorm
//' @export
// [[Rcpp::export]]

double dtnorm_cpp(
   double x, double mean, double sd, double point, bool above, bool log = false
) {
  return dtnorm(x, mean, sd, point, above, log);
}

//' @rdname dtnorm
//' @export
// [[Rcpp::export]]

double dttnorm_cpp(
   double x, double mean, double sd, double lower, double upper, bool log = false
) {
  return dttnorm(x, mean, sd, lower, upper, log);
}

//' @rdname dtnorm
//' @export
// [[Rcpp::export]]

double rtnorm_cpp(
   double mean, double sd, double point, bool above, bool log = false
) {
  return rtnorm(mean, sd, point, above, log);
}

//' @rdname dtnorm
//' @export
// [[Rcpp::export]]

double rttnorm_cpp(
   double mean, double sd, double lower, double upper, bool log = false
) {
  return rttnorm(mean, sd, lower, upper, log);
}

//' @rdname dwishart
//' @export
// [[Rcpp::export]]

double dwishart_cpp(
   arma::mat const& x, int const& df, arma::mat const& scale,
   bool log = false, bool inv = false
) {
  return dwishart(x, df, scale, log, inv);
}

//' @rdname dwishart
//' @export
// [[Rcpp::export]]

arma::mat rwishart_cpp(
   double df, arma::mat const& scale, bool inv = false
) {
  return rwishart(df, scale, inv);
}

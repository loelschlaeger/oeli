// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "../inst/include/oeli.h"

//' @rdname ddirichlet
//' @export
// [[Rcpp::export]]

double ddirichlet_cpp(
   arma::vec const& x, arma::vec const& concentration, bool log = false
) {
  return oeli::ddirichlet(x, concentration, log);
}

//' @rdname ddirichlet
//' @export
// [[Rcpp::export]]

arma::vec rdirichlet_cpp(
   arma::vec const& concentration
) {
  return oeli::rdirichlet(concentration);
}

//' @rdname dmixnorm
//' @export
// [[Rcpp::export]]

double dmixnorm_cpp(
   arma::vec const& x, arma::mat const& mean, arma::mat const& Sigma,
   arma::vec proportions, bool log = false
) {
   return oeli::dmixnorm(x, mean, Sigma, proportions, log);
}

//' @rdname dmixnorm
//' @export
// [[Rcpp::export]]

double pmixnorm_cpp(
   arma::vec const& x, arma::mat const& mean, arma::mat const& Sigma,
   arma::vec proportions, double abseps = 1e-3,
   Rcpp::Nullable<Rcpp::NumericVector> lower = R_NilValue,
   std::string method = "genz", int draws = 500
) {
  return oeli::pmixnorm(
    x, mean, Sigma, proportions, abseps, lower, method, draws
  );
}

//' @rdname dmixnorm
//' @export
// [[Rcpp::export]]

arma::vec rmixnorm_cpp(
   arma::mat const& mean, arma::mat const& Sigma, arma::vec proportions,
   bool log = false
) {
  return oeli::rmixnorm(mean, Sigma, proportions, log);
}

//' @rdname dmvnorm
//' @export
// [[Rcpp::export]]

double dmvnorm_cpp(
   arma::vec const& x, arma::vec const& mean, arma::mat const& Sigma,
   bool log = false
) {
  return oeli::dmvnorm(x, mean, Sigma, log);
}

//' @rdname dmvnorm
//' @export
// [[Rcpp::export]]

double pmvnorm_cpp(
   arma::vec const& x, arma::vec const& mean, arma::mat const& Sigma,
   double abseps = 1e-3,
   Rcpp::Nullable<Rcpp::NumericVector> lower = R_NilValue,
   std::string method = "genz", int draws = 500
) {
  return oeli::pmvnorm(x, mean, Sigma, abseps, lower, method, draws);
}

//' @rdname dmvnorm
//' @export
// [[Rcpp::export]]

arma::vec rmvnorm_cpp(
   arma::vec const& mean, arma::mat const& Sigma, bool log = false
) {
  return oeli::rmvnorm(mean, Sigma, log);
}

//' @rdname dtnorm
//' @export
// [[Rcpp::export]]

double dtnorm_cpp(
   double x, double mean, double sd, double point, bool above, bool log = false
) {
  return oeli::dtnorm(x, mean, sd, point, above, log);
}

//' @rdname dtnorm
//' @export
// [[Rcpp::export]]

double dttnorm_cpp(
   double x, double mean, double sd, double lower, double upper,
   bool log = false
) {
  return oeli::dttnorm(x, mean, sd, lower, upper, log);
}

//' @rdname dtnorm
//' @export
// [[Rcpp::export]]

double rtnorm_cpp(
   double mean, double sd, double point, bool above, bool log = false
) {
  return oeli::rtnorm(mean, sd, point, above, log);
}

//' @rdname dtnorm
//' @export
// [[Rcpp::export]]

double rttnorm_cpp(
   double mean, double sd, double lower, double upper, bool log = false
) {
  return oeli::rttnorm(mean, sd, lower, upper, log);
}

//' @rdname dwishart
//' @export
// [[Rcpp::export]]

double dwishart_cpp(
   arma::mat const& x, double df, arma::mat const& scale,
   bool log = false, bool inv = false
) {
  return oeli::dwishart(x, df, scale, log, inv);
}

//' @rdname dwishart
//' @export
// [[Rcpp::export]]

arma::mat rwishart_cpp(
   double df, arma::mat const& scale, bool inv = false
) {
  return oeli::rwishart(df, scale, inv);
}

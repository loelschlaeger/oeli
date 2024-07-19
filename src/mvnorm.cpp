// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

//' @rdname dmvnorm
//' @export
// [[Rcpp::export]]
double dmvnorm_cpp(
   arma::vec const& x, arma::vec const& mean, arma::mat const& Sigma,
   bool log = false
) {
 int p = x.size();
 double sqrt2pi = std::sqrt(2.0 * M_PI);
 arma::mat quadform  = trans(x-mean) * solve(Sigma, arma::eye(p,p)) * (x-mean);
 double norm = pow(sqrt2pi,-p) * pow(arma::det(Sigma),-0.5);
 double density = norm * exp(-0.5 * quadform(0,0));
 if (log) return std::log(density);
 return density;
}

//' @rdname dmvnorm
//' @export
// [[Rcpp::export]]
arma::vec rmvnorm_cpp(
   arma::vec mean, arma::mat const& Sigma, bool log = false
) {
 int p = mean.size();
 arma::mat L = trans(chol(Sigma));
 arma::vec eps = Rcpp::rnorm(p, 0.0, 1.0);
 arma::vec draw = L * eps + mean;
 if (log) {
   for (int j = 0; j < p; ++j) {
     draw(j) = std::exp(draw(j));
   }
 }
 return draw;
}

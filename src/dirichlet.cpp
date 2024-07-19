// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

//' @rdname ddirichlet
//' @export
// [[Rcpp::export]]
double ddirichlet_cpp(
  arma::vec const& x, arma::vec const& concentration, bool log = false
) {
 int p = x.size();
 double P = 1.0;
 for (int j = 0; j < p; ++j) {
   P *= pow(x(j), concentration(j) - 1);
 }
 double norm = 1.0;
 for (int j = 0; j < p; ++j) {
   norm *= std::tgamma(concentration(j));
 }
 norm = norm / std::tgamma(sum(concentration));
 double density = P / norm;
 if (log) return std::log(density);
 return density;
}

//' @rdname ddirichlet
//' @export
// [[Rcpp::export]]
arma::vec rdirichlet_cpp(
  arma::vec const& concentration
) {
 int p = concentration.size();
 arma::vec draw = arma::zeros<arma::vec>(p);
 double sum_term = 0;
 for (int j = 0; j < p; ++j) {
   double cur = R::rgamma(concentration[j], 1.0);
   draw(j) = cur;
   sum_term += cur;
 }
 for (int j = 0; j < p; ++j) {
   draw(j) = draw(j) / sum_term;
 }
 return(draw);
}

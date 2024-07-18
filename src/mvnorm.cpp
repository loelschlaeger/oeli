// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

//' Compute density of multivariate normal distribution
//'
//' @description
//' This function computes the density of a multivariate normal distribution.
//'
//' @details
//' This function performs no input checks. See \code{\link{dmvnorm}}
//' for the version with input checks.
//'
//' @param x
//' A \code{numeric}, a quantile vector of length \code{p}.
//' @param mean
//' A \code{numeric}, the mean vector of length \code{p}.
//' @param Sigma
//' A \code{matrix}, the covariance matrix of dimension \code{p} x \code{p}.
//' @param log
//' A \code{logical}, if \code{TRUE} the logarithm of the density value is
//' returned.
//' By default, \code{log = FALSE}.
//'
//' @return
//' A \code{numeric}, the density value.
//'
//' @keywords internal
//'
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

//' Draw from multivariate normal distribution
//'
//' @description
//' This function draws from a multivariate normal distribution.
//'
//' @details
//' The function builds upon the following fact:
//' If \eqn{\epsilon = (\epsilon_1,\dots,\epsilon_p)},
//' where each \eqn{\epsilon_i} is drawn independently from a standard normal
//' distribution, then \eqn{\mu + L\epsilon} is a draw from the \eqn{p}-variate
//' normal distribution \eqn{N(\mu,\Sigma)}, where \eqn{L} is the lower
//' triangular factor of the Choleski decomposition of \eqn{\Sigma}.
//'
//' This function performs no input checks. See \code{\link{rmvnorm}}
//' for the version with input checks.
//'
//' @inheritParams dmvnorm_cpp
//' @param log
//' A \code{logical}, if \code{TRUE} the draw is taken from the log-normal
//' distribution.
//' By default, \code{log = FALSE}.
//'
//' @return
//' A column vector of length \code{p}, the random draw.
//'
//' @keywords internal
//'
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

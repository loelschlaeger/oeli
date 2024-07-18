// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

//' Compute density of (Inverse-) Wishart distribution
//'
//' @description
//' This function computes the density of the (Inverse-) Wishart distribution.
//'
//' @details
//' This function performs no input checks. See \code{\link{dwishart}}
//' for the version with input checks.
//'
//' @param x
//' A \code{matrix}, a covariance matrix of dimension \code{p} x \code{p}.
//' @param df
//' An \code{integer}, the degrees of freedom.
//' Must be greater or equal \code{p}.
//' @param scale
//' A \code{matrix}, the scale matrix of dimension \code{p} x \code{p}.
//' Must be a covariance matrix.
//' @param log
//' A \code{logical}, if \code{TRUE} the logarithm of the density value is
//' returned.
//' By default, \code{log = FALSE}.
//' @param inv
//' A \code{logical}, if \code{TRUE} the density of the Inverse-Wishart
//' distribution is returned.
//' By default, \code{inv = FALSE}.
//'
//' @return
//' A \code{numeric}, the density value.
//'
//' @keywords internal
//'
// [[Rcpp::export]]
double dwishart_cpp(
    arma::mat const& x, int const& df, arma::mat const& scale,
    bool log = false, bool inv = false
) {
  int p = x.n_cols;
  double dfh = df * 0.5;
  double P = 1.0;
  double norm = 1.0;
  double gamma = pow(M_PI, (p * (p - 1) * 0.25));
  for (int j = 1; j < (p + 1); ++j) {
    gamma *= std::tgamma(dfh - ((j - 1.0) * 0.5));
  }
  if (inv) {
    P = pow(arma::det(scale), dfh);
    P *= std::exp(-0.5 * arma::trace(scale * x.i()));
    P *= pow(arma::det(x), (-0.5 * (df + p + 1.0)));
    norm = pow(2.0, (0.5 * (df * p))) * gamma;
  } else {
    P = pow(arma::det(x), ((df - p - 1.0) * 0.5));
    P *= std::exp(-0.5 * arma::trace(scale.i() * x));
    norm = pow(2.0, (dfh * p)) * pow(arma::det(scale), dfh) * gamma;
  }
  double density = P / norm;
  if (log) return std::log(density);
  return density;
}

//' Draw from Wishart distribution
//'
//' @description
//' This function draws from a Wishart or Inverse-Wishart distribution.
//'
//' @details
//' The Wishart distribution is a generalization to multiple dimensions of the
//' gamma distributions and draws from the space of covariance matrices.
//' Its expectation is \code{df * scale}, and its variance increases both in
//' \code{df} and in the values of \code{scale}.
//'
//' This function performs no input checks. See \code{\link{rwishart}}
//' for the version with input checks.
//'
//' @inheritParams dwishart_cpp
//'
//' @return
//' A \code{matrix}, the random draw.
//'
//' @keywords internal
//'
// [[Rcpp::export]]
arma::mat rwishart_cpp(double df, arma::mat const& scale, bool inv = false){
 int m = scale.n_rows;
 arma::mat T = arma::zeros<arma::mat>(m,m);
 for(int i = 0; i < m; i++) {
   T(i,i) = std::sqrt(R::rchisq(df-i));
 }
 for(int j = 0; j < m; j++) {
   for(int i = j+1; i < m; i++) {
     T(i,j) = R::rnorm(0.0, 1.0);
   }
 }
 arma::mat C = trans(T)*chol(scale);
 arma::mat CI = solve(trimatu(C), arma::eye(m,m));
 if (inv) {
   return trans(C) * C;
 } else {
   return CI * trans(CI);
 }
}


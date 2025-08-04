// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]

double dwishart(
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

// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]

arma::mat rwishart(
  double df, arma::mat const& scale, bool inv = false
) {
  int m = scale.n_rows;
  arma::mat T = arma::zeros<arma::mat>(m, m);
  for (int i = 0; i < m; ++i) {
    T(i, i) = std::sqrt(R::rchisq(df - i));
    for (int j = 0; j < i; ++j) {
      T(i, j) = R::rnorm(0.0, 1.0);
    }
  }
  arma::mat L;
  if (inv) {
    arma::mat scale_inv = arma::inv_sympd(scale);
    L = arma::chol(scale_inv, "lower");
  } else {
    L = arma::chol(scale, "lower");
  }
  arma::mat C = L * T;
  arma::mat A = C * C.t();
  if (inv) {
    return A.i();
  } else {
    return A;
  }
}

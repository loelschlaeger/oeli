// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

//' Draw from Dirichlet distribution
//'
//' @description
//' This function draws from a Dirichlet distribution.
//'
//' @details
//' This function performs no input checks. See \code{\link{rdirichlet}}
//' for the version with input checks.
//'
//' @param concentration
//' The non-negative concentration vector of length \code{p}.
//'
//' @return
//' A column vector of length \code{p}, the random draw.
//'
//' @keywords internal
//'
// [[Rcpp::export]]
arma::vec rdirichlet_cpp(arma::vec concentration) {
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

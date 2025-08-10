#ifndef INTERNAL_H
#define INTERNAL_H

// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

double ddirichlet_cpp(
    arma::vec const& x, arma::vec const& concentration, bool log = false
);

arma::vec rdirichlet_cpp(arma::vec const& concentration);

double dmixnorm_cpp(
    arma::vec const& x, arma::mat const& mean, arma::mat const& Sigma,
    arma::vec proportions
);

double pmixnorm_cpp(
    arma::vec const& x, arma::mat const& mean, arma::mat const& Sigma,
    arma::vec proportions, double abseps = 1e-3
);

arma::vec rmixnorm_cpp(
    arma::mat const& mean, arma::mat const& Sigma, arma::vec proportions
);

double dmvnorm_cpp(
    arma::vec const& x, arma::vec const& mean, arma::mat const& Sigma,
    bool log = false
);

double pmvnorm_cpp(
    arma::vec const& x, arma::vec const& mean, arma::mat const& Sigma,
    double abseps = 1e-3
);

arma::vec rmvnorm_cpp(
    arma::vec const& mean, arma::mat const& Sigma, bool log = false
);

double dtnorm_cpp(
    double x, double mean, double sig, double point, bool above,
    bool log = false
);

double dttnorm_cpp(
    double x, double mean, double sig, double lower, double upper,
    bool log = false
);

double rtnorm_cpp(
    double mean, double sig, double point, bool above, bool log = false
);

double rttnorm_cpp(
    double mean, double sig, double lower, double upper, bool log = false
);

double dwishart_cpp(
    arma::mat const& x, int const& df, arma::mat const& scale,
    bool log = false, bool inv = false
);

arma::mat rwishart_cpp(double df, arma::mat const& scale, bool inv = false);

#endif

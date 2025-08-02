#ifndef INTERNAL_H
#define INTERNAL_H

// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

double ddirichlet_cpp(arma::vec const& x, arma::vec const& concentration, bool log = false);

arma::vec rdirichlet_cpp(arma::vec const& concentration);

double dmvnorm_cpp(arma::vec const& x, arma::vec const& mean, arma::mat const& Sigma, bool log = false);

arma::vec rmvnorm_cpp(arma::vec mean, arma::mat const& Sigma, bool log = false);

double dtnorm_cpp(double x, double mean, double sig, double point, bool above, bool log = false);

double dttnorm_cpp(double x, double mean, double sig, double lower, double upper, bool log = false);

double rtnorm_cpp(double mean, double sig, double point, bool above, bool log = false);

double rttnorm_cpp(double mean, double sig, double lower, double upper, bool log = false);

double dwishart_cpp(arma::mat const& x, int const& df, arma::mat const& scale, bool log = false, bool inv = false);

arma::mat rwishart_cpp(double df, arma::mat const& scale, bool inv = false);

#endif

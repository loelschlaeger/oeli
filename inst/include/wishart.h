#ifndef WISHART_H
#define WISHART_H

// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

double dwishart_cpp(arma::mat const& x, int const& df, arma::mat const& scale, bool log = false, bool inv = false);

arma::mat rwishart_cpp(double df, arma::mat const& scale, bool inv = false);

#endif

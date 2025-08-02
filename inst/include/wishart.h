#ifndef OELI_WISHART_H
#define OELI_WISHART_H

// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

double dwishart(arma::mat const& x, int const& df, arma::mat const& scale, bool log = false, bool inv = false);

arma::mat rwishart(double df, arma::mat const& scale, bool inv = false);

#endif

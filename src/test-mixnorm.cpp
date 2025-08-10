#include <testthat.h>
#include "internal.h"

context("dmixnorm") {

  test_that("univariate mixture normal density can be computed") {
    arma::vec x = arma::zeros<arma::vec>(1);
    arma::mat mean = arma::zeros<arma::mat>(1, 2);
    arma::mat Sigma(1, 2); Sigma.col(0).fill(1.0); Sigma.col(1).fill(1.0);
    arma::vec proportions(2); proportions(0) = 0.6; proportions(1) = 0.4;
    double density = dmixnorm_cpp(x, mean, Sigma, proportions);
    int factor = 1000;
    double density_rounded = std::round(density * factor) / factor;
    expect_true(density_rounded == 0.399);
    arma::vec proportions2 = 2.0 * proportions;
    double density2 = dmixnorm_cpp(x, mean, Sigma, proportions2);
    expect_true(density == density2);
  }

  test_that("multivariate mixture normal density can be computed") {
    arma::vec x = arma::zeros<arma::vec>(2);
    arma::mat mean = arma::zeros<arma::mat>(2, 2);
    arma::mat Sigma(4, 2); // each column is vec(I2)
    Sigma.col(0) = arma::vectorise(arma::eye<arma::mat>(2, 2));
    Sigma.col(1) = arma::vectorise(arma::eye<arma::mat>(2, 2));
    arma::vec proportions(2); proportions(0) = 0.7; proportions(1) = 0.3;
    double density = dmixnorm_cpp(x, mean, Sigma, proportions);
    int factor = 1000;
    double density_rounded = std::round(density * factor) / factor;
    expect_true(density_rounded == 0.159);
  }

}

context("pmixnorm") {

  test_that("univariate mixture normal CDF can be computed") {
    arma::vec x = arma::zeros<arma::vec>(1);
    arma::mat mean = arma::zeros<arma::mat>(1, 3);
    arma::mat Sigma(1, 3); Sigma.col(0).fill(1.0); Sigma.col(1).fill(1.0);
    Sigma.col(2).fill(1.0);
    arma::vec proportions(3); proportions(0) = 0.5; proportions(1) = 0.3;
    proportions(2) = 0.2;
    double prob = pmixnorm_cpp(x, mean, Sigma, proportions, 1e-3);
    expect_true(prob == 0.5);
    double prob2 = pmixnorm_cpp(x, mean, Sigma, 2.0 * proportions, 1e-3);
    expect_true(prob == prob2);
  }

  test_that("multivariate mixture normal CDF can be computed") {
    arma::vec x = arma::zeros<arma::vec>(2);
    arma::mat mean = arma::zeros<arma::mat>(2, 2);
    arma::mat Sigma(4, 2);
    Sigma.col(0) = arma::vectorise(arma::eye<arma::mat>(2, 2));
    Sigma.col(1) = arma::vectorise(arma::eye<arma::mat>(2, 2));
    arma::vec proportions(2); proportions(0) = 0.4; proportions(1) = 0.6;
    double prob = pmixnorm_cpp(x, mean, Sigma, proportions, 1e-3);
    expect_true(prob == 0.25);
  }

}

context("rmixnorm") {

  test_that("univariate mixture normal can be drawn") {
    arma::mat mean(1, 1); mean.fill(1.0);
    arma::mat Sigma(1, 1); Sigma.fill(1.0);
    arma::vec props(1); props.fill(1.0);
    arma::vec draw = rmixnorm_cpp(mean, Sigma, props);
    expect_true(draw.n_elem == 1);
  }

  test_that("multivariate mixture normal can be drawn") {
    arma::mat mean = arma::zeros<arma::mat>(2, 1);
    arma::mat Sigma(4, 1);
    Sigma.col(0) = arma::vectorise(arma::eye<arma::mat>(2, 2));
    arma::vec props(1); props.fill(1.0);
    arma::vec draw = rmixnorm_cpp(mean, Sigma, props);
    expect_true(draw.n_elem == 2);
  }

}

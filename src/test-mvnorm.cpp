#include <testthat.h>
#include "mvnorm.h"

context("mvnorm") {

  test_that("multivariate normal density can be computed") {
    arma::vec x = arma::zeros<arma::vec>(2);
    arma::vec mean = arma::zeros<arma::vec>(2);
    arma::mat Sigma = arma::eye<arma::mat>(2,2);
    double density = dmvnorm_cpp(x, mean, Sigma, false);
    int factor = 1000;
    double density_rounded = round(density * factor) / factor;
    expect_true(density_rounded == 0.159);
    double density_log = dmvnorm_cpp(x, mean, Sigma, true);
    double density_log_rounded = round(density_log * factor) / factor;
    expect_true(density_log_rounded == -1.838);
  }

  test_that("univariate normal density can be computed") {
    arma::vec x = arma::zeros<arma::vec>(1);
    arma::vec mean = arma::zeros<arma::vec>(1);
    arma::mat Sigma = arma::eye<arma::mat>(1,1);
    double density = dmvnorm_cpp(x, mean, Sigma, false);
    int factor = 1000;
    double density_rounded = round(density * factor) / factor;
    expect_true(density_rounded == 0.399);
    double density_log = dmvnorm_cpp(x, mean, Sigma, true);
    double density_log_rounded = round(density_log * factor) / factor;
    expect_true(density_log_rounded == -0.919);
  }

  test_that("multivariate normal can be drawn") {
    arma::vec mean = arma::zeros<arma::vec>(2);
    arma::mat Sigma = arma::eye<arma::mat>(2,2);
    arma::vec draw = rmvnorm_cpp(mean, Sigma, false);
    expect_true(draw.size() == 2);
    arma::vec draw_log = rmvnorm_cpp(mean, Sigma, true);
    expect_true(draw_log.size() == 2);
    expect_true(draw_log(0) > 0);
    expect_true(draw_log(1) > 0);
  }

}

#include <testthat.h>
#include "mvnorm.h"

context("dmvnorm_cpp") {

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

  test_that("univariate normal density can be computed (degenerate case)") {
    arma::vec x_1 = arma::vec(1).fill(1);
    arma::vec x_2 = arma::vec(1).fill(1.1);
    arma::mat Sigma = arma::mat(1, 1, arma::fill::zeros);
    double density_1 = dmvnorm_cpp(x_1, x_1, Sigma, false);
    expect_true(std::isinf(density_1));
    double density_1_log = dmvnorm_cpp(x_1, x_1, Sigma, true);
    expect_true((std::isinf(density_1_log) && density_1_log > 0));
    double density_2 = dmvnorm_cpp(x_1, x_2, Sigma, false);
    expect_true(density_2 == 0.0);
    double density_2_log = dmvnorm_cpp(x_1, x_2, Sigma, true);
    expect_true((std::isinf(density_2_log) && density_2_log < 0));
  }

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

  test_that("multivariate normal density can be computed (degenerate case)") {
    arma::vec x_1 = arma::vec(2).fill(1);
    arma::vec x_2 = arma::vec(2).fill(1.1);
    arma::mat Sigma = arma::mat(2, 2, arma::fill::zeros);
    double density_1 = dmvnorm_cpp(x_1, x_1, Sigma, false);
    expect_true(std::isinf(density_1));
    double density_1_log = dmvnorm_cpp(x_1, x_1, Sigma, true);
    expect_true((std::isinf(density_1_log) && density_1_log > 0));
    double density_2 = dmvnorm_cpp(x_1, x_2, Sigma, false);
    expect_true(density_2 == 0.0);
    double density_2_log = dmvnorm_cpp(x_1, x_2, Sigma, true);
    expect_true((std::isinf(density_2_log) && density_2_log < 0));
  }

}

context("rmvnorm_cpp") {

  test_that("univariate normal can be drawn") {
    arma::vec mean = arma::vec(1).fill(1);
    arma::mat Sigma = arma::eye<arma::mat>(1,1);
    arma::vec draw = rmvnorm_cpp(mean, Sigma, false);
    expect_true(draw.size() == 1);
    arma::vec draw_log = rmvnorm_cpp(mean, Sigma, true);
    expect_true(draw_log.size() == 1);
    expect_true(arma::all(draw_log > 0));
  }

  test_that("univariate normal can be drawn (degenerate case)") {
    arma::vec mean = arma::vec(1).fill(1);
    arma::mat Sigma = arma::mat(1, 1, arma::fill::zeros);
    arma::vec draw = rmvnorm_cpp(mean, Sigma, false);
    expect_true(draw.size() == 1);
    expect_true(arma::all(mean == draw));
    arma::vec draw_log = rmvnorm_cpp(mean, Sigma, true);
    expect_true(draw_log.size() == 1);
    expect_true(arma::all(draw_log == std::exp(1.0)));
  }

  test_that("multivariate normal can be drawn") {
    arma::vec mean = arma::zeros<arma::vec>(2);
    arma::mat Sigma = arma::eye<arma::mat>(2,2);
    arma::vec draw = rmvnorm_cpp(mean, Sigma, false);
    expect_true(draw.size() == 2);
    arma::vec draw_log = rmvnorm_cpp(mean, Sigma, true);
    expect_true(draw_log.size() == 2);
    expect_true(arma::all(draw_log > 0));
  }

  test_that("multivariate normal can be drawn (degenerate case)") {
    arma::vec mean = arma::vec(2).fill(1);
    arma::mat Sigma = arma::mat(2, 2, arma::fill::zeros);
    arma::vec draw = rmvnorm_cpp(mean, Sigma, false);
    expect_true(draw.size() == 2);
    expect_true(arma::all(mean == draw));
    arma::vec draw_log = rmvnorm_cpp(mean, Sigma, true);
    expect_true(draw_log.size() == 2);
    expect_true(arma::all(draw_log > 0));
  }

}

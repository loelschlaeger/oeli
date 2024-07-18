#include <testthat.h>
#include "wishart.h"

context("wishart") {

  test_that("Wishart density can be computed") {
    arma::mat x = arma::eye<arma::mat>(2,2);
    int df = 4;
    arma::mat scale = arma::eye<arma::mat>(2,2);
    scale(1) = 0.2; scale(2) = 0.2; scale(3) = 2.0;
    double density = dwishart_cpp(x, df, scale, false, false);
    int factor = 1000;
    double density_rounded = round(density * factor) / factor;
    expect_true(density_rounded == 0.005);
    double density_log = dwishart_cpp(x, df, scale, true, false);
    double density_log_rounded = round(density_log * factor) / factor;
    expect_true(density_log_rounded == -5.335);
  }

  test_that("Inverse-Wishart density can be computed") {
    arma::mat x = arma::eye<arma::mat>(2,2);
    int df = 4;
    arma::mat scale = arma::eye<arma::mat>(2,2);
    scale(1) = 0.2; scale(2) = 0.2; scale(3) = 2.0;
    double density = dwishart_cpp(x, df, scale, false, true);
    int factor = 1000;
    double density_rounded = round(density * factor) / factor;
    expect_true(density_rounded == 0.034);
    double density_log = dwishart_cpp(x, df, scale, true, true);
    double density_log_rounded = round(density_log * factor) / factor;
    expect_true(density_log_rounded == -3.378);
  }

  test_that("(Inverse-) Wishart can be drawn") {
    double df = 3.0;
    arma::mat scale = arma::eye<arma::mat>(2,2);
    arma::mat draw = rwishart_cpp(df, scale, false);
    expect_true(draw.n_rows == 2);
    expect_true(draw.n_cols == 2);
    arma::mat draw_log = rwishart_cpp(df, scale, true);
    expect_true(draw_log.n_rows == 2);
    expect_true(draw_log.n_cols == 2);
  }

}

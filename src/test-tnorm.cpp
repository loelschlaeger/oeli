#include <testthat.h>
#include "tnorm.h"

context("dtnorm_cpp") {

  test_that("univariate truncated normal density can be computed") {
    double x = 1.0;
    double mean = 0.0;
    double sd = 1.0;
    double point = 0.0;
    int factor = 1000;
    double density_b = dtnorm_cpp(x, mean, sd, point, false, false);
    double density_b_rounded = round(density_b * factor) / factor;
    expect_true(density_b_rounded == 0.484);
    double density_b_log = dtnorm_cpp(x, mean, sd, point, false, true);
    double density_b_log_rounded = round(density_b_log * factor) / factor;
    expect_true(density_b_log_rounded == -0.726);
    double density_a = dtnorm_cpp(x, mean, sd, point, true, false);
    double density_a_rounded = round(density_a * factor) / factor;
    expect_true(density_a_rounded == 0);
    double density_a_log = dtnorm_cpp(x, mean, sd, point, true, true);
    double density_a_log_rounded = round(density_a_log * factor) / factor;
    expect_true(density_a_log_rounded == 0);
  }

}

context("rtnorm_cpp") {

  test_that("univariate truncated normal can be drawn") {
    double mean = 0.0;
    double sd = 1.0;
    double point = 0.0;
    double draw_tb = rtnorm_cpp(mean, sd, point, false, false);
    expect_true(draw_tb > 0.0);
    double draw_ta = rtnorm_cpp(mean, sd, point, true, false);
    expect_true(draw_ta < 0.0);
  }

}

context("dttnorm_cpp") {

  test_that("univariate doubly truncated normal density can be computed") {
    double x_b = -2.0;
    double x_m = 0.0;
    double x_a = 2.0;
    double mean = 0.0;
    double sd = 1.0;
    double lower = -1.0;
    double upper = 1.0;
    int factor = 1000;
    double density_b = dttnorm_cpp(x_b, mean, sd, lower, upper, false);
    double density_b_rounded = round(density_b * factor) / factor;
    expect_true(density_b_rounded == 0);
    double density_b_log = dttnorm_cpp(x_b, mean, sd, lower, upper, true);
    double density_b_log_rounded = round(density_b_log * factor) / factor;
    expect_true(density_b_log_rounded == 0);
    double density_m = dttnorm_cpp(x_m, mean, sd, lower, upper, false);
    double density_m_rounded = round(density_m * factor) / factor;
    expect_true(density_m_rounded == 0.584);
    double density_m_log = dttnorm_cpp(x_m, mean, sd, lower, upper, true);
    double density_m_log_rounded = round(density_m_log * factor) / factor;
    expect_true(density_m_log_rounded == -0.537);
    double density_a = dttnorm_cpp(x_a, mean, sd, lower, upper, false);
    double density_a_rounded = round(density_a * factor) / factor;
    expect_true(density_a_rounded == 0);
    double density_a_log = dttnorm_cpp(x_a, mean, sd, lower, upper, true);
    double density_a_log_rounded = round(density_a_log * factor) / factor;
    expect_true(density_a_log_rounded == 0);
  }

}

context("rttnorm_cpp") {

  test_that("univariate doubly truncated normal can be drawn") {
    double mean = 0.0;
    double sd = 1.0;
    double lower = -1.0;
    double upper = 1.0;
    double draw = rttnorm_cpp(mean, sd, lower, upper);
    expect_true(draw > -1.0);
    expect_true(draw < 1.0);
  }

}

run_cpp_tests("oeli")

test_that("compiled tests are registered", {
  expect_true(is.loaded("run_testthat_tests", PACKAGE = "oeli"))
})

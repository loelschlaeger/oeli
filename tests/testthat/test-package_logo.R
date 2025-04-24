test_that("package logo can be created", {
  expect_true(ggplot2::is_ggplot(package_logo("my_package")))
})

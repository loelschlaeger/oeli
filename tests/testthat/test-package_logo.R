test_that("package logo can be created", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_true(ggplot2::is_ggplot(package_logo("my_package")))
})

test_that("custom background can be used", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  background <- ggplot2::ggplot(data = cars) +
    ggplot2::geom_point(ggplot2::aes(x = speed, y = dist))
  expect_true(
    ggplot2::is_ggplot(
      package_logo("my_package", background = background, brackets = TRUE)
    )
  )
})

test_that("Dirichlet density can be computed", {
  x <- c(0.5, 0.3, 0.2)
  concentration <- 1:3
  factor <- 1000
  expect_equal(
    round(ddirichlet(x = x, concentration = concentration, log = FALSE) * factor) / factor,
    0.72
  )
  expect_equal(
    round(ddirichlet(x = x, concentration = concentration, log = TRUE) * factor) / factor,
    -0.329
  )
  expect_error(
    ddirichlet(x = c(0.5, 0.3, 0.3), concentration = concentration),
    "Input `x` is bad"
  )
  expect_equal(ddirichlet(x = c(0.1, 0.2, 0.7), concentration = c(1, 1, 1)), 2)
})

test_that("Dirichlet can be drawn", {
  concentration <- 1:3
  expect_length(rdirichlet(concentration = concentration), 3)
  checkmate::expect_matrix(
    rdirichlet(n = 5, concentration = concentration),
    ncols = 3, nrows = 5
  )
  expect_error(
    rdirichlet(concentration = diag(3)),
    "Input `concentration` is bad"
  )
  checkmate::expect_matrix(
    rdirichlet(n = 5, concentration = 1),
    ncols = 1, nrows = 5
  )
})

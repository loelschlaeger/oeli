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
    "'x' must sum up to 1"
  )
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
    "Assertion on 'concentration' failed: Must be of type 'vector', not 'matrix'."
  )
  checkmate::expect_matrix(
    rdirichlet(n = 5, concentration = 1),
    ncols = 1, nrows = 5
  )
})

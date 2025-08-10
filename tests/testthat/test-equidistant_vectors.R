test_that("equidistant vectors in Euclidean space can be generated", {

  ### basic cases
  V <- equidistant_vectors(dim = 1, n = 2)
  expect_true(is.matrix(V))
  expect_identical(dim(V), c(1L, 2L))
  V <- equidistant_vectors(dim = 3, n = 4)
  expect_identical(dim(V), c(3L, 4L))

  ### center is respected
  center <- c(0.5, -1, 2)
  for (n in 1:4) {
    V <- equidistant_vectors(dim = 3, n = n, dist = 1, center = center)
    expect_lt(sqrt(sum((rowMeans(V) - center)^2)), 1e-10)
  }

  ### distance is respected
  set.seed(123)
  for (d in 1:5) {
    for (n in seq_len(d + 1)[-1]) {
      dist_target <- runif(1, min = 0.1, max = 3)
      center <- rnorm(d)
      V <- equidistant_vectors(
        dim = d, n = n, dist = dist_target, center = center
      )
      pw <- as.vector(stats::dist(t(V)))
      expect_lt(abs(mean(pw) - dist_target), 1e-8)
    }
  }
})

test_that("simulating regressors works", {
  labels <- c("N1", "N2", "N3")
  n <- 100
  data <- correlated_regressors(labels, n)
  checkmate::expect_data_frame(data, col.names = "strict", nrows = n)
})

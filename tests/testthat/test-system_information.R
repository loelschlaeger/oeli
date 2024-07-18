test_that("system information can be obtained", {
  x <- system_information()
  checkmate::expect_list(x)
  checkmate::expect_names(
    names(x), identical.to = c("machine", "cores", "ram", "os", "rversion")
  )
})

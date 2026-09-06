test_that("namespace calls can be found", {
  path <- tempfile()
  dir.create(path)
  writeLines(
    c("a <- pkg:::fun(1)", "b <- pkg2::fun2(2) + pkg2::fun3(3)"),
    file.path(path, "code.R")
  )
  calls <- find_namespace_calls(path)
  checkmate::expect_data_frame(calls, nrows = 2, ncols = 5)
  expect_equal(calls$call, c("pkg2::fun2", "pkg2::fun3"))
  calls <- find_namespace_calls(path, triple_colon = TRUE)
  expect_equal(calls$call, c("pkg:::fun", "pkg2::fun2", "pkg2::fun3"))
  expect_equal(
    find_namespace_calls(path, triple_colon = TRUE, as_list = TRUE),
    list(pkg = "fun", pkg2 = c("fun2", "fun3"))
  )
  empty <- tempfile()
  dir.create(empty)
  calls <- find_namespace_calls(empty)
  checkmate::expect_data_frame(calls, nrows = 0, ncols = 5)
  expect_equal(find_namespace_calls(empty, as_list = TRUE), list())
  unlink(c(path, empty), recursive = TRUE)
})

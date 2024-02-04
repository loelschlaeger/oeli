test_that("lists can be merged", {
  expect_equal(
    merge_lists(list("a" = 1, "b" = 2, "z" = NULL)),
    list(a = 1, b = 2, z = NULL)
  )
  expect_equal(
    merge_lists(list("a" = 1, "b" = 2, "z" = NULL), list()),
    list(a = 1, b = 2, z = NULL)
  )
  expect_equal(
    merge_lists(list("a" = 1, "b" = 2, "z" = NULL), list("b" = 3, "c" = 4)),
    list(a = 1, b = 2, z = NULL, c = 4)
  )
  expect_equal(
    merge_lists(list("a" = 1, "b" = 2), list("b" = 3, "c" = 4, "z" = NULL)),
    list(a = 1, b = 2, c = 4, z = NULL)
  )
  expect_equal(
    merge_lists(list("a" = 1, "b" = 2), list("b" = 3, "c" = 4), list("d" = 5)),
    list(a = 1, b = 2, c = 4, d = 5)
  )
  expect_equal(
    merge_lists(list("a" = 1, "b" = 2), list("b" = 3, "c" = 4), list("z" = NULL)),
    list(a = 1, b = 2, c = 4, z = NULL)
  )
})

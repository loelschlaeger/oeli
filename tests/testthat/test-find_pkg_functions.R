test_that("R functions can be found in package source files", {
  pkg_dir <- file.path(tempdir(), "findpkgfunctions")
  unlink(pkg_dir, recursive = TRUE)
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "man"), recursive = TRUE)

  writeLines(
    c(
      "Package: findpkgfunctions",
      "Title: Test Package",
      "Version: 0.0.1",
      "Description: A tiny package for tests.",
      "License: GPL-3",
      "Encoding: UTF-8"
    ),
    file.path(pkg_dir, "DESCRIPTION")
  )
  writeLines(
    "export(exported_fun)",
    file.path(pkg_dir, "NAMESPACE")
  )
  writeLines(
    c(
      "exported_fun <- function(x) x",
      "internal_fun <- function() NULL",
      "container <- list(method = function() TRUE)"
    ),
    file.path(pkg_dir, "R", "functions.R")
  )
  writeLines(
    c(
      "\\name{test-functions}",
      "\\alias{exported_fun}",
      "\\alias{internal_fun}",
      "\\title{Test functions}"
    ),
    file.path(pkg_dir, "man", "test-functions.Rd")
  )

  out <- find_pkg_functions(pkg_dir, include_namespace = FALSE)

  expect_s3_class(out, "data.frame")
  expect_s3_class(out, "tbl_df")
  expect_equal(names(out), c("name", "title", "exported", "signature"))
  expect_equal(anyDuplicated(out$name), 0L)
  expect_true(all(c("exported_fun", "internal_fun", "method") %in% out$name))
  expect_equal(out$title[out$name == "exported_fun"], "Test functions")
  expect_equal(out$title[out$name == "internal_fun"], "Test functions")
  expect_true(out$exported[out$name == "exported_fun"][1])
  expect_false(out$exported[out$name == "internal_fun"][1])
})

test_that("functions can be found in an installed namespace", {
  out <- find_pkg_functions("utils", include_source = FALSE)

  expect_s3_class(out, "data.frame")
  expect_s3_class(out, "tbl_df")
  expect_equal(names(out), c("name", "title", "exported", "signature"))
  expect_equal(anyDuplicated(out$name), 0L)
  expect_true("head" %in% out$name)
  expect_equal(
    out$title[out$name == "head"],
    "Return the First or Last Parts of an Object"
  )
})

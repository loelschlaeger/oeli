test_that("simulation works", {
  f <- function() rnorm(1, sd = 0.1)
  sim <- Simulator$new(verbose = FALSE)
  sim$define(f = f)
  sim$go(runs = 2, backup = FALSE)
  checkmate::expect_r6(sim, "Simulator")
  checkmate::expect_tibble(sim$cases, nrows = 2, ncols = 4)
  expect_error(sim$cases <- "bad", "read-only")
  checkmate::expect_tibble(sim$results, nrows = 2, ncols = 4)
  expect_error(sim$results <- "bad", "read-only")
})

test_that("re-run works", {
  throw_error <- TRUE
  g <- function(x, y) {
    if (x + y == 5 && throw_error) stop()
    x + y
  }
  sim <- Simulator$new(verbose = FALSE)
  sim$define(f = g, x = as.list(1:2), y = as.list(2:3))
  sim$go(runs = 1, backup = FALSE)
  expect_identical(
    sim$cases,
    structure(
      list(
        .case = 1:4, .pending = c(FALSE, FALSE, FALSE, TRUE),
        .error = c(FALSE, FALSE, FALSE, TRUE), .run = c(1L, 1L, 1L, 1L),
        x = list(1L, 2L, 1L, 2L), y = list(2L, 2L, 3L, 3L)
      ),
      row.names = c(NA, -4L), class = c("tbl_df", "tbl", "data.frame")
    )
  )
  expect_identical(
    sim$results |> dplyr::select(-.seconds),
    structure(
      list(
        .case = 1:3,
        .input = list(
          list(x = 1L, y = 2L), list(x = 2L, y = 2L), list(x = 1L, y = 3L)
        ),
        .output = list(3L, 4L, 4L)
      ),
      row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame")
    )
  )
  throw_error <- FALSE
  sim$go(backup = FALSE)
  expect_identical(
    sim$cases,
    structure(
      list(
        .case = 1:4, .pending = c(FALSE, FALSE, FALSE, FALSE),
        .error = c(FALSE, FALSE, FALSE, FALSE), .run = c(1L, 1L, 1L, 1L),
        x = list(1L, 2L, 1L, 2L), y = list(2L, 2L, 3L, 3L)
      ),
      row.names = c(NA, -4L), class = c("tbl_df", "tbl", "data.frame")
    )
  )
  expect_identical(
    sim$results |> dplyr::select(-.seconds),
    structure(
      list(
        .case = 1:4,
        .input = list(
          list(x = 1L, y = 2L), list(x = 2L, y = 2L), list(x = 1L, y = 3L), list(x = 2L, y = 3L)
        ),
        .output = list(3L, 4L, 4L, 5L)
      ),
      row.names = c(NA, -4L), class = c("tbl_df", "tbl", "data.frame")
    )
  )
})

test_that("backup works", {
  h <- function(x, y = 1) x + y
  sim <- Simulator$new(verbose = FALSE)
  sim$define(f = h, x = as.list(1:2))
  path <- file.path(tempdir(), paste0(sample(letters, size = 26), collapse = ""))
  sim$go(runs = 2, backup = TRUE, path = path)
  sim_restored <- Simulator$new(use_backup = path)
  expect_error(sim_restored$define(), "Definition already provided.")
  expect_identical(sim, sim_restored)
})

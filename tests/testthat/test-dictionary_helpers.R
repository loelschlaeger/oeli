test_that("dictionary with optimizers works", {
  optimizer_dictionary <- Dictionary$new(
    key_name = "label",
    alias_name = "method",
    value_names = c(
      "algorithm",
      "arg_objective",
      "arg_initial",
      "out_value",
      "out_parameter",
      "direction",
      "arguments",
      "out_elements"
    ),
    value_assert = alist(
      "algorithm" = checkmate::assert_function(),
      "arg_objective" = checkmate::assert_string(),
      "arg_initial" = checkmate::assert_string(),
      "out_value" = checkmate::assert_string(),
      "out_parameter" = checkmate::assert_string(),
      "direction" = checkmate::assert_choice(choices = c("min", "max")),
      "arguments" = checkmate::assert_list(names = "unique"),
      "out_elements" = checkmate::assert_character(any.missing = FALSE, unique = TRUE)
    ),
    allow_overwrite = FALSE,
    keys_reserved = c("custom"),
    alias_choices = c("constrained", "unconstrained")
  )
  expect_snapshot(
    print(optimizer_dictionary)
  )
  expect_s3_class(
    optimizer_dictionary,
    "Dictionary"
  )
  expect_error(
    optimizer_dictionary$add(
      "label" = "custom",
      "method" = c("unconstrained"),
      "algorithm" = stats::nlm,
      "arg_objective" = "f",
      "arg_initial" = "p",
      "out_value" = "minimum",
      "out_parameter" = "min",
      "direction" = "min",
      "arguments" = as.list(formals(stats::nlm)),
      "out_elements" = c("minimum", "estimate", "gradient", "hessian", "code", "iterations")
    ),
    "The key 'custom' must not be used."
  )
  expect_silent(
    optimizer_dictionary$add(
      "label" = "stats::nlm",
      "method" = c("unconstrained"),
      "algorithm" = stats::nlm,
      "arg_objective" = "f",
      "arg_initial" = "p",
      "out_value" = "minimum",
      "out_parameter" = "min",
      "direction" = "min",
      "arguments" = as.list(formals(stats::nlm)),
      "out_elements" = c("minimum", "estimate", "gradient", "hessian", "code", "iterations")
    )$add(
      "label" = "stats::optim",
      "algorithm" = stats::optim,
      "arg_objective" = "fn",
      "arg_initial" = "par",
      "out_value" = "minimum",
      "out_parameter" = "min",
      "direction" = "min",
      "arguments" = as.list(formals(stats::optim)),
      "out_elements" = c("par", "value", "counts", "convergence", "message", "hessian")
    )
  )
  expect_snapshot(
    print(optimizer_dictionary)
  )
  expect_equal(
    optimizer_dictionary$get("stats::nlm", "arg_initial"),
    "p"
  )
  expect_silent(
    optimizer_dictionary$remove("stats::optim")
  )
  expect_equal(
    optimizer_dictionary$keys,
    "stats::nlm"
  )
  expect_equal(
    optimizer_dictionary$alias,
    list(unconstrained = "stats::nlm")
  )
})

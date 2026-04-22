# Changelog

## oeli 0.7.6

- Addressed [\#6](https://github.com/loelschlaeger/oeli/issues/6).

- Updated documentation.

- Added data `hermann`.

- Added functions
  [`check_one_hot_matrix()`](http://loelschlaeger.de/oeli/reference/check_one_hot_matrix.md),
  [`test_one_hot_matrix()`](http://loelschlaeger.de/oeli/reference/check_one_hot_matrix.md),
  and
  [`assert_one_hot_matrix()`](http://loelschlaeger.de/oeli/reference/check_one_hot_matrix.md).

## oeli 0.7.5

CRAN release: 2025-08-18

- Moved header files from `../src/` to `../inst/include/` to make them
  available externally.

- Removed degenerate case from
  [`dmvnorm()`](http://loelschlaeger.de/oeli/reference/dmvnorm.md).

- Fixed a bug in sampling from the inverse Wishart distribution.

- Added function
  [`pmvnorm()`](http://loelschlaeger.de/oeli/reference/dmvnorm.md) that
  calculates the (multivariate) Gaussian CDF.

- Added function
  [`gaussian_tv()`](http://loelschlaeger.de/oeli/reference/gaussian_tv.md)
  that calculates Gaussian total variation.

- Added function
  [`equidistant_vectors()`](http://loelschlaeger.de/oeli/reference/equidistant_vectors.md)
  that generates equidistant vectors in Euclidean space.

- Added functions
  [`dmixnorm()`](http://loelschlaeger.de/oeli/reference/dmixnorm.md),
  [`pmixnorm()`](http://loelschlaeger.de/oeli/reference/dmixnorm.md),
  and [`rmixnorm()`](http://loelschlaeger.de/oeli/reference/dmixnorm.md)
  that compute the density, cumulative distribution function, and
  samples from a mixture of multivariate normal distribution.

## oeli 0.7.4

CRAN release: 2025-06-27

- Added R6 object `Simulator` that provides some convenience when
  running simulations.

- Added function
  [`occurrence_info()`](http://loelschlaeger.de/oeli/reference/occurrence_info.md)
  that provides verbose information about absolute or relative
  occurrences.

## oeli 0.7.3

CRAN release: 2025-05-19

- Added function
  [`find_namespace_calls()`](http://loelschlaeger.de/oeli/reference/find_namespace_calls.md)
  that searches for namespace calls in `.R` files.

- Cleaned up package dependencies.

- Added option for custom background in
  [`package_logo()`](http://loelschlaeger.de/oeli/reference/package_logo.md).

## oeli 0.7.2

CRAN release: 2025-04-18

- Added function
  [`check_missing()`](http://loelschlaeger.de/oeli/reference/check_missing.md)
  that checks for missing formal arguments.

## oeli 0.7.1

CRAN release: 2024-11-27

- Allowed for zero-dimension edge cases in
  [`cov_to_chol()`](http://loelschlaeger.de/oeli/reference/cov_to_chol.md)
  and
  [`chol_to_cov()`](http://loelschlaeger.de/oeli/reference/cov_to_chol.md).

- Added function
  [`map_indices()`](http://loelschlaeger.de/oeli/reference/map_indices.md)
  that maps indices from an input vector to corresponding sequences of
  grouped indices.

- Added function
  [`M()`](http://loelschlaeger.de/oeli/reference/diff_cov.md) for taking
  differences such that the resulting vector is negative.

- Removed function `renv_development_packages()` and argument `use_logo`
  in
  [`package_logo()`](http://loelschlaeger.de/oeli/reference/package_logo.md)
  to get rid of dependency to [usethis](https://usethis.r-lib.org)
  package.

- Extended
  [`input_check_response()`](http://loelschlaeger.de/oeli/reference/input_check_response.md)
  to allow for multiple alternative checking criteria.

## oeli 0.7.0

CRAN release: 2024-10-16

- Improved documentation of
  [`diff_cov()`](http://loelschlaeger.de/oeli/reference/diff_cov.md),
  [`undiff_cov()`](http://loelschlaeger.de/oeli/reference/diff_cov.md),
  and [`delta()`](http://loelschlaeger.de/oeli/reference/diff_cov.md).

- Improved documentation of
  [`dmvnorm()`](http://loelschlaeger.de/oeli/reference/dmvnorm.md) and
  [`rmvnorm()`](http://loelschlaeger.de/oeli/reference/dmvnorm.md). The
  mean vector can now be of length 1 for convenience. The degenerate
  case (covariance is zero) is now supported. Also separate dimensions
  of `Sigma` can be degenerate for
  [`rmvnorm()`](http://loelschlaeger.de/oeli/reference/dmvnorm.md).

- Added function
  [`split_vector_at()`](http://loelschlaeger.de/oeli/reference/split_vector_at.md)
  to split a vector at specific positions.

- Allow for zero matrix in
  [`cov_to_chol()`](http://loelschlaeger.de/oeli/reference/cov_to_chol.md).

- Added function
  [`round_data.frame()`](http://loelschlaeger.de/oeli/reference/round_data.frame.md)
  that rounds `numeric` columns of a `data.frame`.

- Added function
  [`print_data.frame()`](http://loelschlaeger.de/oeli/reference/print_data.frame.md)
  that prints a (possibly abbreviated) `data.frame`.

- Renamed function `group_data_frame()` -\>
  [`group_data.frame()`](http://loelschlaeger.de/oeli/reference/group_data.frame.md).

- Renamed function `delete_data_frame_columns()` -\>
  [`delete_columns_data.frame()`](http://loelschlaeger.de/oeli/reference/delete_columns_data.frame.md).

## oeli 0.6.0

CRAN release: 2024-09-17

- Added function
  [`quiet()`](http://loelschlaeger.de/oeli/reference/quiet.md) to
  silence expressions.

- Added functions
  [`dtnorm()`](http://loelschlaeger.de/oeli/reference/dtnorm.md) and
  [`dttnorm()`](http://loelschlaeger.de/oeli/reference/dtnorm.md) to
  calculate the density of a (two-sided) truncated normal distribution.

- Added function
  [`correlated_regressors()`](http://loelschlaeger.de/oeli/reference/correlated_regressors.md)
  to simulate correlated regressor values.

- Improved documentation.

- Removed functions `check_date()` and `find_closest_year()` (too
  special, only needed for [fHMM](https://loelschlaeger.de/fHMM/)
  package).

## oeli 0.5.2

CRAN release: 2024-06-19

- Added function
  [`identical_structure()`](http://loelschlaeger.de/oeli/reference/identical_structure.md)
  to check if two objects have the identical structure.

- Fixed bug in
  [`matrix_diagonal_indices()`](http://loelschlaeger.de/oeli/reference/matrix_diagonal_indices.md):
  did not always return `integer`.

## oeli 0.5.1

- Added function
  [`vector_occurrence()`](http://loelschlaeger.de/oeli/reference/vector_occurrence.md)
  to find the positions of first or last occurrence of unique vector
  elements.

## oeli 0.5.0

CRAN release: 2024-05-22

- Added function
  [`chunk_vector()`](http://loelschlaeger.de/oeli/reference/chunk_vector.md)
  to split a vector into chunks.

- Added function
  [`insert_matrix_column()`](http://loelschlaeger.de/oeli/reference/insert_matrix_column.md)
  to add a column to a matrix.

- Added function
  [`insert_vector_entry()`](http://loelschlaeger.de/oeli/reference/insert_vector_entry.md)
  to add a entry to a vector.

- Renamed `basic_package_sticker()` -\>
  [`package_logo()`](http://loelschlaeger.de/oeli/reference/package_logo.md)
  and added a demo.

- Added function
  [`matrix_diagonal_indices()`](http://loelschlaeger.de/oeli/reference/matrix_diagonal_indices.md)
  that returns the indices of the diagonal elements of a quadratic
  matrix.

- Fixed bug in
  [`check_numeric_vector()`](http://loelschlaeger.de/oeli/reference/check_numeric_vector.md):
  `null.ok` did not work.

- Added function
  [`subsets()`](http://loelschlaeger.de/oeli/reference/subsets.md) that
  generates subsets of a vector.

- Fixed bug in
  [`system_information()`](http://loelschlaeger.de/oeli/reference/system_information.md):
  do not fail if information cannot be retrieved.

## oeli 0.4.1

CRAN release: 2024-02-24

- Removed HMM code and `plot_sequence()` function (both do not really
  fit in this package).

- Added more vignettes to illustrate use cases.

## oeli 0.4.0

CRAN release: 2024-02-04

- Fixed bug in checks for matrices by checking for `NA`, `NaN` and
  infinite values.

- Increment RoxygenNote to 7.3.0.

- Added function
  [`system_information()`](http://loelschlaeger.de/oeli/reference/system_information.md)
  that returns some general system level information.

- Added functions
  [`check_list_of_lists()`](http://loelschlaeger.de/oeli/reference/check_list_of_lists.md),
  [`test_list_of_lists()`](http://loelschlaeger.de/oeli/reference/check_list_of_lists.md),
  and
  [`assert_list_of_lists()`](http://loelschlaeger.de/oeli/reference/check_list_of_lists.md).

- Added functions
  [`check_numeric_vector()`](http://loelschlaeger.de/oeli/reference/check_numeric_vector.md),
  [`test_numeric_vector()`](http://loelschlaeger.de/oeli/reference/check_numeric_vector.md),
  and
  [`assert_numeric_vector()`](http://loelschlaeger.de/oeli/reference/check_numeric_vector.md).

- Fixed a bug in
  [`merge_lists()`](http://loelschlaeger.de/oeli/reference/merge_lists.md),
  where `NULL` elements got erroneously ignored.

## oeli 0.3.2

CRAN release: 2024-01-14

- Fixed bug in
  [`function_arguments()`](http://loelschlaeger.de/oeli/reference/function_arguments.md).

## oeli 0.3.1

CRAN release: 2023-12-08

- Fixed <https://github.com/RcppCore/Rcpp/issues/1287>.

## oeli 0.3.0

CRAN release: 2023-12-02

- Added function `check_date()` which checks if the input has the
  correct date format.

- Added function `find_closest_year()` which finds the closest year to a
  given date.

- Added function
  [`match_numerics()`](http://loelschlaeger.de/oeli/reference/match_numerics.md)
  which matches the indices of two numeric vectors as good as possible
  (that means with the smallest possible sum of deviations).

- Added function
  [`simulate_markov_chain()`](http://loelschlaeger.de/oeli/reference/simulate_markov_chain.md)
  which simulates a Markov chain.

- Added function
  [`sample_transition_probability_matrix()`](http://loelschlaeger.de/oeli/reference/sample_transition_probability_matrix.md)
  which samples a transition probability matrix.

- Added functions
  [`test_covariance_matrix()`](http://loelschlaeger.de/oeli/reference/check_covariance_matrix.md),
  [`test_correlation_matrix()`](http://loelschlaeger.de/oeli/reference/check_correlation_matrix.md),
  [`test_transition_probability_matrix()`](http://loelschlaeger.de/oeli/reference/check_transition_probability_matrix.md),
  and
  [`test_probability_vector()`](http://loelschlaeger.de/oeli/reference/check_probability_vector.md)
  (which are the “test” version of the corresponding “assert”
  functions).

- Added argument `tolerance` for covariance matrix, correlation matrix,
  transition probability matrix, and probability vector check functions
  to account for machine epsilon and avoid false positives.

- Added functions `ll_hmm()` and `simulate_hmm()` to compute
  log-likelihood and simulate data from hidden Markov model.

- Added argument `on_time_out` to function
  [`timed()`](http://loelschlaeger.de/oeli/reference/timed.md) that
  defines what action to take if the evaluation time exceeded (error,
  warning, or silent).

- Added support to select `identifier = "all"` in `Index` object and to
  suppress warnings (if unknown identifiers were selected) (globally)
  via the `$hide_warnings` field.

- Added argument `logical` to `Index` object to combine multiple
  identifiers either with logical and or logical or.

- Added function
  [`unexpected_error()`](http://loelschlaeger.de/oeli/reference/unexpected_error.md)
  that handles an unknown error.

- Added function `group_data_frame()` that groups a `data.frame`
  according to the values of a column.

- Added function `delete_data_frame_columns()` that deletes columns of a
  `data.frame`.

- Added function `renv_development_packages()` that creates a file that
  loads development packages so that
  [renv](https://rstudio.github.io/renv/) can detect and write them to
  the lockfile.

- Added function `plot_sequence()` that plots a sequence of numbers.

- Renamed `Index` -\> `Storage`.

- Initialized a package website with
  [pkgdown](https://pkgdown.r-lib.org/) and started to add vignettes.

## oeli 0.2.0

CRAN release: 2023-11-03

- Added function
  [`timed()`](http://loelschlaeger.de/oeli/reference/timed.md) which
  evaluates an expression and interrupts the evaluation after a defined
  amount of seconds.

- Added function
  [`do.call_timed()`](http://loelschlaeger.de/oeli/reference/do.call_timed.md)
  which measures the computation time of a
  [`do.call()`](https://rdrr.io/r/base/do.call.html) call.

- Added function
  [`try_silent()`](http://loelschlaeger.de/oeli/reference/try_silent.md)
  which tries to execute an expression and returns a string with the
  error message if the execution failed.

- Added R6 object `Index` that provides a simple indexing interface for
  list elements.

- Modified `basic_package_sticker()`: Option to add brackets to package
  name, now scales font, the function is no longer exported.

- Added function
  [`function_body()`](http://loelschlaeger.de/oeli/reference/function_body.md)
  which extracts the body of a function.

- Added function `permutation()` which creates all permutations of a
  given vector.

- Added function
  [`variable_name()`](http://loelschlaeger.de/oeli/reference/variable_name.md)
  which tries to determine the name of a variable passed to a function.

- Added function
  [`function_defaults()`](http://loelschlaeger.de/oeli/reference/function_defaults.md)
  which returns the default function arguments.

- Added R6 object `Dictionary` that provides a simple simple key-value
  interface.

- Added function
  [`merge_lists()`](http://loelschlaeger.de/oeli/reference/merge_lists.md)
  which merges `list`s based on their element names.

- Renamed arguments in function
  [`timed()`](http://loelschlaeger.de/oeli/reference/timed.md): `expr`
  -\> `expression`, `secs` -\> `seconds`.

- Added argument `units` to function
  [`do.call_timed()`](http://loelschlaeger.de/oeli/reference/do.call_timed.md).

## oeli 0.1.0

CRAN release: 2023-10-23

- Initial CRAN submission.

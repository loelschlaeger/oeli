# oeli 0.7.2

* Added function `check_missing()` that checks for missing formal arguments.

# oeli 0.7.1

* Allowed for zero-dimension edge cases in `cov_to_chol()` and `chol_to_cov()`.

* Added function `map_indices()` that maps indices from an input vector to corresponding sequences of grouped indices.

* Added function `M()` for taking differences such that the resulting vector is negative.

* Removed function `renv_development_packages()` and argument `use_logo` in `package_logo()` to get rid of dependency to `{usethis}` package.

* Extended `input_check_response()` to allow for multiple alternative checking criteria.

# oeli 0.7.0

* Improved documentation of `diff_cov()`, `undiff_cov()`, and `delta()`.

* Improved documentation of `dmvnorm()` and `rmvnorm()`. The mean vector can now be of length 1 for convenience. The degenerate case (covariance is zero) is now supported. Also separate dimensions of `Sigma` can be degenerate for `rmvnorm()`.

* Added function `split_vector_at()` to split a vector at specific positions.

* Allow for zero matrix in `cov_to_chol()`.

* Added function `round_data.frame()` that rounds `numeric` columns of a `data.frame`.

* Added function `print_data.frame()` that prints a (possibly abbreviated) `data.frame`.

* Renamed function `group_data_frame()` -> `group_data.frame()`.

* Renamed function `delete_data_frame_columns()` -> `delete_columns_data.frame()`.

# oeli 0.6.0

* Added function `quiet()` to silence expressions.

* Added functions `dtnorm()` and `dttnorm()` to calculate the density of a (two-sided) truncated normal distribution.

* Added function `correlated_regressors()` to simulate correlated regressor values.

* Improved documentation.

* Removed functions `check_date()` and `find_closest_year()` (too special, only needed for `{fHMM}` package).

# oeli 0.5.2

* Added function `identical_structure()` to check if two objects have the identical structure.

* Fixed bug in `matrix_diagonal_indices()`: did not always return `integer`.

# oeli 0.5.1

* Added function `vector_occurrence()` to find the positions of first or last occurrence of unique vector elements.

# oeli 0.5.0

* Added function `chunk_vector()` to split a vector into chunks.

* Added function `insert_matrix_column()` to add a column to a matrix.

* Added function `insert_vector_entry()` to add a entry to a vector.

* Renamed `basic_package_sticker()` -> `package_logo()` and added a demo.

* Added function `matrix_diagonal_indices()` that returns the indices of the diagonal elements of a quadratic matrix.

* Fixed bug in `check_numeric_vector()`: `null.ok` did not work.

* Added function `subsets()` that generates subsets of a vector.

* Fixed bug in `system_information()`: do not fail if information cannot be retrieved.

# oeli 0.4.1

* Removed HMM code and `plot_sequence()` function (both do not really fit in this package).

* Added more vignettes to illustrate use cases.

# oeli 0.4.0

* Fixed bug in checks for matrices by checking for `NA`, `NaN` and infinite values.

* Increment RoxygenNote to 7.3.0.

* Added function `system_information()` that returns some general system level information.

* Added functions `check_list_of_lists()`, `test_list_of_lists()`, and `assert_list_of_lists()`.

* Added functions `check_numeric_vector()`, `test_numeric_vector()`, and `assert_numeric_vector()`.

* Fixed a bug in `merge_lists()`, where `NULL` elements got erroneously ignored.

# oeli 0.3.2

* Fixed bug in `function_arguments()`.

# oeli 0.3.1

* Fixed https://github.com/RcppCore/Rcpp/issues/1287.

# oeli 0.3.0

* Added function `check_date()` which checks if the input has the correct date format.

* Added function `find_closest_year()` which finds the closest year to a given date.

* Added function `match_numerics()` which matches the indices of two numeric vectors as good as possible (that means with the smallest possible sum of deviations).

* Added function `simulate_markov_chain()` which simulates a Markov chain.

* Added function `sample_transition_probability_matrix()` which samples a transition probability matrix.

* Added functions `test_covariance_matrix()`, `test_correlation_matrix()`, `test_transition_probability_matrix()`, and `test_probability_vector()` (which are the "test" version of the corresponding "assert" functions).

* Added argument `tolerance` for covariance matrix, correlation matrix, transition probability matrix, and probability vector check functions to account for machine epsilon and avoid false positives.

* Added functions `ll_hmm()` and `simulate_hmm()` to compute log-likelihood and simulate data from hidden Markov model.

* Added argument `on_time_out` to function `timed()` that defines what action to take if the evaluation time exceeded (error, warning, or silent).

* Added support to select `identifier = "all"` in `Index` object and to suppress warnings (if unknown identifiers were selected) (globally) via the `$hide_warnings` field.

* Added argument `logical` to `Index` object to combine multiple identifiers either with logical and or logical or.

* Added function `unexpected_error()` that handles an unknown error.

* Added function `group_data_frame()` that groups a `data.frame` according to the values of a column.

* Added function `delete_data_frame_columns()` that deletes columns of a `data.frame`.

* Added function `renv_development_packages()` that creates a file that loads development packages so that `{renv}` can detect and write them to the lockfile.

* Added function `plot_sequence()` that plots a sequence of numbers.

* Renamed `Index` -> `Storage`.

* Initialized a package website with `{pkgdown}` and started to add vignettes.

# oeli 0.2.0

* Added function `timed()` which evaluates an expression and interrupts the evaluation after a defined amount of seconds.

* Added function `do.call_timed()` which measures the computation time of a `do.call()` call.

* Added function `try_silent()` which tries to execute an expression and returns a string with the error message if the execution failed.

* Added R6 object `Index` that provides a simple indexing interface for list elements.

* Modified `basic_package_sticker()`: Option to add brackets to package name, now scales font, the function is no longer exported.

* Added function `function_body()` which extracts the body of a function.

* Added function `permutation()` which creates all permutations of a given vector.

* Added function `variable_name()` which tries to determine the name of a variable passed to a function.

* Added function `function_defaults()` which returns the default function arguments.

* Added R6 object `Dictionary` that provides a simple simple key-value interface.

* Added function `merge_lists()` which merges `list`s based on their element names.

* Renamed arguments in function `timed()`: `expr` -> `expression`, `secs` -> `seconds`.

* Added argument `units` to function `do.call_timed()`.

# oeli 0.1.0

* Initial CRAN submission.

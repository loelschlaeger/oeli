# oeli development

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

# oeli 0.1.0

* Initial CRAN submission.

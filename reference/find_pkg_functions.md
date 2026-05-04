# Find R functions in a package

This function lists R functions found in an R package. It can inspect
the loaded namespace for exported and non-exported functions and, if a
package source path is available, scan `.R` files in `R/`.

## Usage

``` r
find_pkg_functions(pkg, include_namespace = TRUE, include_source = TRUE)
```

## Arguments

- pkg:

  \[`character(1)`\]  
  The name of an installed package or the path to a package source
  directory.

- include_namespace:

  \[`logical(1)`\]  
  Include functions found in the loaded package namespace?

- include_source:

  \[`logical(1)`\]  
  Include functions found by scanning `.R` source files?

## Value

A `tibble` with one row per found R function and columns `name`,
`title`, `exported`, and `signature`.

## Details

`include_namespace = TRUE` inspects the package namespace with
[`asNamespace()`](https://rdrr.io/r/base/ns-internal.html). This works
for installed packages and returns the R functions that are actually
available after the package is loaded, including internal functions.

`include_source = TRUE` scans the package source files under `R/`. This
is useful when `pkg` points to a package source directory, for example
while developing a package before it is installed. If both options are
`TRUE`, the result is combined and duplicate function names are removed.

The `title` column is read from Rd documentation aliases. It is `NA`
when no matching Rd documentation entry is available for a function.

## See also

Other package helpers:
[`Dictionary`](http://loelschlaeger.de/oeli/reference/Dictionary.md),
[`Storage`](http://loelschlaeger.de/oeli/reference/Storage.md),
[`check_missing()`](http://loelschlaeger.de/oeli/reference/check_missing.md),
[`find_namespace_calls()`](http://loelschlaeger.de/oeli/reference/find_namespace_calls.md),
[`identical_structure()`](http://loelschlaeger.de/oeli/reference/identical_structure.md),
[`input_check_response()`](http://loelschlaeger.de/oeli/reference/input_check_response.md),
[`match_arg()`](http://loelschlaeger.de/oeli/reference/match_arg.md),
[`package_logo()`](http://loelschlaeger.de/oeli/reference/package_logo.md),
[`print_data.frame()`](http://loelschlaeger.de/oeli/reference/print_data.frame.md),
[`print_matrix()`](http://loelschlaeger.de/oeli/reference/print_matrix.md),
[`system_information()`](http://loelschlaeger.de/oeli/reference/system_information.md),
[`unexpected_error()`](http://loelschlaeger.de/oeli/reference/unexpected_error.md),
[`user_confirm()`](http://loelschlaeger.de/oeli/reference/user_confirm.md)

## Examples

``` r
if (FALSE) { # \dontrun{
find_pkg_functions("R6")
find_pkg_functions(".")
} # }
```

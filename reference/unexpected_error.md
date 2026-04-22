# Handling of an unexpected error

This function reacts to an unexpected error by throwing an error and
linking to an issue site with the request to submit an issue.

## Usage

``` r
unexpected_error(
  msg = "Ups, an unexpected error occured.",
  issue_link = "https://github.com/loelschlaeger/oeli/issues"
)
```

## Arguments

- msg:

  \[`character(1)`\]  
  An error message.

- issue_link:

  \[`character(1)`\]  
  The URL to an issues site.

## Value

No return value, but it throws an error.

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
[`user_confirm()`](http://loelschlaeger.de/oeli/reference/user_confirm.md)

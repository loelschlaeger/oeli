# General system level information

This function returns a `list` of general system level information.

## Usage

``` r
system_information()
```

## Value

A `list` with elements:

- `maschine`, the model name of the device

- `cores`, the number of cores

- `ram`, the size of the RAM

- `os`, the operating system

- `rversion`, the R version used

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
[`unexpected_error()`](http://loelschlaeger.de/oeli/reference/unexpected_error.md),
[`user_confirm()`](http://loelschlaeger.de/oeli/reference/user_confirm.md)

## Examples

``` r
system_information()
#> $machine
#> [1] "Intel(R) Xeon(R) Platinum 8370C CPU @ 2.80GHz"
#> 
#> $cores
#> [1] 4
#> 
#> $ram
#> 16.8 GB
#> 
#> $os
#> [1] "unix"
#> 
#> $rversion
#> [1] ‘4.5.3’
#> 
```

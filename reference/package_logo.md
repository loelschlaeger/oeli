# Creating a basic logo for an R package

This function creates a basic R package logo. The logo has a white
background and the package name (with or without curly brackets) in the
center. The font size for the package name is scaled such that it fits
inside the logo. Type
[`?oeli`](http://loelschlaeger.de/oeli/reference/oeli-package.md) to see
an example.

## Usage

``` r
package_logo(
  package_name,
  brackets = FALSE,
  background = ggplot2::ggplot() + ggplot2::theme_void(),
  s_x = 1,
  s_y = 1,
  s_width = 1,
  s_height = 1,
  white_around_sticker = FALSE
)
```

## Arguments

- package_name:

  \[`character(1)`\]  
  The package name.

- brackets:

  \[`logical(1)`\]  
  Curly brackets around the package name?

- background:

  A `ggplot` object, the background of the sticker.

- s_x, s_y, s_width, s_height, white_around_sticker:

  Passed on to
  [`sticker`](https://rdrr.io/pkg/hexSticker/man/sticker.html).

## Value

A `ggplot` object.

## References

- This function builds upon
  [`sticker`](https://rdrr.io/pkg/hexSticker/man/sticker.html).

- Use `use_logo` to set up the logo for a package.

## See also

Other package helpers:
[`Dictionary`](http://loelschlaeger.de/oeli/reference/Dictionary.md),
[`Storage`](http://loelschlaeger.de/oeli/reference/Storage.md),
[`check_missing()`](http://loelschlaeger.de/oeli/reference/check_missing.md),
[`find_namespace_calls()`](http://loelschlaeger.de/oeli/reference/find_namespace_calls.md),
[`identical_structure()`](http://loelschlaeger.de/oeli/reference/identical_structure.md),
[`input_check_response()`](http://loelschlaeger.de/oeli/reference/input_check_response.md),
[`match_arg()`](http://loelschlaeger.de/oeli/reference/match_arg.md),
[`print_data.frame()`](http://loelschlaeger.de/oeli/reference/print_data.frame.md),
[`print_matrix()`](http://loelschlaeger.de/oeli/reference/print_matrix.md),
[`system_information()`](http://loelschlaeger.de/oeli/reference/system_information.md),
[`unexpected_error()`](http://loelschlaeger.de/oeli/reference/unexpected_error.md),
[`user_confirm()`](http://loelschlaeger.de/oeli/reference/user_confirm.md)

## Examples

``` r
print(package_logo("my_package", brackets = TRUE))
```

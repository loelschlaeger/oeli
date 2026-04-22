# Check list of lists

These functions check whether the input is a list that contains list
elements.

## Usage

``` r
check_list_of_lists(x, len = NULL)

assert_list_of_lists(
  x,
  len = NULL,
  .var.name = checkmate::vname(x),
  add = NULL
)

test_list_of_lists(x, len = NULL)
```

## Arguments

- x:

  \[`any`\]  
  Object to check.

- len:

  \[`integer(1)`\]  
  Exact expected length of `x`.

- .var.name:

  \[`character(1)`\]  
  Name of the checked object to print in assertions. Defaults to the
  heuristic implemented in
  [`vname`](https://mllg.github.io/checkmate/reference/vname.html).

- add:

  \[`AssertCollection`\]  
  Collection to store assertion messages. See
  [`AssertCollection`](https://mllg.github.io/checkmate/reference/AssertCollection.html).

## Value

Same as documented in
[`check_list`](https://mllg.github.io/checkmate/reference/checkList.html).

## See also

Other list helpers:
[`merge_lists()`](http://loelschlaeger.de/oeli/reference/merge_lists.md)

## Examples

``` r
L <- list(list(1), list(2), 3)
check_list_of_lists(L)
#> [1] "Check for element 3 failed: Must be of type 'list', not 'double'"
test_list_of_lists(L)
#> [1] FALSE
if (FALSE) { # \dontrun{
assert_list_of_lists(L)
} # }
```

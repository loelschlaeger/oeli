# Check probability vector

These functions check whether the input fulfills the properties of a
probability matrix.

## Usage

``` r
check_probability_vector(x, len = NULL, tolerance = sqrt(.Machine$double.eps))

assert_probability_vector(
  x,
  len = NULL,
  tolerance = sqrt(.Machine$double.eps),
  .var.name = checkmate::vname(x),
  add = NULL
)

test_probability_vector(x, len = NULL, tolerance = sqrt(.Machine$double.eps))
```

## Arguments

- x:

  \[`any`\]  
  Object to check.

- len:

  \[`integer(1)`\]  
  Exact expected length of `x`.

- tolerance:

  \[`numeric(1)`\]  
  A non-negative tolerance value.

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
[`check_numeric`](https://mllg.github.io/checkmate/reference/checkNumeric.html).

## See also

Other vector helpers:
[`check_numeric_vector()`](http://loelschlaeger.de/oeli/reference/check_numeric_vector.md),
[`chunk_vector()`](http://loelschlaeger.de/oeli/reference/chunk_vector.md),
[`equidistant_vectors()`](http://loelschlaeger.de/oeli/reference/equidistant_vectors.md),
[`insert_vector_entry()`](http://loelschlaeger.de/oeli/reference/insert_vector_entry.md),
[`map_indices()`](http://loelschlaeger.de/oeli/reference/map_indices.md),
[`match_numerics()`](http://loelschlaeger.de/oeli/reference/match_numerics.md),
[`permutations()`](http://loelschlaeger.de/oeli/reference/permutations.md),
[`split_vector_at()`](http://loelschlaeger.de/oeli/reference/split_vector_at.md),
[`subsets()`](http://loelschlaeger.de/oeli/reference/subsets.md),
[`vector_occurrence()`](http://loelschlaeger.de/oeli/reference/vector_occurrence.md)

## Examples

``` r
p <- c(0.2, 0.3, 0.6)
check_probability_vector(p)
#> [1] "Must add up to 1"
test_probability_vector(p)
#> [1] FALSE
if (FALSE) { # \dontrun{
assert_probability_vector(p)
} # }
```

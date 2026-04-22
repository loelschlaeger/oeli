# Split a vector into chunks

This function either

- splits a vector into `n` chunks of equal size (`type = 1`),

- splits a vector into chunks of size `n` (`type = 2`).

## Usage

``` r
chunk_vector(x, n, type = 1, strict = FALSE)
```

## Arguments

- x:

  \[atomic()\`\]  
  A vector of elements.

- n:

  \[`integer(1)`\]  
  A number smaller or equal `length(x)`.

- type:

  \[`1` \| `2`\]  
  Either

  - `1` (default) to split `x` into `n` chunks of equal size,

  - or `2` to split `x` into chunks of size `n`.

- strict:

  \[`logical(1)`\]  
  Set to `TRUE` to fail if `length(x)` is not a multiple of `n`, or
  `FALSE` (default), else.

## Value

A `list`.

## See also

Other vector helpers:
[`check_numeric_vector()`](http://loelschlaeger.de/oeli/reference/check_numeric_vector.md),
[`check_probability_vector()`](http://loelschlaeger.de/oeli/reference/check_probability_vector.md),
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
x <- 1:12
chunk_vector(x, n = 3, type = 1)
#> $`1`
#> [1] 1 2 3 4
#> 
#> $`2`
#> [1] 5 6 7 8
#> 
#> $`3`
#> [1]  9 10 11 12
#> 
chunk_vector(x, n = 3, type = 2)
#> $`1`
#> [1] 1 2 3
#> 
#> $`2`
#> [1] 4 5 6
#> 
#> $`3`
#> [1] 7 8 9
#> 
#> $`4`
#> [1] 10 11 12
#> 
try(chunk_vector(x, n = 5, strict = TRUE))
#> Error : Input `n` is bad: Not a multiple of 'length(x)'
```

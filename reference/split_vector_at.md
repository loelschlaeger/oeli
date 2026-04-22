# Split a vector at positions

This function splits a vector at specific positions.

## Usage

``` r
split_vector_at(x, at)
```

## Arguments

- x:

  \[atomic()\`\]  
  A vector of elements.

- at:

  \[[`integer()`](https://rdrr.io/r/base/integer.html)\]  
  Index position(s) just before to split.

  For example, `at = n` splits before the `n`th element of `x`.

## Value

A `list`.

## References

Based on https://stackoverflow.com/a/19274414.

## See also

Other vector helpers:
[`check_numeric_vector()`](http://loelschlaeger.de/oeli/reference/check_numeric_vector.md),
[`check_probability_vector()`](http://loelschlaeger.de/oeli/reference/check_probability_vector.md),
[`chunk_vector()`](http://loelschlaeger.de/oeli/reference/chunk_vector.md),
[`equidistant_vectors()`](http://loelschlaeger.de/oeli/reference/equidistant_vectors.md),
[`insert_vector_entry()`](http://loelschlaeger.de/oeli/reference/insert_vector_entry.md),
[`map_indices()`](http://loelschlaeger.de/oeli/reference/map_indices.md),
[`match_numerics()`](http://loelschlaeger.de/oeli/reference/match_numerics.md),
[`permutations()`](http://loelschlaeger.de/oeli/reference/permutations.md),
[`subsets()`](http://loelschlaeger.de/oeli/reference/subsets.md),
[`vector_occurrence()`](http://loelschlaeger.de/oeli/reference/vector_occurrence.md)

## Examples

``` r
x <- 1:10
split_vector_at(x, c(2, 3, 5, 7))
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [1] 3 4
#> 
#> [[4]]
#> [1] 5 6
#> 
#> [[5]]
#> [1]  7  8  9 10
#> 
```

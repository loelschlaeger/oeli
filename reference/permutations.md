# Build permutations

This function creates all permutations of a given `vector`.

## Usage

``` r
permutations(x)
```

## Arguments

- x:

  \[`atomic()`\]  
  Any `vector`.

## Value

A `list` of all permutations of `x`.

## References

Modified version of <https://stackoverflow.com/a/20199902/15157768>.

## See also

Other vector helpers:
[`check_numeric_vector()`](http://loelschlaeger.de/oeli/reference/check_numeric_vector.md),
[`check_probability_vector()`](http://loelschlaeger.de/oeli/reference/check_probability_vector.md),
[`chunk_vector()`](http://loelschlaeger.de/oeli/reference/chunk_vector.md),
[`equidistant_vectors()`](http://loelschlaeger.de/oeli/reference/equidistant_vectors.md),
[`insert_vector_entry()`](http://loelschlaeger.de/oeli/reference/insert_vector_entry.md),
[`map_indices()`](http://loelschlaeger.de/oeli/reference/map_indices.md),
[`match_numerics()`](http://loelschlaeger.de/oeli/reference/match_numerics.md),
[`split_vector_at()`](http://loelschlaeger.de/oeli/reference/split_vector_at.md),
[`subsets()`](http://loelschlaeger.de/oeli/reference/subsets.md),
[`vector_occurrence()`](http://loelschlaeger.de/oeli/reference/vector_occurrence.md)

## Examples

``` r
permutations(1:3)
#> [[1]]
#> [1] 1 2 3
#> 
#> [[2]]
#> [1] 1 3 2
#> 
#> [[3]]
#> [1] 2 1 3
#> 
#> [[4]]
#> [1] 2 3 1
#> 
#> [[5]]
#> [1] 3 1 2
#> 
#> [[6]]
#> [1] 3 2 1
#> 
permutations(LETTERS[1:3])
#> [[1]]
#> [1] "A" "B" "C"
#> 
#> [[2]]
#> [1] "A" "C" "B"
#> 
#> [[3]]
#> [1] "B" "A" "C"
#> 
#> [[4]]
#> [1] "B" "C" "A"
#> 
#> [[5]]
#> [1] "C" "A" "B"
#> 
#> [[6]]
#> [1] "C" "B" "A"
#> 
```

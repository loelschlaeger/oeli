# Dictionary R6 Object

Provides a simple key-value interface based on R6.

## See also

Other package helpers:
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

## Active bindings

- `keys`:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Available keys.

- `alias`:

  \[[`list()`](https://rdrr.io/r/base/list.html)\]  
  Available keys per alias value.

## Methods

### Public methods

- [`Dictionary$new()`](#method-Dictionary-new)

- [`Dictionary$add()`](#method-Dictionary-add)

- [`Dictionary$get()`](#method-Dictionary-get)

- [`Dictionary$remove()`](#method-Dictionary-remove)

- [`Dictionary$print()`](#method-Dictionary-print)

------------------------------------------------------------------------

### Method `new()`

Initializing a new `Dictionary` object.

#### Usage

    Dictionary$new(
      key_name,
      alias_name = NULL,
      value_names = character(),
      value_assert = alist(),
      allow_overwrite = TRUE,
      keys_reserved = character(),
      alias_choices = NULL,
      dictionary_name = NULL
    )

#### Arguments

- `key_name`:

  \[`character(1)`\]  
  The name for the key variable.

- `alias_name`:

  \[`NULL` \| `character(1)`\]  
  Optionally the name for the alias variable.

- `value_names`:

  \[`character(0)`\]  
  The names of the values connected to a key.

- `value_assert`:

  \[`alist(1)`\]  
  For each element in `value_names`, `values_assert` *can* have an
  identically named element of the form `checkmate::assert*(...)`, where
  `...` can be any argument for the assertion function except for the
  `x` argument.

- `allow_overwrite`:

  \[`logical(1)`\]  
  Allow overwriting existing keys with new values? Duplicate keys are
  never allowed.

- `keys_reserved`:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Names that must not be used as keys.

- `alias_choices`:

  \[`NULL` or [`character()`](https://rdrr.io/r/base/character.html)\]  
  Optionally possible values for the alias. Can also be `NULL`, then all
  alias values are allowed.

- `dictionary_name`:

  \[`NULL` or [`character()`](https://rdrr.io/r/base/character.html)\]  
  Optionally the name for the dictionary.

------------------------------------------------------------------------

### Method `add()`

Adding an element to the dictionary.

#### Usage

    Dictionary$add(...)

#### Arguments

- `...`:

  Values for

  - the key variable `key_name` (must be a single `character`),

  - the alias variable `alias_name` (optionally, must then be a
    `character` `vector`),

  - all the variables specified for `value_names` (if any, they must
    comply to the `value_assert` checks).

------------------------------------------------------------------------

### Method [`get()`](https://rdrr.io/r/base/get.html)

Getting elements from the dictionary.

#### Usage

    Dictionary$get(key, value = NULL)

#### Arguments

- `key`:

  \[`character(1)`\]  
  A value for the key variable `key_name`. Use the `$keys` method for
  available keys.

- `value`:

  \[`NULL` \| `character(1)`\]  
  One of the elements in `value_names`, selecting the required value.
  Can also be `NULL` (default) for all values connected to the `key`,
  returned as a `list`.

------------------------------------------------------------------------

### Method [`remove()`](https://rdrr.io/r/base/rm.html)

Removing elements from the dictionary (and associated alias, if any).

#### Usage

    Dictionary$remove(key)

#### Arguments

- `key`:

  \[`character(1)`\]  
  A value for the key variable `key_name`. Use the `$keys` method for
  available keys.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Printing details of the dictionary.

#### Usage

    Dictionary$print()

## Examples

``` r
# Managing variable metadata for a dataset

meta_dict <- Dictionary$new(
  key_name = "var_name",
  alias_name = "category",
  value_names = c("label", "type"),
  value_assert = alist(
    label = checkmate::assert_string(),
    type = checkmate::assert_choice(choices = c("numeric", "factor", "character"))
  ),
  allow_overwrite = FALSE,
  keys_reserved = c("id"),
  alias_choices = c("demographics", "outcome", "other"),
  dictionary_name = "Variable Metadata"
)

# Add entries to the dictionary
meta_dict$add(
  var_name = "age",
  label = "Age of respondent",
  type = "numeric",
  category = "demographics"
)

meta_dict$add(
  var_name = "gender",
  label = "Gender identity",
  type = "factor",
  category = "demographics"
)

meta_dict$add(
  var_name = "income",
  label = "Annual income in USD",
  type = "numeric",
  category = c("demographics", "outcome")
)

# Print dictionary
meta_dict$print()
#> <Dictionary> Variable Metadata 
#> Keys: 
#> - age
#> - gender
#> - income

# Retrieve full metadata for a variable
meta_dict$get("income")
#> $label
#> [1] "Annual income in USD"
#> 
#> $type
#> [1] "numeric"
#> 

# Retrieve a specific piece of metadata
meta_dict$get("income", value = "label")
#> [1] "Annual income in USD"

# Show variables by category
meta_dict$alias
#> $demographics
#> [1] "age"    "gender" "income"
#> 
#> $outcome
#> [1] "income"
#> 
```

# Category column values of a GRASS vector

Returns values in the `cat` column of a vector in **GRASS**.

## Usage

``` r
.vCats(x, layer = 1, db = FALSE, integer = TRUE)
```

## Arguments

- x:

  A `GVector` or the name of a vector in **GRASS**.

- layer:

  Integer, numeric integer, or character: Layer from which to obtain
  category values.

- db:

  Logical: If `TRUE`, return category numbers from the database table
  associated with the vector. If `FALSE` (default), return category
  numbers from the actual vector.

- integer:

  Logical: If `TRUE` (default), return category values as integers. In
  some cases, a geometry can have multiple categories, in which case
  `NA` is returned. If `FALSE`, return category values as strings (and
  thus, if a geometry has more than one category, does not convert to
  `NA`).

## Value

A vector.

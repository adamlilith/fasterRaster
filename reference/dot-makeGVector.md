# Create a GVector

Create a `GVector` from a vector existing in the current **GRASS**
session.

## Usage

``` r
.makeGVector(
  src,
  table = NULL,
  build = TRUE,
  extensive = FALSE,
  cats = NULL,
  fail = TRUE
)
```

## Arguments

- src:

  Character: The name of the vector in **GRASS**.

- table:

  A `data.table`, `data.frame`, `GVector` with a table, or character.
  This can be `data.table(NULL)` or `data.frame(NULL)` if there is no
  table associated with the vector. If a character, this is interpreted
  as the name of the table in **GRASS**.

- build:

  Logical: If `TRUE` (default), build topology using **GRASS** tool
  `v.build`.

- extensive:

  Logical: If `TRUE`, do extensive topological checks using `v.build`.
  The default is `FALSE`.

- cats:

  `NULL` (default) or an integer vector: Values of the "cats"
  (categories) of the vector in **GRASS**. This is useful *only* for
  speeding up the `GVector` creation process when the "cats" have
  already been ascertained.

- fail:

  Logical: If `TRUE` (default), and the vector either has a 0 east-west
  or north-south extent, then exit the function with an error. If `fail`
  is `FALSE`, then display a warning and return `NULL`.

## Value

A `GVector` (or `NULL` if `fail` is `TRUE` and the `GVector` would be
invalid).

## See also

[`.makeGRaster()`](https://github.com/adamlilith/fasterRaster/reference/dot-makeGRaster.md)

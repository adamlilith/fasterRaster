# Metadata on a vector in GRASS

This function queries **GRASS** to obtain metadata on a vector.

## Usage

``` r
.vectInfo(x, integer = TRUE, cats = NULL)
```

## Arguments

- x:

  A `GVector` or the
  [`sources()`](https://github.com/adamlilith/fasterRaster/reference/sources.md)
  name of one.

- integer:

  Logical. If `TRUE` (default), the "cats" of a vector will be returned
  as type integer. If `FALSE`, they will be returned as a character
  vector.

- cats:

  `NULL` (default) or an integer vector of category numbers, one per
  geometry.

## Value

A `vectInfo` object (a list).

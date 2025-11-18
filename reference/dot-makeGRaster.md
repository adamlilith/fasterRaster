# Create a GRaster

Create a `GRaster` from a raster existing in the current **GRASS**
session.

## Usage

``` r
.makeGRaster(src, names = "raster", levels = "", ac = NULL, fail = TRUE)
```

## Arguments

- src:

  Character (name of the raster in \*\*GRASS) or a `rastInfo` object.

- names:

  Character: Name of the raster.

- levels:

  `NULL` (default), a `data.frame`, `data.table`, an empty string
  (`""`), or a list of `data.frame`s, `data.table`s, and/or empty
  strings: These become the raster's
  [`levels()`](https://github.com/adamlilith/fasterRaster/reference/levels.md).
  If `""`, then no levels are defined.

- ac:

  Vector of numeric/integer values \>=1, or `NULL` (default): Active
  category column (offset by 1, so 1 really means the second column, 2
  means the third, etc.). A value of `NULL` uses an automated procedure
  to figure it out.

- fail:

  Logical: If `TRUE` (default), and the raster either has a 0 east-west
  or north-south extent, then exit the function with an error. If `fail`
  is `FALSE`, then display a warning and return `NULL`.

## Value

A `GRaster`.

## See also

[`.makeGVector()`](https://github.com/adamlilith/fasterRaster/reference/dot-makeGVector.md)

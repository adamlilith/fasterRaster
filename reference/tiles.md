# Divide a GRaster into spatially exclusive subsets

This function divides a `GRaster` into "tiles" or spatial subsets which
can be used for speeding some raster calculations. Tiles can be mutually
exclusive or overlap by a user-defined number of cells.

## Usage

``` r
# S4 method for class 'GRaster'
tiles(x, n, overlap = 0, verbose = FALSE)
```

## Arguments

- x:

  A `GRaster` with one or more layers.

- n:

  Numeric vector: Number of tiles to create. This can be a single
  number, in which case `x` is divided into `n` × `n` tiles, or two
  values in which case it is divided into `n[1]` × `n[2]` tiles (rows x
  columns).

- overlap:

  Numeric vector (default is 0): Number of rows/columns by which to
  expand the size of tiles so they overlap. This can be a single value
  or two values. If just one is provided, the tiles will be expanded by
  `overlap` rows and `overlaps` columns. If two numbers are provided,
  the tiles will be expanded by `overlap[1]` rows and `overlap[2]`
  columns.

- verbose:

  Logical: If `TRUE`, display progress. Default is `FALSE`. Progress is
  only displayed if `x` is a multi-layer `GRaster`.

## Value

If `x` has just one layer, then the output is a `list` with one element
per tile. The [`lapply()`](https://rdrr.io/r/base/lapply.html) and
[`sapply()`](https://rdrr.io/r/base/lapply.html) functions can be used
to apply functions to each tile in the list. If `x` has more than one
layer, then the output will be a `list` of `list`s, with each sub-`list`
containing the tiles for one `GRaster` layer.

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Elevation raster
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

# Create spatially exclusive tiles:
exclusive <- tiles(elev, n = 2, verbose = TRUE)

startpar <- par(mfrow = c(2, 3))
plot(elev, main = "Original")

for (i in seq_along(exclusive)) {
  plot(exclusive[[i]], ext = elev, main = paste("Tile", i))
}
par(startpar)

# Create tiles that overlap:
overlaps <- tiles(elev, n = 2, overlap = 200, verbose = TRUE)

startpar <- par(mfrow = c(2, 3))
plot(elev, main = "Original")

for (i in seq_along(overlaps)) {
  plot(overlaps[[i]], ext = elev, main = paste("Tile", i))
}
par(startpar)

}
```

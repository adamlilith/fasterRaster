# Remove rows and columns from a raster that are all NA

This function removes any rows and columns from a `GRaster` that are all
`NA`.

If the `GRaster` is a stack of rasters, then the rasters will all be
trimmed to the same extent such that none have any rows or columns that
are all `NA`. In other words, if at least one raster in the stack has a
non-`NA` cell in a row or column, all rasters will retain that row or
column.

## Usage

``` r
# S4 method for class 'GRaster'
trim(x, pad = 0)
```

## Arguments

- x:

  A `GRaster`.

- pad:

  Numeric integer: Number of `NA` rows and columns to retain. The
  default is 0.

## Value

A `GRaster`.

## See also

[`terra::trim()`](https://rspatial.github.io/terra/reference/trim.html),
[`extend()`](https://github.com/adamlilith/fasterRaster/reference/extend.md),
and **GRASS** tool `g.region`

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Elevation raster
madElev <- fastData("madElev")

# Convert SpatRaster to a GRaster:
elev <- fast(madElev)

# Trim NA rows/columns:
trimmedElev <- trim(elev)
dim(elev)
dim(trimmedElev)

# Trim a "stack" of rasters. We will artificially add NA rows and columns to
# one raster to demonstrate how the trim() function removes only rows/columns
# that are NA for *all* rasters.
elevNAs <- elev

fun <- " = if(col() > ncols() - 200, null(), madElev)"
elevNAs <- app(elevNAs, fun)

# Notice raster is "narrower" because we added NA columns
plot(elevNAs)

elevs <- c(elev, elevNAs)
trimmedElevs <- trim(elevs)
trimmedElevNAs <- trim(elevNAs)

dim(elevs)
dim(trimmedElevNAs)
dim(trimmedElevs)

}
```

# Group adjacent cells with similar values

`clump()` identifies groups of adjacent cells that have the same value
or same approximate value, and assigns them a unique number, creating
"clumps" of same- or similar-valued cells.

## Usage

``` r
# S4 method for class 'GRaster'
clump(x, minDiff = 0, minClumpSize = 1, diagonal = TRUE)
```

## Arguments

- x:

  A `GRaster`.

- minDiff:

  Numeric in the range \[0, 1): Minimum difference between cells in
  order for them to be assigned to the same clump. This is a proportion
  of the range across all cells. For example, if `minDiff` is set to
  0.01, then the maximum difference between cells in a clump can be up
  to 1% of the entire range across all cells. Small values can create
  large clumps. The default is 0, in which case values have to be
  exactly the same.

- minClumpSize:

  Numeric integer \>= 1. Minimum number of cells in a clump. The default
  is 1.

- diagonal:

  Logical: If `TRUE` (default), then cells "connected" at corners will
  be included as part of the same clump.

## Value

A `GRaster`.

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Elevation raster
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

# Find clumps based on exact values. This will appear as a gradient because
# most cells are assigned to a group of 1 cell.
exact <- clump(elev)
plot(exact)

# Clump based on approximate values:
approx <- clump(elev, minDiff = 0.0075)
plot(approx)

# Clump based on approximate values with minimum clump size:
approx20 <- clump(elev, minDiff = 0.005, minClumpSize = 20)
plot(approx20)

approx
approx20

}
```

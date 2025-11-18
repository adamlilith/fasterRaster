# Select values from rasters in a stack based on values in another raster

`selectRange()` selects values from `GRaster`s in a "stack" based on the
values in another "selection" raster. For example, if the stack has
three layers (call them A, B, and C), the "selection" raster could have
values of 1, 2, or 3 in each cell. The raster that is returned will have
values from A wherever the selection raster is 1, B from where it is 2,
and C from where it is 3.

## Usage

``` r
# S4 method for class 'GRaster'
selectRange(x, y)
```

## Arguments

- x:

  A `GRaster`, typically with more than one layer.

- y:

  A `GRaster` with integer values. The raster will be rounded if it does
  not. The values are typically between 1 and the number of layers in
  `x`, but wherever they are outside this range, the returned raster
  will have `NA` values.

## Value

A `GRaster`.

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster
elev <- fast(madElev)

# Make a stack of various versions of "elev" from which to select from:
x <- c(elev, 10 * elev, ln(elev), -1 * elev)
x

# Make a layer with random numbers between 1 and 4:
fun <- "= round(rand(0.5, 4.5))"
y <- app(elev, fun = fun)

selected <- selectRange(x, y)

}
```

# Fill NA cells in a raster using interpolation

This function uses splines to fill `NA` cells in a raster based on the
values of nearby cells. Depending on the method used, not all `NA` cells
can be filled.

## Usage

``` r
# S4 method for class 'GRaster'
fillNAs(
  x,
  lambda = NULL,
  method = "bilinear",
  min = -Inf,
  max = Inf,
  cells = Inf
)
```

## Arguments

- x:

  A `GRaster`.

- lambda:

  Either `NULL` (default), or a numeric value \> 0: If `NULL`, then the
  function will use leave-one-out crossvalidation to find the optimal
  value.

- method:

  Character: Type of spline, either "`bilinear`" (default), "`bicubic`",
  or "`RST`" (regularized splines with tension). Partial matching is
  used and case is ignored.

  **Note**: The RST method will often display warnings, but these can be
  ignored.

- min, max:

  Numeric: Lowest and highest values allowed in the interpolated values.
  Values outside these bounds will be truncated to the minimum/maximum
  value(s) allowed. The default imposes no constraints. For
  multi-layered rasters, you can supply a single value for `min` and/or
  `max`, or multiple values (one per layer). Values will be recycled if
  there are fewer than one or them per layer in the raster.

- cells:

  Integer or numeric integer: Number of cells away from the non-`NA`
  cells to fill. For example, if `cells = 2`, then only cells within a
  2-cell buffer of non-`NA` cells will be filled. The default is `Inf`
  (fill all possible cells–some methods may not be able to do this,
  depending on the configuration of the raster).

## Value

A `GRaster`.

## See also

[`terra::interpNear()`](https://rspatial.github.io/terra/reference/interpNear.html),
**GRASS** tool `r.fillnulls` (see `grassHelp("r.fillnulls")`)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Elevation raster:
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

### Fill NAs:
bilinear <- fillNAs(elev)
bicubic <- fillNAs(elev, method = "bicubic")
rst <- fillNAs(elev, method = "rst")

maps <- c(elev, bilinear, bicubic, rst)
names(maps) <- c("original", "bilinear", "bicubic", "RST")
plot(maps)

### Constrain interpolated values to > 0
constrained <- fillNAs(elev, min = 0)

# Compare unconstrained and constrained:
minmax(bilinear)
minmax(constrained)

### Interpolate to only first 10 cells away from non-NA cells:
restrained <- fillNAs(elev, cells = 10)

maps <- c(elev, restrained)
names(maps) <- c("Original", "within 10 cells")
plot(maps)

}
```

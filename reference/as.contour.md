# Contour lines from a "GRaster"

Create a `GVector` of contour lines from a `GRaster`.

## Usage

``` r
# S4 method for class 'GRaster'
as.contour(x, nlevels, levels)
```

## Arguments

- x:

  A `GRaster`.

- nlevels:

  Numeric: A positive integer or missing (default). Number of levels at
  which to calculate contours. Levels will be calculated in equal-sized
  steps from the smallest to the largest value of `x`. Either `nlevels`
  or `levels` must be specified.

- levels:

  Numeric vector: A numeric vector of values at which to calculate
  contour lines. Either `nlevels` or `levels` must be specified.

## Value

A `GVector` representing contour lines.

## See also

[`terra::as.contour()`](https://rspatial.github.io/terra/reference/contour.html),
**GRASS** manual page for tool `r.contour` (see
`grassHelp("r.contour")`)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Elevation raster
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

# Calculate contour lines:
conts <- as.contour(elev, nlevels = 10)

plot(elev)
plot(conts, add = TRUE)

}
```

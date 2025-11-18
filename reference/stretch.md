# Rescale values in a GRaster

`stretch()` rescales the values in a `GRaster`. All values can be
rescaled, or just values in a user-defined range. This range can be
given by specifying either the lower and upper bounds of the range using
`smin` and `smax`, and/or by the quantiles (across all cells of the
raster) using `minq` and `maxq`.

## Usage

``` r
# S4 method for class 'GRaster'
stretch(x, minv = 0, maxv = 255, minq = 0, maxq = 1, smin = NA, smax = NA)
```

## Arguments

- x:

  A `GRaster`.

- minv, maxv:

  Numeric: Minimum and maximum values to which to rescale values.

- minq, maxq:

  Numeric: Specifies range of values to rescale, given by their
  quantiles. The default is to stretch all values (the 0th and 100th
  quantiles). One or both are ignored if `smin` and/or `smax` are
  provided.

- smin, smax:

  Numeric or `NA`: Specifies range of values to rescale. If `NA`
  (default), then all values are rescaled.

## Value

A `GRaster`.

## See also

[`terra::stretch()`](https://rspatial.github.io/terra/reference/stretch.html)
and tool `r.rescale` in **GRASS** (not used on this function)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster
elev <- fast(madElev)

### Stretch based on user-defined range

#  fasterRaster
fr <- stretch(elev, smin=1, smax=100)
fr

# terra
tr <- stretch(madElev, smin = 1, smax = 100)
tr

# Compare fasterRaster to terra output
fr <- rast(fr)
fr <- extend(fr, tr)
fr - tr

### Stretch values in a certain quantile range

#  fasterRaster
fr <- stretch(elev, minq = 0.25, maxq = 0.75)
fr

# terra
tr <- stretch(madElev, minq = 0.25, maxq = 0.75)
tr

# Compare fasterRaster to terra output
fr <- rast(fr)
fr <- extend(fr, tr)
fr - tr

}
```

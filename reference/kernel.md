# Kernel density estimator of points

`kernel()` creates a raster using a kernel density estimator of the
density of points in a "points" `GVector`.

## Usage

``` r
# S4 method for class 'GVector'
kernel(x, y, kernel = "Epanechnikov", optimize = TRUE, h = NULL)
```

## Arguments

- x:

  A "points" `GVector`.

- y:

  A `GRaster`: The extent and resolution of this raster will be used to
  create the density raster. Otherwise, values in this raster are
  ignored.

- kernel:

  Character: Name of the kernel function to use. Possible values
  include:

  - `"Epanechnikov"` (default)

  - `"Gaussian"`

  - `"uniform"`

  - `"triangular"`

  - `"quartic"`

  - `"triweight"`

  - `"cosine"`

  Partial matching is used, and case is ignored.

- optimize:

  Logical: If `TRUE` (default), then attempt to find the optimal radius
  less than or equal to the `radius` value using the "Gaussian" kernel.
  If `FALSE`, use the `radius` value as-is.

- h:

  Numeric or `NULL` (default): Smoothing bandwidth of kernel estimator.

  If this is `NULL`, the Epanechnikov kernel is used, and `optimize` is
  `TRUE`, then Silverman's rule-of-thumb is used to estimate the optimal
  value of `h`: \$\$h = 0.9 \* min(\sigma_x / n^1/6, \sigma_y /
  n^1/6)\$\$

  If the Gaussian kernel is used, and `optimize` is `TRUE`, then the
  **GRASS** `v.kernel` function will attempt to identify the optimal
  bandwidth, up to the value of `h`, if `h` is defined.

  Otherwise, if `h` is `NULL`, then the value will be arbitrarily set at
  1/5th of the shorter of the distance of the x- and y-extent of the
  points.

## Value

A `GRaster`.

## See also

**GRASS** manual page for tool `v.kernel` (see `grassHelp("v.kernel")`)

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Elevation raster, plant specimen collections, rivers vector,
# outline of area vector
madElev <- fastData("madElev")
madDypsis <- fastData("madDypsis")

# Convert to fasterRaster format:
elev <- fast(madElev)
dypsis <- fast(madDypsis)

# Kernel density estimation:
kde <- kernel(dypsis, elev)
plot(kde)
plot(dypsis, add = TRUE, pch = 1)

}
```

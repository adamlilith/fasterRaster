# Apply a principal component analysis (PCA) to layers of a GRaster

This function applies a principal component analysis to layers of a
`GRaster`.

## Usage

``` r
# S4 method for class 'GRaster'
princomp(x, scale = TRUE, scores = FALSE)
```

## Arguments

- x:

  A `GRaster` with two or more layers.

- scale:

  Logical: If `TRUE` (default), input layers will be rescaled by
  dividing each layer by its overall population standard deviation.
  Rasters will always be centered (have their mean subtracted from
  values). Centering and scaling is recommended when rasters values are
  in different units.

- scores:

  Logical: If `TRUE`, the `prcomp` object will have the scores attached
  to it. This can greatly increase the size of the object in memory if
  the input raster has many cells. It will also take more time. If
  `FALSE` (default), then skip returning scores.

## Value

A multi-layer `GRaster` with one layer per principal component axis. The
[`pcs()`](https://github.com/adamlilith/fasterRaster/reference/pcs.md)
function can be used on the output raster to retrieve a `prcomp` object
from the raster, which includes rotations (loadings) and proportions of
variance explained.

## See also

[`terra::princomp()`](https://rspatial.github.io/terra/reference/princomp.html),
[`terra::prcomp()`](https://rspatial.github.io/terra/reference/prcomp.html)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Climate raster:
madChelsa <- fastData("madChelsa")

# Convert a SpatRaster to a GRaster:
chelsa <- fast(madChelsa)

# Generate raster with layers representing principal component predictions:
pcRast <- princomp(chelsa, scale = TRUE)
plot(pcRast)

# Get information on the PCA:
prinComp <- pcs(pcRast)

prinComp
summary(prinComp)
plot(prinComp)

}
```

# Mask values in a raster

The output of `mask()` is a `GRaster` that has the same as values as the
input raster. However, if the `mask` argument is a `GRaster`, the output
will have `NA` values in the same cells that the `mask` raster has `NA`
cells. If the `mask` argument is a `GVector`, then the output raster
will have `NA` values in cells the `GVector` does not cover.

## Usage

``` r
# S4 method for class 'GRaster,GRaster'
mask(x, mask, inverse = FALSE, maskvalues = NA, updatevalue = NA)

# S4 method for class 'GRaster,GVector'
mask(x, mask, inverse = FALSE, updatevalue = NA)
```

## Arguments

- x:

  A `GRaster`.

- mask:

  A `GRaster` or `GVector`.

- inverse:

  Logical: If `TRUE`, the effect of the mask is inverted. That is, a
  copy of the input raster is made, but cells that overlap with an `NA`
  in the mask raster or are not covered by the mask vector retain their
  values. Cells that overlap with an `NA` in the mask raster or overlap
  with the mask vector are forced to `NA`.

- maskvalues:

  Numeric vector, including `NA` (only for when `mask` is a `GRaster`):
  The value(s) in the mask raster cells that serve as the mask. The
  default is `NA`, in which case cells in the input raster that overlap
  with `NA` cells in the mask are forced to `NA`.

- updatevalue:

  Numeric, including `NA` (default): The values assigned to masked
  cells.

## Value

A `GRaster`.

## See also

[`terra::mask()`](https://rspatial.github.io/terra/reference/mask.html),
**GRASS** tool `r.mask`

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Example data
madElev <- fastData("madElev") # raster
madForest <- fastData("madForest2000") # raster
madCoast <- fastData("madCoast4") # vector

# Convert to GRasters and GVectors
elev <- fast(madElev)
forest <- fast(madForest)
coast <- fast(madCoast)

ant <- coast[coast$NAME_4 == "Antanambe"]

# Mask by a raster or  vector:
maskByRast <- mask(elev, forest)
plot(c(forest, maskByRast))

maskByVect <- mask(elev, ant)
plot(maskByVect)
plot(ant, add = TRUE)

# Mask by a raster or vector, but invert mask:
maskByRastInvert <- mask(elev, forest, inverse = TRUE)
plot(c(forest, maskByRastInvert))

maskByVectInvert <- mask(elev, ant, inverse = TRUE)
plot(maskByVectInvert)
plot(ant, add = TRUE)

# Mask by a raster, but use custom values for the mask:
maskByRastCustomMask <- mask(elev, elev, maskvalues = 1:20)
plot(c(elev <= 20, maskByRastCustomMask))

# Mask by a raster or vector, but force masked values to a custom value:
byRastCustomUpdate <- mask(elev, forest, updatevalue = 7)
plot(byRastCustomUpdate)

byVectCustomUpdate <- mask(elev, ant, updatevalue = 7)
plot(byVectCustomUpdate)

# Mask by a raster, inverse, custom values, and custom update:
byRastAll <-
   mask(elev, elev, inverse = TRUE, maskvalues = 1:20, updatevalue = 7)

plot(c(elev, byRastAll))

}
```

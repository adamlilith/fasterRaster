# Create one GRaster layer per unique value in a GRaster

This function creates a multi-layered `GRaster` for every unique values
in an input `GRaster`. By default, the output will have a value of 1
wherever the input has the given value, and 0 elsewhere. This is useful
for creating dummy variable `GRaster` layers for use with models that
have factors, especially if the input `GRaster` is categorical. Note
that the
[`predict()`](https://github.com/adamlilith/fasterRaster/reference/predict.md)
function in **fasterRaster** usually does not need this treatment of
`GRaster`s since it can handle categorical rasters already.

## Usage

``` r
# S4 method for class 'GRaster'
segregate(x, classes = NULL, keep = FALSE, other = 0, bins = 100, digits = 3)
```

## Arguments

- x:

  A `GRaster`.

- classes:

  Either `NULL` (default) or a character vector with category labels for
  which to create outputs. If the input is not a categorical/factor or
  `integer` `GRaster`, this is ignored.

- keep:

  Logical: If `FALSE` (default), then the original value in the input
  `GRaster` will be retained in the each of the output `GRaster` layers
  wherever the input had the respective value. Other cells will be
  assigned a value of `other`.

- other:

  Numeric or `NA`: Value to assign to cells that do not have the target
  value.

- bins:

  Numeric: Number of bins in which to put values. This is only used for
  `GRaster`s that are not categorical/factor rasters or `integer`
  rasters.

- digits:

  Numeric: Number of digits to which to round input if it is a `numeric`
  or `double` `GRaster` (see
  `vignettes("GRasters", package = "fasterRaster")`).

## Value

If the input `x` is a single-layered `GRaster`, the output will be a
multi-layered `GRaster` with one layer per value in the input, or one
layer per values in `classes`. If the input is a multi-layered
`GRaster`, the output will be a `list` of multi-layered `GRaster`s.

## See also

[`terra::segregate()`](https://rspatial.github.io/terra/reference/segregate.html)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Elevation and land cover raster
madElev <- fastData("madElev") # integer raster
madCover <- fastData("madCover") # categorical raster

# Convert to GRasters
elev <- fast(madElev)
cover <- fast(madCover)

# Subset elevation raster to just a few values to make example faster:
elevSubset <- elev[elev <= 3]
segregate(elevSubset)
segregate(elevSubset, keep = TRUE, other = -1)

# Segregate the factor raster
segregate(cover)

classes <- c("Grassland with mosaic forest", "Mosaic cropland/vegetation")
seg <- segregate(cover, classes = classes)
plot(seg)

}
```

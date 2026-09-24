# Force cells with NA in any layer of a GRaster stack to NA

'na.omit()' returns a `GRaster` stack with the same number of layers as
the input, but with all cells that have `NA` in any layer set to `NA` in
all layers. This is useful for masking out areas where data are missing
in any of the rasters.

## Usage

``` r
# S4 method for class 'GRaster'
na.omit(object, verbose = FALSE)
```

## Arguments

- object:

  A "stack" of `GRaster`s.

- verbose:

  Logical: If `TRUE`, display progress.

## Value

A `GRaster`.

## See also

[`terra::na.omit()`](https://rspatial.github.io/terra/reference/na.omit.html),
[`stats::na.omit()`](https://rdrr.io/r/stats/na.fail.html), **GRASS**
tool `r.mapcalc` (see `grassHelp("r.mapcalc")`)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

### Mask layer-by-layer

# Elevation raster
madElev <- fastData("madElev")
madForest2000 <- fastData("madForest2000")

# Convert SpatRasters to GRasters:
elev <- fast(madElev)
forest2000 <- fast(madForest2000)

# Make a random layer and stack it with elevation:
x <- c(elev, forest2000)

# Put NAs in cells where any layer has an NA:
masked <- na.omit(x, verbose = TRUE)
plot(masked)

}
```

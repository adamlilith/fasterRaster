# Randomly sample cells from a GRaster

`sampleRast()` randomly samples cells from non-`NA` cells of a raster.
The output will be a raster with selected non-`NA` cells, and all other
cells set to `NA`. To generate random points, see
[`spatSample()`](https://github.com/adamlilith/fasterRaster/reference/spatSample.md).

## Usage

``` r
# S4 method for class 'GRaster'
sampleRast(
  x,
  size,
  prop = FALSE,
  maskvalues = NA,
  updatevalue = NULL,
  test = FALSE,
  seed = NULL
)
```

## Arguments

- x:

  A `GRaster`.

- size:

  Numeric: Number of cells or proportion of cells to select.

- prop:

  Logical: If `TRUE`, the value of `size` will be interpreted as a
  proportion of cells. The default is `FALSE` (`size` is interpreted as
  the number of cells to select).

- maskvalues:

  Numeric vector, including `NA`, or `NULL` (default): Values in the
  raster to select from. All others will be ignored. If this is `NULL`,
  then only non-`NA` cells will be selected for retention.

- updatevalue:

  Numeric or `NULL` (default): Value to assign to masked cells. If
  `NULL`, then the values in the input raster are retained.

- test:

  Logical: If `TRUE`, and `size` is greater than the number of non-`NA`
  cells in `x`, then fail. Testing this can take a long time for large
  rasters. The default is `FALSE`.

- seed:

  `NULL` (default) or numeric: If `NULL`, then a random seed will be
  generated for the random number generator. Otherwise a seed can be
  provided.

## Value

A `GRaster`.

## See also

[`spatSample()`](https://github.com/adamlilith/fasterRaster/reference/spatSample.md);
[`terra::spatSample()`](https://rspatial.github.io/terra/reference/sample.html),
tool `r.random` in **GRASS**

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Example data
madElev <- fastData("madElev") # raster

# Convert to GRasters and GVectors
elev <- fast(madElev)

### spatSample()

# Random points as data.frame or data.table:
randVals <- spatSample(elev, size = 20, values = TRUE)
randVals

# Random points as a points GVector:
randPoints <- spatSample(elev, size = 20, as.points = TRUE)
randPoints
plot(elev)
plot(randPoints, add = TRUE)

# Random points in a select area:
madCoast <- fastData("madCoast4") # vector
coast <- fast(madCoast)
ant <- coast[coast$NAME_4 == "Antanambe"] # subset

restrictedPoints <- spatSample(elev, size = 20, as.points = TRUE,
   strata = ant)

plot(elev)
plot(ant, add = TRUE)
plot(restrictedPoints, add = TRUE) # note 20 points for entire geometry

# Random points, one set per subgeometry:
stratifiedPoints <- spatSample(elev, size = 20, as.points = TRUE,
   strata = ant, byStratum = TRUE)

plot(elev)
plot(ant, add = TRUE)
plot(stratifiedPoints, pch = 21, bg = "red", add = TRUE) # note 20 points per subgeometry

# Random categories:
madCover <- fastData("madCover") # raster
cover <- fast(madCover)

randCover <- spatSample(cover, size = 20, values = TRUE,
     cat = TRUE, xy = TRUE)
randCover

### sampleRast()

# Random cells in non-NA cells:
rand <- sampleRast(elev, 10000)
plot(rand)
nonnacell(rand)

# Use custom values for the mask:
randCustomMask <- sampleRast(elev, 10000, maskvalues = 1:20)
plot(randCustomMask)

# Force selected values to a custom value:
randCustomUpdate <- sampleRast(elev, 10000, updatevalue = 7)
plot(randCustomUpdate)

# Custom values for mask and set selected cells to custom value:
randAll <- sampleRast(elev, 10000, maskvalues = 1:20, updatevalue = 7)
plot(randAll)

}
```

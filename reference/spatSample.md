# Sample random points from a GRaster or GVector

`spatSample()` randomly locates points across a `GRaster` or `GVector`.
It can return a `GVector`, the coordinates, values associated with the
points, or all of these. If you want to generate a raster with
randomly-sampled cells, see
[`sampleRast()`](https://github.com/adamlilith/fasterRaster/reference/sampleRast.md).

## Usage

``` r
# S4 method for class 'GRaster'
spatSample(
  x,
  size,
  as.points = FALSE,
  values = TRUE,
  cats = TRUE,
  xy = FALSE,
  strata = NULL,
  byStratum = FALSE,
  zlim = NULL,
  seed = NULL,
  verbose = FALSE
)

# S4 method for class 'GVector'
spatSample(
  x,
  size,
  as.points = FALSE,
  values = TRUE,
  xy = FALSE,
  byStratum = FALSE,
  zlim = NULL,
  seed = NULL
)
```

## Arguments

- x:

  A `GRaster` or `GVector`.

- size:

  Numeric value \> 0: Number of points to create.

- as.points:

  Logical: If `FALSE` (default), the output is a `data.frame` or
  `data.table`. If `TRUE`, the output is a "points" `GVector`.

- values:

  Logical: If `TRUE` (default), values of the `GRaster` at points are
  returned.

- cats:

  Logical: If `TRUE` (default) and the `GRaster` is categorical, then
  return the category label of each cell. If `values` is also `TRUE`,
  then the cell value will also be returned.

- xy:

  Logical: If `TRUE`, return the longitude and latitude of each point.
  Default is `FALSE`.

- strata:

  Either `NULL` (default), or a `GVector` defining strata. If supplied,
  the `size` argument will be interpreted as number of points to place
  per geometry in `strata`. Note that using strata can dramatically slow
  the process.

- byStratum:

  Logical: If `FALSE` (default), then `size` number of points will be
  placed within the entire area delineated by `strata`. If `TRUE`, then
  `size` points will be placed within each subgeometry of `strata`.

- zlim:

  Either `NULL` (default), or a vector of two numbers defining the lower
  and upper altitudinal bounds of coordinates. This cannot be combined
  with `values = TRUE` or `cats = TRUE`.

- seed:

  Either `NULL` (default) or an integer: Random number seed. If this is
  `NULL`, the a seed will be set randomly. Values will be rounded to the
  nearest integer.

- verbose:

  Logical: If `TRUE`, display progress. Default is `FALSE`.

## Value

A `data.frame`, `data.table`, or `GVector`.

## See also

[`sampleRast()`](https://github.com/adamlilith/fasterRaster/reference/sampleRast.md),
[`terra::spatSample()`](https://rspatial.github.io/terra/reference/sample.html),
tool `v.random` in **GRASS** (see `grassHelp("v.random")`)

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

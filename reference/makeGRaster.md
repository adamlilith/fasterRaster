# Create a GRaster from a GRASS raster

Create a `GRaster` from a raster existing in the current **GRASS**
session.

## Usage

``` r
makeGRaster(src, names = "raster", levels = "", ac = NULL, fail = TRUE)
```

## Arguments

- src:

  Character: The name of the raster in **GRASS**.

- names:

  Character: Name of the raster (cf.
  [`names()`](https://github.com/adamlilith/fasterRaster/reference/names.md)).

- levels:

  `NULL` (default), a `data.frame`, `data.table`, an empty string
  (`""`), or a list of `data.frame`s, `data.table`s, and/or empty
  strings: These become the raster's
  [`levels()`](https://github.com/adamlilith/fasterRaster/reference/levels.md).
  If `""`, then no levels are defined.

- ac:

  Vector of numeric/integer values \>=1, or `NULL` (default): Active
  category column (offset by 1, so 1 really means the second column, 2
  means the third, etc.). A value of `NULL` uses an automated procedure
  to figure it out.

- fail:

  Logical: If `TRUE` (default), and the raster either has a 0 east-west
  or north-south extent, then exit the function with an error. If `fail`
  is `FALSE`, then display a warning and return `NULL`.

## Value

A `GRaster`.

## See also

[`makeGVector()`](https://github.com/adamlilith/fasterRaster/reference/makeGVector.md)

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Example data
madElev <- fastData("madElev") # integer raster
madCover <- fastData("madCover") # categorical raster
madCoast4 <- fastData("madCoast4") # polygons vector
madRivers <- fastData("madRivers") # lines vector
madDypsis <- fastData("madDypsis") # points vector

### Create GRasters from SpatRasters

# Create an integer raster:
elev <- fast(madElev)
elev

# Create a categorical raster:
cover <- fast(madCover)
madCover
levels(madCover) # category levels

# Create a GRaster from a file on disk:
rastFile <- system.file("extdata", "madForest2000.tif", package = "fasterRaster")
forest2000 <- fast(rastFile)
forest2000

# Create a 1's raster that spans the world:
ones <- fast(rastOrVect = "raster", crs = "epsg:4326")
ones

### Create GVectors

# Create a GVector from an sf vector:
coast4 <- fast(madCoast4)
coast4

# Create a GVector from a SpatVector:
madRivers <- vect(madRivers)
class(madRivers)
rivers <- fast(madRivers)
rivers

# Create a GVector from a vector on disk:
vectFile <- system.file("extdata/shapes", "madCoast.shp",
   package = "fasterRaster")
coast0 <- fast(vectFile)
coast0

# Import only Dypsis occurrences in a restricted area:
ant <- coast4[coast4$NAME_4 == "Antanambe"]
dypsisRestrict <- fast(madDypsis, extent = ant)
dypsis <- fast(madDypsis)

plot(coast4)
plot(ant, col = "gray80", add = TRUE)
plot(dypsis, add = TRUE)
plot(dypsisRestrict, col = "red", add = TRUE)

# Create a generic GVector that spans the world:
wallToWall <- fast(rastOrVect = "vector", crs = "epsg:4326") # WGS84
wallToWall

# Create a GVector from a numeric vector
pts <- c(-90.2, 38.6, -122.3, 37.9)
pts <- fast(pts, crs = "epsg:4326") # WGS84

# Create a GVector from a matrix (can also use data.frame or data.table):
mat <- matrix(c(-90.2, 38.6, -122.3, 37.9), ncol = 2, byrow = TRUE)
mat <- fast(mat, crs = "epsg:4326", keepgeom = TRUE) # WGS84

}
```

# Create a GVector from a GRASS vector

Create a `GVector` from a vector existing in the current **GRASS**
session.

## Usage

``` r
makeGVector(
  src,
  table = NULL,
  build = TRUE,
  extensive = FALSE,
  cats = NULL,
  fail = TRUE
)
```

## Arguments

- src:

  Character: The name of the vector in **GRASS**.

- table:

  A `data.table`, `data.frame`, `GVector` with a table, or character.
  This can be `data.table(NULL)` or `data.frame(NULL)` if there is no
  table associated with the vector. If a character, this is interpreted
  as the name of the table in **GRASS**.

- build:

  Logical: If `TRUE` (default), build topology using **GRASS** tool
  `v.build`.

- extensive:

  Logical: If `TRUE`, do extensive topological checks using `v.build`.
  The default is `FALSE`.

- cats:

  `NULL` (default) or an integer vector: Values of the "cats"
  (categories) of the vector in **GRASS**. This is useful *only* for
  speeding up the `GVector` creation process when the "cats" have
  already been ascertained. For most users, this will be better left as
  `NULL`.

- fail:

  Logical: If `TRUE` (default), and the vector either has a 0 east-west
  or north-south extent, then exit the function with an error. If `fail`
  is `FALSE`, then display a warning and return `NULL`.

## Value

A `GVector` (or `NULL` if `fail` is `TRUE` and the `GVector` would be
invalid).

## See also

[`makeGRaster()`](https://github.com/adamlilith/fasterRaster/reference/makeGRaster.md)

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

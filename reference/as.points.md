# Convert a GRaster, or lines or polygons GVector to a points vector

`as.points()` converts a `GRaster`, or a lines or polygons `GVector` to
a points `GVector`.

For `GRasters`, the points have the coordinates of cell centers and are
assigned the cells' values. Only non-`NA` cells will be converted to
points.

For `GVectors`, each point will have the attributes of the line or
polygon to which it belonged. Points are extracted from each vertex.

## Usage

``` r
# S4 method for class 'GRaster'
as.points(x, values = TRUE)

# S4 method for class 'GVector'
as.points(x)
```

## Arguments

- x:

  A `GRaster`, `GVector`.

- values:

  Logical: If `TRUE` (default), create an attribute table with raster
  cell values, with one row per point.

## Value

A `points` `GVector`.

## See also

[`crds()`](https://github.com/adamlilith/fasterRaster/reference/crds.md),
[`as.lines()`](https://github.com/adamlilith/fasterRaster/reference/as.lines.md),
[`as.polygons()`](https://github.com/adamlilith/fasterRaster/reference/as.polygons.md),
[`terra::as.points()`](https://rspatial.github.io/terra/reference/as.points.html),
and modules `v.to.points` and `r.to.vect` in **GRASS**

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Elevation raster, outline of a part of Madagascar, and rivers vector:
madElev <- fastData("madElev")
madCoast0 <- fastData("madCoast0")
madRivers <- fastData("madRivers")

# Convert to GRaster and GVectors:
elev <- fast(madElev)
coast <- fast(madCoast0)
rivers <- fast(madRivers)

# For speed, we will first crop the raster to a small extent:
elevCrop <- crop(elev, c(2530000, 2532000, -1722000, -1720000))
elevPoints <- as.points(elevCrop)
elevPoints

plot(elevCrop)
plot(elevPoints, pch = 4, cex = 0.7, add = TRUE)

# Extract points from vectors:
coastPoints <- as.points(coast)
riversPoints <- as.points(rivers)

plot(coast)
plot(coastPoints, pch = 4, cex = 0.7, add = TRUE)

plot(rivers, col = "blue", add = TRUE)
plot(riversPoints, col = "blue", pch = 1, cex = 0.7, add = TRUE)

}
```

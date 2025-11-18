# Voronoi tessellation

This function creates a Voronoi tessellation from a set of spatial
points or polygons.

## Usage

``` r
# S4 method for class 'GVector'
voronoi(x, buffer = 0)
```

## Arguments

- x:

  A `GVector` "points" object.

- buffer:

  Numeric: By default, this function creates a vector that has an extent
  exactly the same as the input data. However, the apparent extent can
  be changed by setting this value to a value different from 0. Negative
  values reduce the size of the extent, and positive extend it. Units
  are in map units.

## Value

A `GVector`.

## See also

[`terra::voronoi()`](https://rspatial.github.io/terra/reference/voronoi.html),
[`sf::st_voronoi()`](https://r-spatial.github.io/sf/reference/geos_unary.html),
tool `v.voronoi` in **GRASS**

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)

# Example vectors
madDypsis <- fastData("madDypsis") # points
madCoast4 <- fastData("madCoast4") # polygons

# Convert sf vectors to GVectors
dypsis <- fast(madDypsis)
coast4 <- fast(madCoast4)
ant <- coast4[coast4$NAME_4 == "Antanambe"]

# Delaunay triangulation
dypsisDel <- delaunay(dypsis)
plot(dypsisDel)
plot(dypsis, pch = 1, col = "red", add = TRUE)

# Voronoi tessellation
vor <- voronoi(dypsis)
plot(vor)
plot(dypsis, pch = 1, col = "red", add = TRUE)

# Random Voronoi tessellation
rand <- rvoronoi(coast4, size = 100)
plot(rand)

}
```

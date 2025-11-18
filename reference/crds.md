# Coordinates of a vector"s features or a raster"s cell centers

Returns the coordinates of the center of cells of a `GRaster` or
coordinates of a `GVector`'s vertices. The output will be a `matrix`,
`data.frame`, or `list`. If you want the output to be a "points"
`GVector`, use
[`as.points()`](https://github.com/adamlilith/fasterRaster/reference/as.points.md).

## Usage

``` r
# S4 method for class 'GRaster'
crds(x, z = is.3d(x), na.rm = TRUE)

# S4 method for class 'GVector'
crds(x, z = is.3d(x))

st_coordinates(x)
```

## Arguments

- x:

  A `GVector` or a `GRaster`.

- z:

  If `TRUE` (default), return x-, y-, and z-coordinates. If `FALSE`,
  just return x- and y-coordinates. For 2-dimensional objects,
  z-coordinates will all be 0.

- na.rm:

  Logical: If `TRUE`, remove cells that are `NA` (`GRaster`s only).

## Value

A `matrix`, `data.frame`, or `list`.

## See also

[`terra::crds()`](https://rspatial.github.io/terra/reference/crds.html)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Plant specimens (points), elevation (raster)
madDypsis <- fastData("madDypsis")
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster, and sf to a GVector
dypsis <- fast(madDypsis)
elev <- fast(madElev)

### Get coordinates:
dypsisPoints <- crds(dypsis)
elevPoints <- crds(elev)

head(dypsisPoints)
head(elevPoints)

}
```

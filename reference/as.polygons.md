# Convert a raster to a polygons vector

`as.polygons()` converts a `GRaster` to a "polygons" `GVector`. After
running this function, [geometry
cleaning](https://github.com/adamlilith/fasterRaster/reference/breakPolys.md)
may be useful to use to "tidy up" the vector.

## Usage

``` r
# S4 method for class 'GRaster'
as.polygons(x, round = TRUE, smooth = FALSE)
```

## Arguments

- x:

  A `GRaster`. If more than one layer is in the `GRaster`, only the
  first will be used (with a warning).

- round:

  Logical: If `TRUE` (default), values in the raster will be rounded
  first before conversion to a vector. This causes cells that are
  adjacent that have the same (rounded) values to be combined into a
  single polygon. For more control, see
  [`clump()`](https://github.com/adamlilith/fasterRaster/reference/clump.md).

- smooth:

  Logical: If `TRUE`, round the corners of square features. Default is
  `FALSE`.

## Value

A `GVector`.

## See also

[`as.points()`](https://github.com/adamlilith/fasterRaster/reference/as.points.md),
[`as.lines()`](https://github.com/adamlilith/fasterRaster/reference/as.lines.md),
[`terra::as.polygons()`](https://rspatial.github.io/terra/reference/as.polygons.html),
[geometry
cleaning](https://github.com/adamlilith/fasterRaster/reference/breakPolys.md),
and **GRASS** tool `r.to.vect`

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Elevation
madElev <- fastData("madElev")

# Convert SpatRaster to GRaster:
elev <- fast(madElev)

# To speed things up, first group cells of similar value:
elevClumps <- clump(elev, minDiff = 0.0115)

# Convert to polygons:
rastToPolys <- as.polygons(elevClumps)
plot(rastToPolys)

}
```

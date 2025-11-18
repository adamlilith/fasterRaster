# Convert a GVector to a GRaster

The `rasterize()` function converts a `GVector` into a `GRaster`.

## Usage

``` r
# S4 method for class 'GVector,GRaster'
rasterize(x, y, field = "", background = NA, by = NULL, verbose = TRUE)
```

## Arguments

- x:

  A `GVector`.

- y:

  A `GRaster`: The new raster will have the same extent and resolution
  as this raster.

- field:

  Character: Name of a column in the data table of `y` to "burn" into
  the raster. If not `""` (default), then the output will be a
  categorical `GRraster`. If `field` is `""`, then all geometries will
  be "burned" to the raster and have the same value.

- background:

  Numeric or `NA` (default): Value to put in cells that are not covered
  by the `GVector`. Note that if this is not `NA` and not an integer,
  then the output cannot be a categorical raster (i.e., there will be no
  "levels" table associated with it).

- by:

  Either `NULL` (default) or character: If this is not `NULL`, then the
  `GVector` will be subset by the values in the field named by `by`. The
  output will be a multi-layer raster, with one layer per unique value
  in `by`.

- verbose:

  Logical: If `by` is not `NULL`, display progress.

## Value

A `GRaster`.

## See also

[`terra::rasterize()`](https://rspatial.github.io/terra/reference/rasterize.html),
**GRASS** tool `v.to.rast` (see `grassHelp("v.to.rast")`)

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Elevation raster, outline of a part of Madagascar, and rivers vector:
madElev <- fastData("madElev") # raster
madDypsis <- fastData("madDypsis") # points vector
madRivers <- fastData("madRivers") # lines vector
madCoast4 <- fastData("madCoast4") # polygons vector

# Convert to GRaster and GVectors:
elev <- fast(madElev)
dypsis <- fast(madDypsis)
coast4 <- fast(madCoast4)
rivers <- fast(madRivers)

# Convert points, line, and polygons vectors to rasters:
points <- rasterize(dypsis, elev)
plot(points)

lines <- rasterize(rivers, elev)
plot(lines)

polys <- rasterize(coast4, elev)
plot(polys)

communes <- rasterize(coast4, elev, field = "NAME_4")
plot(communes)

# Change background value:
polysNeg1 <- rasterize(coast4, elev, background = -1)
plot(polysNeg1)

# Make one layer per river:
byRiver <- rasterize(rivers, elev, field = "NAM", by = "NAM")
plot(byRiver)

}
```

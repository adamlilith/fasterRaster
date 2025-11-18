# Convert a raster to a lines vector

`as.lines()` converts a `GRaster` to a "lines" `GVector`. Before you
apply this function, you may need to run
[`thinLines()`](https://github.com/adamlilith/fasterRaster/reference/thinLines.md)
on the raster to reduce linear features to a single-cell width. You may
also need to use [clean
geometry](https://github.com/adamlilith/fasterRaster/reference/breakPolys.md)
(especially the
[`removeDups()`](https://github.com/adamlilith/fasterRaster/reference/breakPolys.md)
and
[`removeDangles()`](https://github.com/adamlilith/fasterRaster/reference/breakPolys.md))
afterward to remove duplicated vertices and "dangling" lines.

## Usage

``` r
# S4 method for class 'GRaster'
as.lines(x)
```

## Arguments

- x:

  A `GRaster`. If more than one layer is in the `GRaster`, only the
  first will be used (with a warning).

## Value

A `GVector`.

## See also

[`as.points()`](https://github.com/adamlilith/fasterRaster/reference/as.points.md),
[`as.polygons()`](https://github.com/adamlilith/fasterRaster/reference/as.polygons.md),
[`terra::as.lines()`](https://rspatial.github.io/terra/reference/as.lines.html),
[`thinLines()`](https://github.com/adamlilith/fasterRaster/reference/thinLines.md),
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

# Convert to GRaster:
elev <- fast(madElev)

# Thin elevation raster:
thinned <- thinLines(elev, iter = 300)
plot(thinned)

# Convert to lines:
rastToLines <- as.lines(thinned)
plot(rastToLines)

# We can clean this:
cleanLines <- fixDangles(x = rastToLines)
plot(rastToLines, col = "red")
plot(cleanLines, add = TRUE)

}
```

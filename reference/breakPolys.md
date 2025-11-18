# Fix issues with geometries of a vector

These functions are intended to help fix geometric issues with a
`GVector`. Note that the functionality of the `snap()` and
`removeAreas()` functions can also be implemented when using
[`fast()`](https://github.com/adamlilith/fasterRaster/reference/fast.md)
to create a `GVector`.

- `breakPolys()`: Break topologically clean areas. This is similar to
  `fixLines()`, except that it does not break loops. Topologically clean
  vectors may occur if the vector was imported from a format that does
  not enforce topology, such as a shapefile. Duplicate geometries are
  automatically removed after breaking.

- `fixBridges()`: Change "bridges" to "islands" (which are topologically
  incorrect) within geometries to lines.

- `fixDangles()`: Change "dangles" hanging off boundaries to lines if
  shorter than `tolerance` distance. If `tolerance` is \<0, all dangles
  will be changed to lines. Units of `tolerance` are in map units, or in
  degrees for unprojected CRSs. If `tolerance` \<0, all dangles are
  removed, and the function will retain only closed loops and lines
  connecting loops. Dangles will be removed from longest to shortest.

- `fixLines()`: Break lines at intersections and lines that form closed
  loops.

- `remove0()`: Remove all boundaries and lines with a length of 0.

- `removeAngles()`: Collapse lines that diverge at an angle that is
  computationally equivalent to 0. This tool often needs to be followed
  with the `break()` and `removeDups()` methods.

- `removeBridges()`: Remove "bridges" to "islands" (which are
  topologically incorrect) within geometries.

- `removeDangles()`: Remove "dangling" lines if shorter than `tolerance`
  distance. If `tolerance` is \<0, all dangles will be removed. Units of
  `tolerance` are in map units, or in degrees for unprojected CRSs. If
  `tolerance` \<0, all dangles are removed, and the function will retain
  only closed loops and lines connecting loops. Dangles will be removed
  from longest to shortest.

- `removeDupCentroids()`: Remove duplicated area centroids. In
  **GRASS**, closed polygons have their attributes mapped to a (hidden)
  centroid of the polygon.

- `removeDups()`: Remove duplicated features and area centroids.

- `removeSmallPolys()`: Remove polygons smaller than `tolerance`. Units
  of `tolerance` are in square meters (regardless of the CRS).

- `snap()`: Snap lines/boundaries to each other if they are less than
  `tolerance` apart. Subsequent removal of dangles may be needed. Units
  of `tolerance` are map units, or degrees for unprojected CRSs.

## Usage

``` r
# S4 method for class 'GVector'
breakPolys(x)

# S4 method for class 'GVector'
fixBridges(x)

# S4 method for class 'GVector'
fixDangles(x, tolerance = -1)

# S4 method for class 'GVector'
fixLines(x)

# S4 method for class 'GVector'
remove0(x)

# S4 method for class 'GVector'
removeAngles(x)

# S4 method for class 'GVector'
removeBridges(x)

# S4 method for class 'GVector'
removeDangles(x, tolerance = -1)

# S4 method for class 'GVector'
removeDupCentroids(x)

# S4 method for class 'GVector'
removeDups(x)

# S4 method for class 'GVector'
removeSmallPolys(x, tolerance)

# S4 method for class 'GVector'
snap(x, tolerance)
```

## Arguments

- x:

  A `GVector`.

- tolerance:

  Numeric or `NULL` (default): Minimum distance in map units (degrees
  for unprojected, usually meters for projected) or minimum area (in
  meters-squared, regardless of projection).

## Value

A `GVector`.

## See also

[`terra::topology()`](https://rspatial.github.io/terra/reference/topology.html),
[`fillHoles()`](https://github.com/adamlilith/fasterRaster/reference/fillHoles.md),
[`terra::removeDupNodes()`](https://rspatial.github.io/terra/reference/topology.html),
*Details* section in
[`fast()`](https://github.com/adamlilith/fasterRaster/reference/fast.md),
[`simplifyGeom()`](https://github.com/adamlilith/fasterRaster/reference/simplifyGeom.md),
[`smoothGeom()`](https://github.com/adamlilith/fasterRaster/reference/smoothGeom.md),
**GRASS** manual page for tool `v.clean` (see `grassHelp("v.clean")`)

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Example data
madRivers <- fastData("madRivers")
rivers <- fast(madRivers)
soam <- rivers[rivers$NAM == "SOAMIANINA"] # select one river for illustration

### Simplify geometry (remove nodes)

vr <- simplifyGeom(soam, tolerance = 2000)
dp <- simplifyGeom(soam, tolerance = 2000, method = "dp")
dpr <- simplifyGeom(soam, tolerance = 2000, method = "dpr", prop = 0.5)
rw <- simplifyGeom(soam, tolerance = 2000, method = "rw")

plot(soam, col = "black", lwd = 3)
plot(vr, col = "blue", add = TRUE)
plot(dp, col = "red", add = TRUE)
plot(dpr, col = "chartreuse", add = TRUE)
plot(rw, col = "orange", add = TRUE)

legend("bottom",
   xpd = NA,
   legend = c(
    "Original",
      "Vertex reduction",
      "Douglas-Peucker",
      "Douglas-Peucker reduction",
      "Reumann-Witkam"
  ),
  col = c("black", "blue", "red", "chartreuse", "orange"),
  lwd = c(3, 1, 1, 1, 1)
)

### Smooth geometry

hermite <- smoothGeom(soam, dist = 2000, angle = 3)
chaiken <- smoothGeom(soam, method = "Chaiken", dist = 2000)

plot(soam, col = "black", lwd = 2)
plot(hermite, col = "blue", add = TRUE)
plot(chaiken, col = "red", add = TRUE)

legend("bottom",
   xpd = NA,
   legend = c(
    "Original",
      "Hermite",
      "Chaiken"
  ),
  col = c("black", "blue", "red"),
  lwd = c(2, 1, 1, 1, 1)
)

### Clean geometry

# Has no effect on this vector!
noDangs <- removeDangles(soam, tolerance = 10000)

plot(soam, col = "black", lwd = 2)
plot(noDangs, col = "red", add = TRUE)

legend("bottom",
   xpd = NA,
   legend = c(
    "Original",
      "No dangles"
  ),
  lwd = c(2, 1),
  col = c("black", "red")
)

}
```

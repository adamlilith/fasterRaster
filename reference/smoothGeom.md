# Smooth the geometry of a vector

`smoothGeom()` makes line segments of a vector appear less angular.

## Usage

``` r
# S4 method for class 'GVector'
smoothGeom(x, method = "Hermite", dist = NULL, angle = 3)
```

## Arguments

- x:

  A `GVector`.

- method:

  Character: Method used to smooth line segments. Partial matching is
  used, and case does not matter:

  - `"Hermite"`: Hermite interpolation (default): Guarantees that the
    output vector always passes through the original points. This method
    adds points (possibly many) by constructing cubic splines with
    points approximately `dist` apart. The number of points can be
    reduced by specifying a smaller value of `angle`, which specifies
    the minimum angle between two successive line segments.

  - `"Chaiken"`: Chaiken's algorithm: Guarantees that the new vector
    always touches the midpoint of each original line segment. The
    points on the new line are at least `dist` apart.

- dist:

  Numeric \> 0 of `NULL` (default): Minimum distance (see `method`).
  Units are in map units. If `NULL`, then 2% of the minimum of the x-,
  y-, and z-extent will be used.

- angle:

  Numeric \> 0: Maximum angle for the Hermite algorithm. Default is 3.

## Value

A `GVector`.

## See also

[`simplifyGeom()`](https://github.com/adamlilith/fasterRaster/reference/simplifyGeom.md),
[`terra::simplifyGeom()`](https://rspatial.github.io/terra/reference/simplify.html),
[geometry
cleaning](https://github.com/adamlilith/fasterRaster/reference/breakPolys.md),
**GRASS** manual page for tool `v.generalize` (see
`grassHelp("v.generalize")`)

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

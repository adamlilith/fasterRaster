# Centroid(s) of a vector or clumps in a raster

This function locates the centroid of each geometry of a `GVector`, or
the centroid of each "clump" of same-valued cells in an
integer/categorical raster (for information on types of `GRaster`s, see
[`vignette("GRasters", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/GRasters.md)).

To use this function with a `GVector`, you need the **GRASS**
`v.centerpoint` addon. To use the function with a `GRaster`, you need
the addon `r.centroids`. In either case, the function will try to
install the respective addon (i.e., you need to have an internet
connection). Once installed, a tool will not need to be installed again.

## Usage

``` r
# S4 method for class 'GVector'
centroids(x, method = NULL)

# S4 method for class 'GRaster'
centroids(x)
```

## Arguments

- x:

  A `GVector` or `GRaster`.

- method:

  `GVector`s: Character or `NULL` (default): Method used for calculating
  centroids. The method of calculation depends on whether the input is a
  `points`, `lines`, or `polygons` `GVector`. If the value is `NULL`,
  then the default method will be chosen, depending on the geometry type
  of the `GVector`:

  - `points`:

    - `"mean"` (default for `points`): Mean of coordinates.

    - `"median"`: Geometric median; more robust to outliers.

    - `"pmedian"`: Point in `x` closest to the geometric median.

  - `lines`:

    - `"mid"` (default for `lines`): Mid-point on each line; will fall
      exactly on the line.

    - `"mean"`: Center of gravity of all line segments; may not fall on
      the line.

    - `"median`: Geometric median; may not fall on the line.

  - `polygons`:

    - `"mean"` (default for `polygons`): Center of gravity (area),
      calculated using area triangulation.

    - `"median"`: Geometric mean; may not fall inside the polygon.

    - `"bmedian"`: Geometric mean; minimum distance to boundaries; may
      not fall inside the polygon.

  Partial matching is used and case is ignored.

## Value

If the input is a `GVector`, the output will be a "points" `GVector`. If
the input is a `GRaster`, the output will be a "points" `GVector` with a
table with statistics on each clump. If the input is a `GRaster` with
more than one layer, the output will be a `list` of `GVector`s, with one
`GVector` per layer.

## See also

[`terra::centroids()`](https://rspatial.github.io/terra/reference/centroids.html);
**GRASS** addon tools `v.centerpoint` and `r.centroids`.

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)
library(terra)

### Points, line, and polygon centroids

# Point centroids:
madDypsis <- fastData("madDypsis")
dypsis <- fast(madDypsis)

dypMean <- centroids(dypsis)
dypMedian <- centroids(dypsis, method = "median")
dypPMedian <- centroids(dypsis, method = "pmedian")

plot(dypsis)
plot(dypMean, col = "red", add = TRUE)
plot(dypMedian, col = "green", pch = 2, add = TRUE)
plot(dypPMedian, col = "blue", pch = 3, add = TRUE)
legend("bottomright",
   legend = c("mean", "median", "pmedian"),
   col = c("red", "green", "blue"),
   pch = c(16, 2, 3),
   xpd = NA
)

# Line centroids:
madRivers <- fastData("madRivers")
rivers <- fast(madRivers)

riversMid <- centroids(rivers)
riversMean <- centroids(rivers, method = "mean")
riversMedian <- centroids(rivers, method = "median")

plot(rivers)
plot(riversMid, col = "red", add = TRUE)
plot(riversMean, col = "green", pch = 2, add = TRUE)
plot(riversMedian, col = "blue", pch = 3, add = TRUE)
legend("bottomright",
   legend = c("mid", "mean", "median"),
   col = c("red", "green", "blue"),
   pch = c(16, 2, 3),
   xpd = NA
)

# Polygon centroids:
madCoast4 <- fastData("madCoast4")
coast4 <- fast(madCoast4)

coastMean <- centroids(coast4)
coastMedian <- centroids(coast4, method = "median")
coastBMedian <- centroids(coast4, method = "bmedian")

plot(coast4)
plot(coastMean, col = "red", add = TRUE)
plot(coastMedian, col = "green", pch = 2, add = TRUE)
plot(coastBMedian, col = "blue", pch = 3, add = TRUE)
legend("bottomright",
   legend = c("mean", "median", "bmedian"),
   col = c("red", "green", "blue"),
   pch = c(16, 2, 1),
   xpd = NA
)

### Centroids of integer GRaster "clumps"

# Load elevation raster
madElev <- fastData("madElev")
elev <- fast(madElev)

# Create clumps of similarly-valued cells
clumps <- clump(elev, minDiff = 0.01, minClumpSize = 1000)

# Centroids:
clumpCents <- centroids(clumps)
clumpCents

plot(clumps)
plot(clumpCents, add = TRUE)


}
```

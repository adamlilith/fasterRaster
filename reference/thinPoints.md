# Reduce number of points in same raster cell

This function thins a "points" `GVector` so that it has no more than `n`
points per grid cell in a raster.

## Usage

``` r
# S4 method for class 'GVector,GRaster'
thinPoints(x, y, n = 1)
```

## Arguments

- x:

  A "points" `GVector`.

- y:

  A `GRaster`.

- n:

  Integer or numeric integer: Maximum number of points to remain in a
  cell. The default is 1.

## Value

A "points" `GVector`.

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Elevation and points
madElev <- fastData("madElev")
madDypsis <- fastData("madDypsis")

# Convert to fasterRaster formats:
elev <- fast(madElev)
dypsis <- fast(madDypsis)

# Aggregate cells of the raster so they are bigger
elevAgg <- aggregate(elev, 32)

# Remove all but one or two points per cell
thin1 <- thinPoints(dypsis, elevAgg, n = 1)
thin2 <- thinPoints(dypsis, elevAgg, n = 2)

# Plot
plot(elevAgg)
plot(dypsis, add = TRUE)
plot(thin2, col = "yellow", add = TRUE)
plot(thin1, col = "red", add = TRUE)
legend(
   "bottomright",
   legend = c("In original & thin 1 & 2",
     "In just thin 1 & 2", "In just thin 1"),
   pch = 16,
   col = c("black", "yellow", "red"),
   bg = "white",
   xpd = NA
)

}
```

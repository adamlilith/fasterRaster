# Convert degrees between 'north-orientation' and 'east orientation'

This function converts facing between "north orientation" and "east
orientation".

In "north orientation" systems, a 0-degree facing is north, and the
angle of facing proceeds clockwise. For example, a 90 degree facing
faces east, 180 south, and 270 west. In "east orientation", a 0-degree
facing is east, and the facing angle proceeds counter-clockwise. For
example, 90 is north, 180 is west, and 270 south.

## Usage

``` r
# S4 method for class 'GRaster'
reorient(x, units = "degrees")

# S4 method for class 'numeric'
reorient(x, units = "degrees")
```

## Arguments

- x:

  A numeric vector or a `GRaster` with cell values equal to facing (in
  degrees).

- units:

  Character: "Units" of values in `x`: either `"degrees"` for degrees
  (default) or `"radians"`. Partial matching is used.

## Value

A `GRaster` or numeric vector. Values will be in the range between 0 and
360 and represents facing in the system "opposing" the input's system.
For example, if the input is north orientation, the output will be east
orientation. If the input is in east orientation, the output will be in
north orientation.

## Examples

``` r
### Re-orient numeric values:
facings <- c(0, 90, 180, 270, 360)
reorient(facings)
#> [1]  90   0 270 180  90

# Re-reorienting returns the same values:
reorient(reorient(facings))
#> [1]   0  90 180 270   0

if (grassStarted()) {

### Re-orient a GRaster:

# Setup
library(terra)
madElev <- fastData("madElev")
elev <- fast(madElev)

# Calculate aspect in degrees, using north orientation:
aspectNorth <- terrain(elev, "aspect")

# Re-orient to east-facing:
aspectEast <- reorient(aspectNorth)

# Re-reorienting is the same, to within rounding error:
aspectNorth - reorient(reorient(aspectNorth))

# Plot:
aspects <- c(aspectNorth, aspectEast)
names(aspects) <- c("north_orientation", "east_orientation")
plot(aspects)


}
```

# Slope, aspect, curvature, and partial slopes

`terrain()` calculates topographic indices, including slope, aspect,
curvature, and partial slopes (slopes in the east-west or north-south
directions).

## Usage

``` r
# S4 method for class 'GRaster'
terrain(
  x,
  v = "slope",
  units = "degrees",
  undefinedAspect = NA,
  northIs0 = TRUE
)
```

## Arguments

- x:

  A `GRaster` (typically representing elevation).

- v:

  Name of the topographic metric(s) to calculate. Valid values include
  one or more of:

  - `"slope"`: Slope. Units are given by argument `units`.

  - `"aspect"`: Aspect. When argument `northIs0` is `TRUE` (default),
    then aspect is given in degrees from north going clockwise (0 =
    north, 90 = east, 180 = south, 270 = west). Units are given by
    argument `units`.

  - `"profileCurve"`: Profile curvature.

  - `"tanCurve"`: Tangential curvature.

  - `"dx"`: Slope in east-west direction.

  - `"dy"`: Slope in north-south direction.

  - `"dxx"`: Second partial derivative in east-west direction.

  - `"dyy"`: Second partial derivative in north-south direction.

  - `"dxy"`: Second partial derivative along east-west and north-south
    direction.

  - `"*"`: All of the above.

- units:

  Character: "Units" in which to calculate slope and aspect: either
  `"degrees"` for degrees (default), `"radians"`, or `"percent"`.
  Partial matching is used.

- undefinedAspect:

  Numeric or `NA` (default): Value to assign to flat areas for which
  aspect cannot be calculated.

- northIs0:

  Logical: If `TRUE` (default), aspect will be reported in "north
  orientation," such that 0 is north, and degrees run clockwise (90 is
  east, 180 south, 270 west). If `FALSE`, then aspect will be reported
  in "east orientation," such that 0 is east, and degrees run
  counterclockwise (90 is north, 180 west, 270 south). The latter is the
  default in **GRASS**, but the former is the default in
  [`terra::terrain()`](https://rspatial.github.io/terra/reference/terrain.html)
  function, so is used here as the default. **Note:** The
  [`sun()`](https://github.com/adamlilith/fasterRaster/reference/sun.md)
  function requires aspect to be in east orientation.

## Value

A `GRaster` with one or more layers.

## See also

[`terra::terrain()`](https://rspatial.github.io/terra/reference/terrain.html),
[`ruggedness()`](https://github.com/adamlilith/fasterRaster/reference/ruggedness.md),
[`wetness()`](https://github.com/adamlilith/fasterRaster/reference/wetness.md),
[`geomorphons()`](https://github.com/adamlilith/fasterRaster/reference/geomorphons.md),
tool `r.slope.aspect` in **GRASS**

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster
elev <- fast(madElev)

# Calculate all topographic metrics
topos <- terrain(elev, v = "*")
topos

plot(topos) # NB Aspect has values of NA when it cannot be defined

# Calculate a hillshade raster
hs <- hillshade(elev)
plot(hs)

}
```

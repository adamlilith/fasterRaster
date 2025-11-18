# Topographic wetness index

This function creates a raster map with values equal to the topographic
wetness index (TWI), which is a measure of how much overland water flow
tends to accumulate in or flow away from a location.

## Usage

``` r
# S4 method for class 'GRaster'
wetness(x)
```

## Arguments

- x:

  A `GRaster` (typically representing elevation). The raster must be
  projected (i.e., not in WGS84, NAD83, et cetera).

## Value

A `GRaster`.

## See also

[`terrain()`](https://github.com/adamlilith/fasterRaster/reference/terrain.md),
[`ruggedness()`](https://github.com/adamlilith/fasterRaster/reference/ruggedness.md),
[`geomorphons()`](https://github.com/adamlilith/fasterRaster/reference/geomorphons.md),
**GRASS** manual for tool `r.topidx` (see `grassHelp("r.topidx")`)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Elevation raster
madElev <- fastData("madElev")

# Convert to GRaster:
elev <- fast(madElev)

# Topographic wetness index:
twi <- wetness(elev)
names(twi) <- 'TWI'
plot(c(elev, twi))

# Terrain ruggedness index:
tri <- ruggedness(elev)
tri7 <- ruggedness(elev, size = 7)
triSmooth7 <- ruggedness(elev, size = 7, exponent = 4)

tris <- c(elev, tri, tri7, triSmooth7)
names(tris) <- c("elevation", "TRI in 3x3", "TRI in 7x7", "Smoothed TRIin 7x7")
plot(tris)

}
```

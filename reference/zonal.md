# Statistics on cells of a GRaster stratified by cells of another raster

Function `zonal()` calculates statistics (mean, sum, etc.) on cells of a
`GRaster` by "zones" created by cells of another `GRaster` or `GVector`.

## Usage

``` r
# S4 method for class 'GRaster,ANY'
zonal(x, z, fun = "mean", probs = 0.5)
```

## Arguments

- x:

  A `GRaster` for which to calculate summary statistics.

- z:

  A `GRaster` or `GVector` used to define zones:

  - If `z` is a `GRaster`, then it must be of type `integer` or `factor`
    (see
    [`vignette("GRasters", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/GRasters.md)).
    Zones will be established based on cells that have the same value in
    this raster.

  - If `z` is a `GVector`, zones will be created for each geometry. If
    geometries overlap, then the zonal statistics will be calculated for
    the ones on top. Thus statistics for the zones defined by geometries
    below these may not represent all the cells covered by that
    geometry.

- fun:

  Character vector: Name of the function(s) to summarize `x` with. These
  can include:

  - `"*"`: All of the functions below.

  - `"cv"`: Sample coefficient of variation (expressed as a proportion
    of the mean).

  - `"cvpop"`: Population coefficient of variation (expressed as a
    proportion of the mean).

  - `"max"` and `"min"`: Highest and lowest values across non-`NA`
    cells.

  - `"mean"` (default): Average.

  - `"meanAbs"`: Mean of absolute values.

  - `"median"`: Median.

  - `"quantile"`: Quantile (see also argument `probs`).

  - `"range"`: Range.

  - `"sd"`: Sample standard deviation.

  - `"sdpop"`: Population standard deviation.

  - `"sum"`: Sum.

  - `"var"`: Sample variance.

  - `"varpop"`: Population variance.

- probs:

  Numeric: Quantile at which to calculate `quantile`. Only a single
  value between 0 and 1 is allowed.

## Value

A `data.frame` or `data.table`.

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Elevation SpatRaster:
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

### Calculate zonal statistics using a GRaster as zones

# Generate a "zones" GRaster by dividing raster into areas based on
# high/low elevation.
names(elev) # Use this name in app() formula.
fun <- "= if (madElev <200, 0, if (madElev <400, 1, 2))"
zones <- app(elev, fun = fun)

# Calculate zonal statistics using a raster as zones
zonal(elev, zones, fun = "mean")
zonal(elev, zones, fun = "*") # all statistics

# Calculate zonal statistics on multi-layered GRaster
elev2 <- c(elev, log10(elev))
zonal(elev2, zones, fun = c("mean", "sum", "sdpop"))

### Calculate zonal statistics using a GVector as zones

madCoast4 <- fastData("madCoast4")
coast <- fast(madCoast4)

zonal(elev, z = coast, fun = "mean")

}
```

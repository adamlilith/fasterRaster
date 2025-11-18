# Identify watershed basins and direction and accumulation of flow

The `flow()` function uses a raster representing elevation to compute
other rasters representing:

- Flow accumulation;

- Direction of flow;

- Watershed basins;

- Flooded areas; and/or

- Topographic convergence (log of flow accumulation divided by local
  slope).

More details about the computations can be found at the help page for
the **GRASS** tool `r.terraflow`\] (see `grassHelp("r.terraflow")`)

## Usage

``` r
# S4 method for class 'GRaster'
flow(
  x,
  direction = "multi",
  return = "accumulation",
  dirThreshold = Inf,
  scratchDir = NULL
)
```

## Arguments

- x:

  A `GRaster` with a single layer, typically representing elevation.

- direction:

  Character: Either `"single"` or `"multi"`. This indicates whether a
  single-direction flow or multi-direction flow model is used. The
  default is `"multi"`. Partial matching is used and case is ignored.

- return:

  Character vector: Indicates what rasters to return. Partial matching
  is used and case is ignored. Options include:

  - `"accumulation"` (default): Flow accumulation raster.

  - `"basins"`: Watershed basins

  - `"direction"`: Flow direction

  - `"flooded"`: Flooded areas

  - `"TCI"`: Topographic convergence index

  - `"*"`: All of the above

- dirThreshold:

  Numeric (default is `Inf`): For the multi-direction flow model, this
  indicates the amount of accumulated flow above which the
  single-direction flow rule is used to locate the egress of water from
  a cell. This is the `d8cut` parameter in `r.stream.extract`.

- scratchDir:

  Character or `NULL` (default): Directory in which to store temporary
  files. The **GRASS** tool `r.terraflow` makes a lot of temporary
  files. If this is `NULL`, then a temporary folder in the user's
  working directory will be used (see
  [`getwd()`](https://rdrr.io/r/base/getwd.html)).

## Value

A `GRaster`.

## See also

[`flowPath()`](https://github.com/adamlilith/fasterRaster/reference/flowPath.md),
[`streams()`](https://github.com/adamlilith/fasterRaster/reference/streams.md),
the **GRASS** tool `r.terraflow` (see `grassHelp("r.terraflow")`)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")
elev <- fast(madElev)

# Calculate flow accumulation and watershed basins
water <- flow(elev, return = c("accum", "basins"))
water

elevWater <- c(elev, water)
plot(elevWater)

}
```

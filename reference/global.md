# Summary statistics for GRasters

`global()` calculates a summary statistic across all the cells of a
`GRaster`. It returns a single value for each layer of the raster.

## Usage

``` r
# S4 method for class 'GRaster'
global(x, fun = "mean", probs = seq(0, 1, 0.25), ...)

# S4 method for class 'missing'
global(x, ...)
```

## Arguments

- x:

  A `GRaster` or missing. If missing, then a vector of all of the
  accepted function names is returned.

- fun:

  Character vector: The name of the function(s):

  - `"*"`: All of the functions below.

  - `"cv"`: Sample coefficient of variation (expressed as a proportion
    of the mean).

  - `"cvpop"`: Population coefficient of variation (expressed as a
    proportion of the mean).

  - `"max"` and `"min"`: Highest and lowest values across non-`NA`
    cells. NB:
    [`minmax()`](https://github.com/adamlilith/fasterRaster/reference/minmax.md)
    is faster.

  - `"mean"` (default): Average.

  - `"meanAbs"`: Mean of absolute values.

  - `"median"`: Median.

  - `"quantile"`: Quantile (see also argument `probs`).

  - `"range"`: Range. Note that following
    [`terra::global()`](https://rspatial.github.io/terra/reference/global.html),
    the minimum and maximum are reported, not the actual range.

  - `"sd"`: Sample standard deviation (same as
    [`stats::sd()`](https://rdrr.io/r/stats/sd.html)).

  - `"sdpop"`: Population standard deviation.

  - `"sum"`: Sum.

  - `"var"`: Sample variance (same as
    [`stats::var()`](https://rdrr.io/r/stats/cor.html)).

  - `"varpop"`: Population variance.

- probs:

  Numeric within the range from 0 to 1: Quantile(s) at which to
  calculate `quantile`.

- ...:

  Other arguments (unused).

## Value

If `x` is missing, the function returns a character vector of all
accepted function names. If `x` is a `GRaster`, a data frame with the
specified statistics is returned.

## See also

[`terra::global()`](https://rspatial.github.io/terra/reference/global.html)
and **GRASS** tool `r.univar`

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

# What functions can we use with global()?
global()

# Calculate global statistics:
global(elev, fun = c("mean", "var", "varpop"))
global(elev, "quantile", probs = c(0.25, 0.5, 0.75))

global(elev, "*") # calculate all available functions

}
```

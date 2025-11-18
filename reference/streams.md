# Create stream network

This function estimates the course of streams and rivers from an
elevation raster. It is based on the **GRASS** tool `r.stream.extract`,
where more details can be found (see `grassHelp("r.stream.extract")`)

## Usage

``` r
# S4 method for class 'GRaster'
streams(
  x,
  accumulation = NULL,
  depression = NULL,
  flowThreshold = 1,
  dirThreshold = 1,
  montgomery = 0,
  minLength = 1
)
```

## Arguments

- x:

  A `GRaster` representing elevation.

- accumulation:

  Either `NULL` (default) or a raster representing flow accumulation. If
  not supplied, an accumulation will created internally. You can
  generate an accumulation raster using
  [`flow()`](https://github.com/adamlilith/fasterRaster/reference/flow.md).

- depression:

  Either `NULL` (default) or a `GRaster` representing depressions (areas
  from which streams will not flow out of).

- flowThreshold:

  Numeric \> 0: Minimum threshold for a stream to be generated. The
  default is 1, which is not necessarily a reasonable value.

- dirThreshold:

  Numeric (default is `Inf`): When flow exceeds this threshold, its
  direction is estimated using a single-flow direction algorithm. Below
  this threshold, a multi-direction flow model is used. This is the
  `d8cut` parameter in `r.stream.extract`, and it is only used if
  `accumulation` is `NULL`. The default is 1, which is not necessarily a
  reasonable value.

- montgomery:

  Numeric: The "Montgomery" exponent for slope, multiplied by
  accumulation as per `accumulation * slope^montgomery`. This value is
  then compared to the threshold to determine if it is sufficient. The
  default is 0 (i.e., no slope scaling).

- minLength:

  Numeric: First-order streams less than this length are removed (units
  in cells). Default is 0 (no removal).

## Value

A `GRaster`.

## See also

[`flow()`](https://github.com/adamlilith/fasterRaster/reference/flow.md),
[`flowPath()`](https://github.com/adamlilith/fasterRaster/reference/flowPath.md),
**GRASS** manual for tool `r.stream.extract` (see
`grassHelp("r.stream.extract")`)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster
elev <- fast(madElev)

# Calculate stream channels
streams <- streams(elev)
plot(streams)

}
```

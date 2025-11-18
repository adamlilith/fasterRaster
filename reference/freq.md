# Frequencies of cell values in a raster

`freq()` tabulates the frequency of cell values in a raster. For rasters
where
[`datatype()`](https://github.com/adamlilith/fasterRaster/reference/datatype.md)
is `integer` or `factor`, the frequency of each value or level is
reported. For other rasters, the range of values is divided into bins,
and the number of cells with values in each bin is reported.

## Usage

``` r
# S4 method for class 'GRaster'
freq(x, digits = 3, bins = 100, value = NULL)
```

## Arguments

- x:

  A `GRaster`.

- digits:

  Numeric integer: Number of digits by which to round raster values.
  Ignored for integer and categorical rasters.

- bins:

  Positive numeric integer: Number of bins in which to divide values of
  `numeric` rasters. The default is 100. For `integer` and categorical
  rasters, each value is tallied (i.e., this is ignored).

- value:

  Numeric or `NULL` (default): If numeric, only cells with this value
  will be counted. If `NULL`, all values will be counted.

## Value

A `data.frame` or a named `list` of `data.frame`s, one per layer in `x`.

## See also

[`terra::freq()`](https://rspatial.github.io/terra/reference/freq.html),
tool `r.stats` in **GRASS**

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev") # raster
madCover <- fastData("madCover") # categorical raster

# Convert to GRasters
elev <- fast(madElev) # raster
cover <- fast(madCover) # categorical raster

# Frequencies of integer raster values
f <- freq(elev)
print(f) # have to do this sometimes if output is a data table

# Frequencies of categorical raster values
f <- freq(cover)
print(f) # have to do this sometimes if output is a data table

# Frequencies of given values
f <- freq(elev, value = 1)
print(f) # have to do this sometimes if output is a data table

# When a GRaster has non-integer values, they will be binned:
f <- freq(elev + 0.1, bins = 10)
print(f)

}
```

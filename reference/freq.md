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
[`crossFreq()`](https://github.com/adamlilith/fasterRaster/reference/crossFreq.md),
**GRASS** tool `r.stats` (see `grassHelp("r.stats")`)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev") # raster
madCover <- fastData("madCover") # categorical raster

# Convert to GRasters
elev <- fast(madElev) # integer raster
cover <- fast(madCover) # categorical raster

# Frequencies of integer raster values
f1 <- freq(elev)
print(f1) # have to do this sometimes if output is a data table

# Frequencies of categorical raster values
f2 <- freq(cover)
print(f2) # have to do this sometimes if output is a data table

# Frequencies of given values
f3 <- freq(elev, value = 4)
print(f3) # have to do this sometimes if output is a data table

# When a GRaster has non-integer values, they will be binned:
f4 <- freq(elev + 0.1, bins = 10)
print(f4)

# Calculate cross frequencies between rasters... both need to be integer.
elevWgs84 <- project(elev, cover)
elevClasses <- clump(elevWgs84, minDiff = 0.13) # bin elevations
names(elevClasses) <- 'elevClass'
f5 <- crossFreq(c(elevClasses, cover), na.rm = FALSE)
print(f5) # have to do this sometimes if output is a data table

}
```

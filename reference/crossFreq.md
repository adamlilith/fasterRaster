# Frequencies of combinations of cells across two or more rasters

`crossFreq()` tabulates the number of cells across two or more rasters
for each combination of values in the rasters. Only cells that are not
`NA` across all rasters will be used.

## Usage

``` r
# S4 method for class 'GRaster'
crossFreq(x, na.rm = TRUE, cats = TRUE, verbose = FALSE)
```

## Arguments

- x:

  A stack of integer/categorical `GRaster`s.

- na.rm:

  Logical: If `TRUE` (default), then only cells that are not `NA` across
  all rasters will be used. If `FALSE`, then for each pair of
  `GRaster`s, all cells that are not `NA` in both rasters will be used.

- cats:

  Logical: If `TRUE` (default), then replace the values of categorical
  rasters with their category names in the output.

- verbose:

  Logical: If `TRUE`, display progress messages.

## Value

A `data.frame` or a named `list` of `data.frame`s, one per each pair of
rasters in `x`.

## See also

[`freq()`](https://github.com/adamlilith/fasterRaster/reference/freq.md),
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

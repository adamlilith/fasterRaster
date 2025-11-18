# Aggregate raster cells into larger cells or combine geometries of a vector

When applied to a `GRaster`, `aggregate()` creates a new raster with
cells that are a multiple of the size of the cells of the original
raster. The new cells can be larger or smaller than the original cells
(this function thus emulates both the
[`terra::aggregate()`](https://rspatial.github.io/terra/reference/aggregate.html)
and
[`terra::disagg()`](https://rspatial.github.io/terra/reference/disaggregate.html)
functions.)

When applied to a `GVector`, all geometries are combined into a
"multipart" geometry, in which geometries are treated as if they were a
single unit. Borders between aggregated geometries can be dissolved if
the `dissolve` argument is `TRUE`. If the `GVector` has a data table
associated with it, the output will also have a data table as long as
there is at least one column with values that are all the same. Values
of columns that do not have duplicated values will be converted to `NA`.

## Usage

``` r
# S4 method for class 'GRaster'
aggregate(
  x,
  fact = 2,
  fun = "mean",
  weight = FALSE,
  prob = NULL,
  na.rm = FALSE
)

# S4 method for class 'GVector'
aggregate(x)
```

## Arguments

- x:

  A `GRaster` or `GVector`.

- fact:

  Numeric vector (rasters only): One, two, or three positive values.
  These reflect the size of the new cells as multiples of the size of
  the old cells. If just one value is supplied, this is used for all two
  or three dimensions. If two values are supplied, the first is
  multiplied by the east-west size of cells, and the second north-south
  size of cells (the raster must be 2D). If three values are supplied,
  the third value is used as the multiplier of the vertical dimension of
  cells. Values are calculated using all cells that have their centers
  contained by the target cell.

  Note that unlike
  [`terra::aggregate()`](https://rspatial.github.io/terra/reference/aggregate.html)
  and
  [`terra::disagg()`](https://rspatial.github.io/terra/reference/disaggregate.html),
  these values need not be integers.

- fun:

  Character (rasters only): Name of the function used to aggregate. For
  `GRaster`s, this is the function that summarizes across cells. For
  `GVector`s, this function will be used to calculate new values of
  numeric or integer cells.

  - `mean`: Average (default)

  - `median`: Median

  - `mode`: Most common value

  - `min`: Minimum

  - `max`: Maximum

  - `range`: Difference between maximum and minimum

  - `sum`: Sum

  - `varpop`: Population variance

  - `sdpop`: Population standard deviation

  - `quantile`: Quantile (see argument `prob`)

  - `count`: Number of non-`NA` cell

  - `diversity`: Number of unique values

- weight:

  Logical (rasters only): If `FALSE`, each source cell that has its
  center in the destination cell will be counted equally. If `TRUE`, the
  value of each source will be weighted the proportion of the
  destination cell the source cell covers.

- prob:

  Numeric (rasters only): Quantile at which to calculate `quantile`.

- na.rm:

  Logical (rasters only): If `FALSE` (default), propagate `NA` cells or
  `NA` values.

## Value

A `GRaster` or `GVector`.

## See also

[`stats::aggregate()`](https://rdrr.io/r/stats/aggregate.html),
[`terra::aggregate()`](https://rspatial.github.io/terra/reference/aggregate.html),
[`disagg()`](https://github.com/adamlilith/fasterRaster/reference/disagg.md),
[`terra::disagg()`](https://rspatial.github.io/terra/reference/disaggregate.html)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")
madCoast4 <- fastData("madCoast4")

### aggregating a GRaster

# Convert:
elev <- fast(madElev)

### Aggregate GRaster by same factor in 2 dimensions
# fasterRaster
agg2 <- aggregate(elev, 2, "mean")
agg2

# Compare rasters aggregated by fasterRaster and terra.
# These should be the same.
agg2terra <- aggregate(madElev, 2)

agg2 <- rast(agg2)
agg2 <- extend(agg2, agg2terra)
agg2 - agg2terra # value is ~0

### Aggregate GRaster by a non-integer factor in 2 dimensions
# fasterRaster
agg2.9 <- aggregate(elev, 2.9, "mean")
agg2.9

# terra
agg2.9terra <- aggregate(madElev, 2.9, "mean")
agg2.9terra

# Compare rasters aggregated by fasterRaster and terra.
# These should be different.
res(agg2.9)
res(agg2.9terra) # terra rounds aggregation factor down
2 * res(madElev) # original resolution multiplied by 2

### Aggregate GRaster by different factor in 2 dimensions
agg2x3 <- aggregate(elev, c(2, 3), "mean")
agg2x3

### aggregating a GVector

madCoast4 <- fastData("madCoast4")

# Convert:
coast4 <- fast(madCoast4)

# Aggregate and disaggregate:
aggCoast <- aggregate(coast4)
disaggCoast <- disagg(coast4)

ngeom(coast4)
ngeom(aggCoast)
ngeom(disaggCoast)

# plot
oldpar <- par(mfrow = c(1, 3))
plot(coast4, main = "Original", col = 1:nrow(coast4))
plot(aggCoast, main = "Aggregated", col = 1:nrow(aggCoast))
plot(disaggCoast, main = "Disaggregated", col = 1:nrow(disaggCoast))
par(oldpar)

}
```

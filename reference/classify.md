# Classify GRaster cell values

This function classifies a \`GRaster“ so that cells that have values
within a given range are assigned a new value. The
[`subst()`](https://github.com/adamlilith/fasterRaster/reference/subst.md)
function is a simpler method for replacing specific values or category
levels.

## Usage

``` r
# S4 method for class 'GRaster'
classify(x, rcl, include.lowest = FALSE, right = TRUE, others = NULL)
```

## Arguments

- x:

  A `GRaster`.

- rcl:

  Reclassification system:

  - A single integer: Number of "bins" into which to divide values.
    Arguments `include.lowest` and `right` apply.

  - A vector of numeric values: Breakpoints of bins into which to divide
    values. These will be sorted from lowest to highest before
    classification. Arguments `include.lowest` and `right` apply.

  - A 2-column `matrix`, `data.frame`, or `data.table`: The first column
    provides specific values in `x` to be replaced, and the second
    provides the values they are replaced with. This method is only
    useful for classifying `integer` `GRaster`s. Arguments
    `include.lowest` and `right` are ignored. Cells will be classified
    in the order in which values are listed in the first column.

  - A 3-column `matrix`, `data.frame`, or `data.table`: The first column
    provides the lower value of a bin, the second the upper value, and
    the third the value to assign to the cells in the bin. Arguments
    `include.lowest` and `right` apply. Cells will be classified in the
    order of how intervals are listed (intervals will not be sorted).

- include.lowest, right:

  Logical: These arguments determine how cells that have values exactly
  equal to the lower or upper ends of an interval are classified.

  - `include.lowest = TRUE` and `right = TRUE`: All intervals will be
    "left-open, right-closed" except for the lowest interval, which will
    be "left-closed/right-closed".

  - `include.lowest = FALSE` and `right = FALSE`: Intervals will be
    "left-closed/right-open". Cells with values equal to the highest
    higher boundary will not be reclassified.

  - `include.lowest = TRUE` and `right = FALSE`: All intervals will be
    "left-closed/right-open", except for the highest interval, which
    will be "right-closed/left-closed".

  - `right = NA`: Only useful for classifying `integer` `GRaster`s. All
    intervals are "left-closed/right-closed". This is easier than
    accounting for "open" intervals when dealing with integers. Argument
    `include.lowest` is ignored.

- others:

  Integer or `NULL` (default), or `NA`: Value to assign to cells that do
  not fall into the set intervals. Cells with `NA` values are not
  reclassified. Setting `others` equal to `NULL` or `NA` replaces all
  other values with `NA`. The value will be coerced to an integer value.

## Value

A `GRaster`. The raster will be a categorical `GRaster` if the original
values were continuous (i.e., a single- or double-precision raster), or
of type "integer" if the input was an integer. See
[`vignette("GRasters", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/GRasters.md).

## See also

[`terra::classify()`](https://rspatial.github.io/terra/reference/classify.html),
[`subst()`](https://github.com/adamlilith/fasterRaster/reference/subst.md)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

# Classify using a scalar indicating number of bins
scalar <- classify(elev, 5)
scalar
levels(scalar)

# Classify using a vector, indicating bin break points
vector <- classify(elev, rcl = c(0, 100, 200, 300, 400, 500, 600))
vector
levels(vector)

# Classify using a 2-column matrix (only valid for integer rasters)
rcl <- data.frame(is = c(1:3, 5, 10), becomes = c(100:102, 105, 110))
twoCol <- classify(elev, rcl = rcl)
twoCol

# Classify using a 3-column table
rcl <- data.frame(
   from = c(0, 100, 200, 300, 400, 500),
   to = c(100, 200, 300, 400, 500, 600),
   becomes = c(1, 2, 3, 10, 12, 15)
)
threeCol <- classify(elev, rcl = rcl)
threeCol
levels(threeCol)

# Convert all values outside range to NA (default)
rcl <- c(100, 200, 300)
v1 <- classify(elev, rcl = rcl)
v1
plot(v1)

# Convert all values outside range to -1
rcl <- c(100, 200, 300)
v2 <- classify(elev, rcl = rcl, others = -1)
v2
plot(v2)

### Left-open/right-closed (default)
minmax(elev) # note min/max values
rcl <- c(1, 200, 570)
v3 <- classify(elev, rcl = rcl, others = 10)
levels(v3)
plot(v3)

### Left-closed/right-open
minmax(elev) # note min/max values
rcl <- c(1, 200, 570)
v4 <- classify(elev, rcl = rcl, others = 10, right = FALSE)
levels(v4)

# Left-open except for lowest bin/right-closed
minmax(elev) # note min/max values
rcl <- c(1, 200, 570)
v5 <- classify(elev, rcl = rcl, others = 10, include.lowest = TRUE)
v5 <- droplevels(v5)
levels(v5)

# Left-closed/right-open except for highest bin
minmax(elev) # note min/max values
rcl <- c(1, 200, 570)
v6 <- classify(elev, rcl = rcl, others = 10,
   right = FALSE, include.lowest = TRUE)
v6 <- droplevels(v6)
levels(v6)

}
```

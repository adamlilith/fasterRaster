# Path of water flow across a landscape

This function finds the least-cost pathway from a set of starting points
to the lowest cells accessible from them while, in each step, traversing
"down" slope gradients. It is intended to depict the path a drop of
water would take when flowing across a landscape. For a single starting
point, the defaults settings will produce a raster with cells with
values of 1 along the path. All other cells will be set to `NA`.

## Usage

``` r
# S4 method for class 'GRaster'
flowPath(x, y, return = "ID")
```

## Arguments

- x:

  A `GRaster` with a single layer, typically representing elevation.

- y:

  A "points" `GVector`. The `GVector` must have \<= 1024 points.

- return:

  Character: Indicates the type of values "burned" into the cells of the
  output raster. Case is ignored and partial matching is used, but only
  one option can be selected.

  - "`ID`" (default): Cells in each path are labeled with the index of
    the starting point. A cell in the flow path of the first point will
    have a value of 1, a cell in the flow path of the second point will
    have a value of 2, and so on.

  - "`sequence`": The output raster's cells will start with 1 at the
    source point(s), then accumulate so that the next cell in the flow
    path is 2, the one after that 3, and so on.

  - "`copy`": The cells in the flow path will have the elevation
    raster's values in the cells along the flow path(s).

  - "`accumulation`": Cells in the flow path will accumulate the
    elevation raster's cell values. For example, if the starting cell
    has an elevation of 700 and the next cell in the drainage path has a
    value of 600 and the one after that 500, then the first cell in the
    path will have a value of 700, the next 1300 (= 700 + 600), and the
    third 1800 (= 700 + 600 + 500).

  - A numeric value: All cells in flow paths will be assigned this
    value.

## Value

A `GRaster`.

## See also

[`flow()`](https://github.com/adamlilith/fasterRaster/reference/flow.md),
[`streams()`](https://github.com/adamlilith/fasterRaster/reference/streams.md),
the **GRASS** tool `r.drain` (see `grassHelp("r.drain")`)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")
madCoast4 <- fastData("madCoast4")

# Convert to GRaster and crop to a sub-portion (easier for visualizing)
elev <- fast(madElev)
coast4 <- fast(madCoast4)
ant <- coast4[coast4$NAME_4 == "Antanambe"]
elevAnt <- crop(elev, ant)

# Create a set of random points to serve as starting points:
starts <- spatSample(elevAnt, 10, as.points = TRUE, seed = 2)

# Remove points in water:
starts <- starts[complete.cases(starts)]

# Calculate flow paths and label each by ID:
paths <- flowPath(elevAnt, starts)
paths

plot(elevAnt, legend = FALSE, main = "Flow path for each point")
plot(paths, add = TRUE)
plot(starts, pch = 1, add = TRUE)

# Flow paths with cell values indicating number of cells from each start:
seqs <- flowPath(elevAnt, starts, return = "seq")

plot(elevAnt, legend = FALSE, main = "Sequentially-numbered flow paths")
plot(seqs, add = TRUE)
plot(starts, pch = 1, add = TRUE)

# We can convert flow paths to lines:
seqLines <- as.lines(seqs)
plot(seqLines)
seqLines

}
```

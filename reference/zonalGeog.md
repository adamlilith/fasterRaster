# Geographic statistics for sets of cells with the same values

This function calculates geographic statistics for each set of cells in
an `integer` or `factor` `GRaster`. Statistics include:

- Area

- Perimeter length

- "Compact square" statistic: \\4 \sqrt(area) / perimeter)\\

- "Compact circle" statistic: \\4 \* P / ( 2 \sqrt(\pi \* A))\\ where
  *P* is the perimeter length and *A* the area.

- fractal dimension: \\2 ( log(P) / log(A + 0.001))\\ where *P* is
  perimeter length and *A* is area.

- The average x- and y-coordinates of each zone.

## Usage

``` r
# S4 method for class 'GRaster'
zonalGeog(x, unit = "meters")
```

## Arguments

- x:

  A `GRaster`.

- unit:

  Character: Units of the output. Any of:

  - `"meters"` (default)

  - `"kilometers"` or `"km"`

  - `"miles"` or `"mi"`

  - `"yards"` or `"yd"`

  - `"feet"` or `"ft"`: International foot; 1 foot exactly equal to
    0.3048 meters

  - `"cells"`: Number or cells

  Partial matching is used and case is ignored.

## Value

A list of `data.frame`s or a `data.table`s, one per layer in `x`. Only
layers that are integers or factors have their geographies calculated.
Other layers have `NULL` tables returned.

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data: Elevation and land cover
madElev <- fastData("madElev")
madForest2000 <- fastData("madForest2000")
madCover <- fastData("madCover")

# Convert to GRasters:
elev <- fast(madElev)
forest2000 <- fast(madForest2000)
cover <- fast(madCover)

# Rename
names(elev) <- "elev"
names(forest2000) <- "forest"

# Geometric statistics for an integer raster zoned by elevation:
fun <-
  "= if (elev <400 & forest == 1, 0, if (elev >=400 & forest == 1, 1, null()))"
forestByElev <- app(c(elev, forest2000), fun = fun)
plot(forestByElev, main = "forest < 400 m & >= 400 m")
zonalGeog(forestByElev)

# Geometric statistics for a categorical raster:
zonalGeog(cover)

}
```

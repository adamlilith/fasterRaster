# Geographic distance

This function produces a raster or a matrix of geographic distances,
depending on the input:

**Case 1: Argument `x` is a `GRaster` and `y` is missing:** By default,
this function replaces values in all `NA` cells with the distance
between them and their closest non-`NA` cell. Alternatively, all
non-`NA` cells can have their values replaced by the distance to `NA`
cells. You can also specify which cells (by value) have their values
replaced by distance to other cells.

**Case 2: Argument `x` is a `GRaster` and `y` is a `GVector`:** All
cells in the raster have their value replaced by the distance to the
nearest features in the `GVector`. Alternatively, calculate the distance
from any cell covered by a vector object and the nearest cell *not*
covered by a vector object. Note that the vector is rasterized first.

**Case 3: Argument `x` is a `GVector` and `y` is a `GVector`:** A matrix
of pairwise distances between all features in one versus the other
`GVector` is returned.

**Case 4: Argument `x` is a `GVector` and `y` is missing:** A matrix of
pairwise distances between all features in the `GVector` is returned.

## Usage

``` r
# S4 method for class 'GRaster,missing'
distance(
  x,
  y,
  target = NA,
  fillNA = TRUE,
  unit = "meters",
  method = ifelse(is.lonlat(x), "geodesic", "Euclidean"),
  minDist = NULL,
  maxDist = NULL
)

# S4 method for class 'GRaster,GVector'
distance(
  x,
  y,
  fillNA = TRUE,
  thick = TRUE,
  unit = "meters",
  method = ifelse(is.lonlat(x), "geodesic", "Euclidean"),
  minDist = NULL,
  maxDist = NULL
)

# S4 method for class 'GVector,GVector'
distance(x, y, unit = "meters", minDist = NULL, maxDist = NULL)

# S4 method for class 'GVector,missing'
distance(x, y, unit = "meters", minDist = NULL, maxDist = NULL)
```

## Arguments

- x:

  A `GRaster` or `GVector`.

- y:

  Either missing, or a `GVector`.

- target:

  Numeric: Only applicable for case 1, when `x` is a `GRaster` and `y`
  is missing. If this is `NA` (default), then cells with `NA`s have
  their values replaced with the distance to the nearest non-`NA` cell.
  If this is another value, then cells with these values have their
  values replaced with the distance to any other cell (meaning, both
  `NA` and non-`NA`, except for cells with this value).

- fillNA:

  Logical: Determines which raster cells to fill with distances.

  - Case 1, when `x` is a `GRaster` and `y` is missing: If `TRUE`
    (default), fill values of `NA` cells with distances to non-`NA`
    cells. If `FALSE`, fill non-`NA` cells width distance to `NA` cells.

  - Case 2, when `x` is a `GRaster` and `y` is a `GVector`: If `TRUE`
    (default), then the returned raster will contain the distance from
    the cell to the closest feature in the vector. If `FALSE`, then
    cells covered by the vector will have their values replaced with the
    distance to the nearest cell not covered, and cells that are not
    covered by the vector will have values of 0.

  - Case 3, when `x` is a `GVector` and `y` is a `GVector`: This
    argument is not used in this case.

- unit:

  Character: Units of the output. Any of:

  - `"meters"`, `"metres"`, or `"m"` (default)

  - `"kilometers"` or `"km"`

  - `"miles"` or `"mi"`

  - `"nautical miles"` or `"nmi"`

  - `"yards"` or `"yd"`

  - `"feet"` or `"ft"` – international, 1 foot exactly equals 0.3048
    meters

  Partial matching is used and case is ignored.

- method:

  Character: The type of distance to calculate. Partial matching is used
  and capitalization is ignored. Possible values include:

  - `Euclidean` (default for projected rasters): Euclidean distance.

  - `geodesic` (default for unprojected rasters): Geographic distance.
    If `x` is unprojected (e.g., WGS84 or NAD83), then the `method` must
    be `"geodesic"`.

  - `squared`: Squared Euclidean distance (faster than just Euclidean
    distance but same rank–good for cases where only order matters).

  - `maximum`: Maximum Euclidean distance.

  - `Manhattan`: Manhattan distance (i.e., "taxicab" distance, distance
    along cells going only north-south and east-west and never along a
    diagonal).

- minDist, maxDist:

  Either `NULL` (default) or numeric values: Ignore distances less than
  or greater than these distances.

- thick:

  Logical: Only applicable for case 2, when `x` is a `GRaster` and `y`
  is a `GVector`. If `TRUE` (default), then the vector will be
  represented by "thickened" lines (i.e., any cell that the
  line/boundary touches, not just the ones on the rendering path).

## Value

If `x` is a `GRaster`, then the output is a `GRaster`. If `x` is a
`GVector`, then the output is a numeric vector.

## See also

[`terra::distance()`](https://rspatial.github.io/terra/reference/distance.html);
**GRASS** modules `r.grow.distance` and `v.distance`

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Elevation raster, rivers vector, locations of Dypsis plants
madElev <- fastData("madElev")
madRivers <- fastData("madRivers")
madDypsis <- fastData("madDypsis")

# Convert a SpatRaster to a GRaster, and sf to a GVector
elev <- fast(madElev)
rivers <- fast(madRivers)
dypsis <- fast(madDypsis)

### case 1: GRaster by itself

# Distance between NA cells and nearest non-NA cells
naDist <- distance(elev)
names(naDist) <- "NA Distance"
plot(naDist)

# Distance between non-NA cells and nearest NA cells
nonNaDist <- distance(elev, fillNA = FALSE)
names(nonNaDist) <- "non-NA Distance"
plot(nonNaDist)

# Distance between cells with an elevation of 3 and any other cell that != 3
distFocal3 <- distance(elev, target = 3)
names(distFocal3) <- "Distance from 3"
plot(distFocal3)

# Distance between any cell and cells with a value of 3
distTo3 <- distance(elev, fillNA = FALSE, target = 3)
names(distTo3) <- "Distance to 3"
plot(distTo3)

### Case 2: GRaster and GVector
distToVect <- distance(elev, rivers)

plot(distToVect)
plot(rivers, add = TRUE)

### Case 3: GVector vs GVector
plot(rivers)
plot(dypsis, add = TRUE)

distToRivers <- distance(dypsis, rivers, unit = "yd")
distToPlants <- distance(rivers, dypsis)
distToRivers
distToPlants

### Case 4: GVector vs itself
distToItself <- distance(dypsis)
distToItself

}
```

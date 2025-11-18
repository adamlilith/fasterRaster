# Extract values from a GRaster at locations in a points GVector

`extract()` obtains the values of a `GRaster` or `GVector` associated
with the locations of a set of points. The output depends on the input:

- **Case \#1: `x` is a numeric or integer `GRaster` and `y` is a points
  `GVector`**: Returns values of cells that have points. If `xy` is
  `TRUE`, also returns the coordinates of the points.

- **Case \#2: `x` is a categorical (factor) `GRaster` and `y` is a
  points `GVector`**: Same as case \#1, but if `cats` is `TRUE`, returns
  category labels of cells that have points. If `xy` is `TRUE`, also
  returns the coordinates of the points.

- **Case \#3: `x` is a categorical `GRaster` and `y` is a lines or
  polygons `GVector`**: Returns a summary (e.g., mean, standard
  deviation, etc.) of all cells that overlap the line(s) or polygon(s).

- **Case \#4: `x` is a `GVector` and `y` is a points `GVector`**:
  Returns the data table row associated each point. If `xy` is `TRUE`,
  also returns the coordinates of the points. Note that whenever a
  points `GVector` is allowed for `y`, a `data.frame`, `data.table`,
  `matrix`, or `numeric` values representing points can be used instead.

## Usage

``` r
# S4 method for class 'GRaster,GVector'
extract(
  x,
  y,
  fun = "mean",
  prob = 0.5,
  overlap = TRUE,
  xy = FALSE,
  cats = TRUE,
  verbose = FALSE,
  ...
)

# S4 method for class 'GRaster,data.frame'
extract(x, y, xy = FALSE, cats = TRUE)

# S4 method for class 'GRaster,data.table'
extract(x, y, xy = FALSE, cats = TRUE)

# S4 method for class 'GRaster,matrix'
extract(x, y, xy = FALSE, cats = TRUE)

# S4 method for class 'GRaster,numeric'
extract(x, y, xy = FALSE, cats = TRUE)

# S4 method for class 'GVector,GVector'
extract(x, y, xy = FALSE, verbose = TRUE)

# S4 method for class 'GVector,data.frame'
extract(x, y, xy = FALSE, verbose = TRUE)

# S4 method for class 'GVector,data.table'
extract(x, y, xy = FALSE, verbose = TRUE)

# S4 method for class 'GVector,matrix'
extract(x, y, xy = FALSE, verbose = TRUE)

# S4 method for class 'GVector,numeric'
extract(x, y, xy = FALSE)
```

## Arguments

- x:

  A `GRaster` or `GVector`.

- y:

  A `GVector`, *or* a `data.frame` or `matrix` where the first two
  columns represent longitude and latitude (in that order), *or* a
  two-element numeric vector where the first column represents longitude
  and the second latitude. Values of `x` will be extracted from the
  points in `y`. `GVector`s can be of types points, lines, or polygons.

- fun:

  Character vector: Name(s) of function(s) to apply to values. This is
  used when `x` is a `GRaster` and `y` is a lines or polygons `GVector`.
  The method(s) specified by `fun` will be applied to all cell values
  that overlap with each geometry (i.e., individual cell values will not
  be returned). Valid functions include:

  - `"countNonNA"`: Number of overlapping cells.

  - `"countNA"`: Number of overlapping `NA` cells.

  - `"mean"`: Average.

  - `"min"`: Minimum.

  - `"max"`: Minimum.

  - `"sum"`: Sum.

  - `"range"`: Maximum - minimum.

  - `"sd"`: Sample standard deviation (same as
    [`stats::sd()`](https://rdrr.io/r/stats/sd.html)).

  - `"sdpop"`: Population standard deviation.

  - `"var"`: Sample variance (same as
    [`stats::var()`](https://rdrr.io/r/stats/cor.html)).

  - `"varpop"`: Population variance.

  - `"cv"`: Coefficient of variation.

  - `"cvpop"`: Population coefficient of variation.

  - `"median"`: Median.

  - `"quantile"`: Quantile; you can specify the quantile using the
    `prob` argument.

- prob:

  Numeric in the range from 0 to 1: Quantile which to calculate. The
  value of `prob` will be rounded to the nearest hundredth.

- overlap:

  Logical: If `TRUE` (default), and `y` is a lines or polygons
  `GVector`, then account for potential overlap of geometries when
  extracting. This can be slow, so if you are sure geometries do not
  overlap, you can change this to `FALSE`. This argument is ignored if
  `y` is a points `GVector`.

- xy:

  Logical: If `TRUE` and `y` represents points, also return the
  coordinates of each point. Default is `FALSE.`

- cats:

  Logical (extracting from a raster): If `TRUE` (default) and `x` is a
  categorical `GRaster`, then return the category labels instead of the
  values.

- verbose:

  Logical: If `TRUE`, display progress (will only function when
  extracting from points on a `GRaster` when the number of `GRaster`s is
  large, or when extracting using a "points" `GVector` with lots of
  points).

- ...:

  Arguments to pass to
  [`project()`](https://github.com/adamlilith/fasterRaster/reference/project.md).
  This is used only if extracting from a `GRaster` at locations
  specified by a `GVector`, and they have a different coordinate
  reference system. In this case, users should specify the `wrap`
  argument to
  [`project()`](https://github.com/adamlilith/fasterRaster/reference/project.md).

## Value

A `data.frame` or `data.table`.

## See also

[`terra::extract()`](https://rspatial.github.io/terra/reference/extract.html),
and modules `r.what` and `v.what` in **GRASS**

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Example data: elevation raster and points vector
madElev <- fastData("madElev") # raster
madCover <- fastData("madCover") # categorical raster
madDypsis <- fastData("madDypsis") # points vector
madRivers <- fastData("madRivers") # lines vector
madCoast4 <- fastData("madCoast4") # polygons vector

# Convert to fasterRaster formats:
elev <- fast(madElev) # raster
cover <- fast(madCover) # categorical raster
dypsis <- fast(madDypsis) # points vector
rivers <- fast(madRivers) # lines vector
coast <- fast(madCoast4) # polygons vector

# Get values of elevation at points where Dypsis species are located:
extract(elev, dypsis, xy = TRUE)

# Extract from categorical raster at points:
categories <- extract(cover, dypsis)
categoryValues <- extract(cover, dypsis, cats = FALSE)
categories
categoryValues

# Extract and summarize values on a raster across polygons:
extract(elev, coast, fun = c("sum", "mean", "countNonNA"), overlap = FALSE)

# Extract and summarize values on a raster across lines:
extract(elev, rivers, fun = c("sum", "mean", "countNonNA"), overlap = FALSE)

# Extract from a polygons vector at a points vector:
polysFromPoints <- extract(coast, dypsis, xy = TRUE)
head(polysFromPoints) # first 3 are outside polygons vector, next 3 are inside

}
```

# Increase/decrease the size of a vector or around non-NA cells of a raster

Buffers can be constructed for `GRaster`s or `GVector`s. For rasters,
the `buffer()` function creates a buffer around non-`NA` cells. The
output will be a raster. For vectors, the `buffer()` and `st_buffer()`
functions create a vector polygon larger or smaller than the focal
vector.

## Usage

``` r
# S4 method for class 'GRaster'
buffer(
  x,
  width,
  unit = "meters",
  method = "Euclidean",
  background = 0,
  lowMemory = FALSE
)

# S4 method for class 'GVector'
buffer(x, width, capstyle = "round", dissolve = TRUE)

# S4 method for class 'GVector'
st_buffer(x, dist, endCapStyle = "round", dissolve = FALSE)
```

## Arguments

- x:

  A `GRaster` or `GVector`.

- width:

  Numeric: For rasters – Maximum distance cells must be from focal cells
  to be within the buffer. For rasters, if the buffering unit is
  `"cells`", then to get `n` cell widths, use `n + epsilon`, where
  `epsilon` is a small number (e.g., 0.001). The larger the buffer, this
  smaller this must be to ensure just `n` cells are included.

  For vectors, distance from the object to place the buffer. Negative
  values create "inside" buffers. Units are in the same units as the
  current coordinate reference system (e.g., degrees for WGS84 or NAD83,
  often meters for projected systems).

- unit:

  Character: Rasters – Indicates the units of `width`. Can be one of:

  - `"cells"`: Units are numbers of cells.

  - `"meters"` (default), `"metres"`, or `"m"`

  - `"kilometers"` or `"km"`

  - `"feet"` or `"ft"`

  - `"miles"` or `"mi"`

  - `"nautical miles"` or `"nmi"`

  Partial matching is used and case is ignored.

- method:

  Character: Rasters – Only used if `units` is `"cells"`. Indicates the
  manner in which distances are calculated for adding of cells:

  - `"Euclidean"`: Euclidean distance (default)

  - `"Manhattan"`: "taxi-cab" distance

  - `"maximum"`: Maximum of the north-south and east-west distances
    between points.

  Partial matching is used and case is ignored.

- background:

  Numeric: Rasters – Value to assign to cells that are not `NA` and not
  part of the buffer (default is 0).

- lowMemory:

  Logical: Rasters – Only used if buffering a raster and `units` is not
  `"meters"`. If `FALSE` (default) use faster, memory-intensive
  procedure. If `TRUE` then use the slower, low-memory version. To help
  decide which to use, consider using the low-memory version on a system
  with 1 GB of RAM for a raster larger than about 32000 x 32000 cells,
  or for a system with with 8 GB of RAM a raster larger than about 90000
  x 90000 cells.

- capstyle, endCapStyle:

  Character: Vectors – Style for ending the "cap" of buffers around
  lines. Valid options include `"rounded"`, `"square"`, and "`flat`".

- dissolve:

  Logical (`GVector`s): If `TRUE` (default), dissolve all buffers after
  creation. If `FALSE`, construct a buffer for each geometry. Note that
  overlapping buffers can cause this function to fail because it creates
  a topologically ambiguous polygon. Thus, using `dissolve = TRUE` is
  recommended.

- dist:

  Vectors – Same as `width`.

## Value

A `GRaster` or a `GVector`.

## Details

Note that in some cases, topologically incorrect vectors can be created
when buffering. This can arise when buffers intersect to create
intersections that technically belong to two or more geometries. This
issue can be resolved by dissolving borders between buffered geometries
using `dissolve = TRUE`, but as of now, there is no fix if you do not
want to dissolve geometries. A workaround would be to create a different
`GVector` for each geometry, and then buffer each individually :(.

## See also

[`terra::buffer()`](https://rspatial.github.io/terra/reference/buffer.html),
[`sf::st_buffer()`](https://r-spatial.github.io/sf/reference/geos_unary.html)

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Elevation raster, rivers vector
madElev <- fastData("madElev")
madRivers <- fastData("madRivers")

# Convert a SpatRaster to a GRaster, and sf to a GVector
elev <- fast(madElev)
rivers <- fast(madRivers)

### Buffer a raster by a given distance:
buffByDist <- buffer(elev, width = 2000) # 2000-m buffer
plot(buffByDist, legend = FALSE)
plot(madElev, add = TRUE)

### Buffer a raster by a given number of cells:
buffByCells <- buffer(elev, width = 20.01, unit = "cells") # 20-cell buffer
plot(buffByCells, legend = FALSE)
plot(madElev, add = TRUE)

### Buffer a vector:
buffRivers <- buffer(rivers, width = 2000, dissolve = TRUE) # 2000-m buffer
plot(buffRivers)
plot(st_geometry(madRivers), col = "blue", add = TRUE)

}
```

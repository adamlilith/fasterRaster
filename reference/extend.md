# Add rows and columns around a writeRaster

`extend()` adds cells around a raster, making it larger.

## Usage

``` r
# S4 method for class 'GRaster,numeric'
extend(x, y, fill = NA)

# S4 method for class 'GRaster,SpatRaster'
extend(x, y, snap = "near", fill = NA)

# S4 method for class 'GRaster,SpatVector'
extend(x, y, snap = "near", fill = NA)

# S4 method for class 'GRaster,SpatExtent'
extend(x, y, snap = "near", fill = NA)

# S4 method for class 'GRaster,sf'
extend(x, y, snap = "near", fill = NA)

# S4 method for class 'GRaster,GSpatial'
extend(x, y, snap = "near", fill = NA)
```

## Arguments

- x:

  A `GRaster`.

- y:

  Any of:

  - An object from which an extent can be obtained; i.e., a
    `SpatRaster`, `SpatVector`, `SpatExtent`, `sf` vector, or a
    `GSpatial` object (any of `GRaster`, `GVector`, or `GRegion`). If
    the extent of `x` is "outside" the extent of `y` on any side, the
    side(s) of `x` that are outside will be kept as-is (i.e., the extent
    of `x` will never be shrunk).

  - A single positive integer: Number of rows and columns to add to the
    top, bottom, and sides of the raster.

  - Two integers \>= 0: Number of columns (1st value) to add to the
    sides, and number of rows (2nd value) to add to the top and bottom
    of the raster.

  - Four integers \>= 0: Number of rows and columns to add (left column,
    right column, bottom row, top row).

- fill:

  Numeric: Value to place in the new cells. The default is `NA`.

- snap:

  Character: Method used to align `y` to `x`. Partial matching is used.
  This is only used if `y` is not a set of numbers.

  - `"near"` (default): Round to nearest row/column

  - `"in"`: Round "inward" toward the extent of `x` to nearest
    row/column

  - `"out"`: Round "outward" away from the extent of `x` to the nearest
    row/column.

## Value

A `GRaster`.

## Details

Known issues: When `GRaster`s are saved to disk explicitly using
[`writeRaster()`](https://github.com/adamlilith/fasterRaster/reference/writeRaster.md),
or implicitly using
[`rast()`](https://github.com/adamlilith/fasterRaster/reference/rast.md)
or
[`plot()`](https://github.com/adamlilith/fasterRaster/reference/plot.md),
rows and columns that are entirely `NA` are dropped.

## See also

[`terra::extend()`](https://rspatial.github.io/terra/reference/extend.html)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")
madRivers <- fastData("madRivers")

# Send spatial objects to GRASS:
elev <- fast(madElev)
rivers <- fast(madRivers)

# Extend raster by number of rows/columns:
extended1 <- extend(elev, 10, fill = 900)
extended2 <- extend(elev, c(10, 20), fill = 900)
extended3 <- extend(elev, c(10, 80, 0, 100), fill = 900)
dim(elev)
dim(extended1)
dim(extended2)
dim(extended3)

plot(extended3)

# When exporting a raster, NA rows and columns are removed.
extended4 <- extend(elev, 100, fill=1) # default fill is NA
extended4terra <- rast(extended4)

dim(extended4)
dim(extended4terra)

plot(extended4)

# Extend the raster by another object with a wider extent.

# For tis example, first crop the raster, then extend it.
elevCrop <- crop(elev, rivers)
uncrop <- extend(elevCrop, elev, fill = 900)
plot(uncrop)

}
```

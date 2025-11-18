# Create longitude/latitude rasters

`longlat()` creates two rasters, one with cell values equal to the
longitude of the cell centers, and one with cell values equal to the
latitude of the cell centers.

## Usage

``` r
# S4 method for class 'GRaster'
longlat(x, degrees = TRUE)
```

## Arguments

- x:

  A `GRaster`.

- degrees:

  Logical: If `TRUE` (default), coordinate values of cells will be in
  degrees. If `FALSE`, and `x` is in a projected coordinate reference
  system, values will represent coordinates in map units (usually
  meters). Values will always be in degrees when the coordinate
  reference system is unprojected (e.g., WGS84, NAD83, etc.).

## Value

A `GRaster` stack.

## See also

[`init()`](https://github.com/adamlilith/fasterRaster/reference/init.md)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster
elev <- fast(madElev)

# Create longitude/latitude rasters
ll <- longlat(elev)
ll # note units of cell values!

}
```

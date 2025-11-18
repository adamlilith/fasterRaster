# Area of GRaster cells

`cellArea()` returns a raster will cell values equal to their area. To
get the area of all cells of a raster, see
[`expanse()`](https://github.com/adamlilith/fasterRaster/reference/expanse.md).

## Usage

``` r
# S4 method for class 'GRaster'
cellSize(x, mask = FALSE, lyrs = FALSE, unit = "meters2")
```

## Arguments

- x:

  A `GRaster`.

- mask:

  Logical: If `TRUE`, then cells that are `NA` in `x` are also `NA` in
  the output.

- lyrs:

  Logical:

  - If `lyrs` is `FALSE` (default), then the output has a single layer.
    In this case, if `mask` is `TRUE`, then cells that are `NA` in the
    *first* layer are also `NA` in the output.

  - If `lyrs` is `TRUE`, then the output has the same number of layers
    as `x` if `mask` is also `TRUE`. In this case, cells that area `NA`
    in the input will also be `NA` in the output in the respective
    layer.

- unit:

  Character: Units of area. Partial matching is used, and case is
  ignored. Can be any of:

  - `"meters2"` (default), `"metres2"`, or `"m2"`

  - `"km2"` or `"kilometers2"`

  - `"ha"` or `"hectares"`

  - `"ac"` or `"acres"`

  - `"mi2"` or `"miles2"`

  - `"ft2"` or `"feet2"`

## Value

A `GRaster`.

## See also

[`terra::cellSize()`](https://rspatial.github.io/terra/reference/cellSize.html),
[`expanse()`](https://github.com/adamlilith/fasterRaster/reference/expanse.md),
[`zonalGeog()`](https://github.com/adamlilith/fasterRaster/reference/zonalGeog.md),
[`omnibus::convertUnits()`](https://adamlilith.github.io/omnibus/reference/convertUnits.html)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Elevation raster
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

# Cell size, no masking, single layer
cs1 <- cellSize(elev)
plot(cs1)

# Cell size, with masking, single layer
cs2 <- cellSize(elev, mask = TRUE)
plot(cs2)

# Cell size, no masking, multilayer
elev2 <- c(elev, log(elev - 200))
cs3 <- cellSize(elev2)
plot(cs3)

# Cell size, masking by 1st layer, multilayer (ignores subsequent layers)
cs4 <- cellSize(elev2, mask = TRUE)
plot(cs4)

# Cell size, masking by each layer, multilayer
cs5 <- cellSize(elev2, mask = TRUE, lyrs = TRUE)
plot(cs5)

}
```

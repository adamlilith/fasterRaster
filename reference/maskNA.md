# Mask all non-NA cells or all NA cells

This function work in two ways:

- If `byLayer = TRUE` (default), it will convert all non-`NA` cells in a
  `GRaster` to a single user-defined value, leaving `NA` cells as `NA`.
  Alternatively, it can convert `NA` cells to a user-defined value, and
  all non-`NA` cells to `NA.` Regardless, if the input is a "stack" of
  `GRaster`s, it will return a stack with the same number of raster
  layers.

- If `byLayer = FALSE` and `x` is a "stack" of `GRaster`s,, it will
  return a single `GRaster` layer. This layer will have a user-defined
  value in all cells that had no `NA`s across all rasters, and `NA`s in
  cells where at least one raster had an `NA`. This is useful for
  masking out areas where data are missing in any of the rasters. If
  `invert = TRUE`, it will return a single `GRaster` layer with a
  user-defined value in all cells that had at least one `NA` across all
  rasters, and `NA`s in cells where all rasters had non-`NA` values.

## Usage

``` r
# S4 method for class 'GRaster'
maskNA(x, value = 1, invert = FALSE, retain = FALSE, byLayer = TRUE)
```

## Arguments

- x:

  A `GRaster`.

- value:

  Numeric: Value to which to assign to masked cells. The default is 1.

- invert:

  Logical: If `FALSE` (default), convert non-`NA` cells to `value`, and
  leave `NA` cells as-is. If `TRUE`, convert all `NA` cells to `value`,
  and non-`NA` cells to `NA`.

- retain:

  Logical: If `invert` is `TRUE` and `retain` is `FALSE` (default),
  non-`NA` cells will retain their value. This argument is ignored if
  `invert` is `FALSE`. Ignored if `byLayer = FALSE`.

- byLayer:

  Logical: If `TRUE` (default), implement the masking layer-by-layer. If
  `x` is a stack of raster, it will return a stack with the same number
  of layers. If `FALSE`, return a single raster layer with `NA` in all
  cells that had at least one `NA` across all layers, and `value` in all
  cells that had no `NA`s across all layers. If `invert = TRUE`, it will
  return a single raster layer with `value` in all cells that had at
  least one `NA` across all layers, and `NA` in all cells that had no
  `NA`s across all layers.

## Value

A `GRaster`.

## See also

[`not.na()`](https://github.com/adamlilith/fasterRaster/reference/math.md),
[`app()`](https://github.com/adamlilith/fasterRaster/reference/app.md),
[`mask()`](https://github.com/adamlilith/fasterRaster/reference/mask.md)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

### Mask layer-by-layer

# Elevation raster
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

# Convert non-NA to 1, NA cells remain NA
elevMask <- maskNA(elev)
elevMask
plot(c(elev, elevMask))

# Convert NA to 1, non-NA cells become NA
elevInvertMask <- maskNA(elev, invert = TRUE)
elevInvertMask
plot(c(elev, elevInvertMask))

# Convert NA to 200, non-NA cells keep their values
elevInvertRetain <- maskNA(elev, value = 200, invert = TRUE, retain = TRUE)
elevInvertRetain
plot(c(elev, elevInvertRetain))

### Mask *across* layers

# Load forest raster, which has many NAs
madForest2000 <- fastData("madForest2000")
forest2000 <- fast(madForest2000)

x <- c(elev, forest2000)

maskByLayer <- maskNA(x)
maskAcrossLayers <- maskNA(x, byLayer = FALSE)

maskByLayer
maskAcrossLayers

}
```

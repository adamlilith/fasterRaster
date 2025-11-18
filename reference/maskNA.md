# Mask all non-NA cells or all NA cells

This function converts all non-`NA` cells in a `GRaster` to a single
user-defined value, leaving `NA` cells as `NA`. Alternatively, it can
convert `NA` cells to a user-defined value, and all non-`NA` cells to
`NA.`

## Usage

``` r
# S4 method for class 'GRaster'
maskNA(x, value = 1, invert = FALSE, retain = FALSE)
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
  `invert` is `FALSE`.

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

}
```

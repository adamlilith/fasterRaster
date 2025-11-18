# Interpolate values at points to a GRaster using inverse-distance weighting

This function interpolates values from a set of points to a raster using
inverse distance weighting (IDW).

## Usage

``` r
# S4 method for class 'GVector,GRaster'
interpIDW(x, y, field, nPoints = Inf, power = 2)
```

## Arguments

- x:

  A "points" `GVector`.

- y:

  A `GRaster` to serve as a template for interpolation: Only points in
  `x` that fall inside the extent of the raster will be used for
  interpolation. You can increase the extent of a `GRaster` using
  [`extend()`](https://github.com/adamlilith/fasterRaster/reference/extend.md).

- field:

  Character, integer, or numeric integer: Name or index of the column in
  `x` with values to interpolate. If `NULL` and if `x` is a
  3-dimensional "points" `GVector`, then the interpolation will act on
  the z-coordinate of each point.

- nPoints:

  Integer or numeric integer: Number of nearest points to use for
  interpolation. The default is to use all points (`Inf`).

- power:

  Numeric value \> 0: Power to which to take distance when
  interpolating. The default value is two, so the value of each point
  used for interpolation is \\1 / d^2\\ where *d* is distance.

## Value

A `GRaster`.

## See also

[`terra::interpIDW()`](https://rspatial.github.io/terra/reference/interpIDW.html),
[`interpSplines()`](https://github.com/adamlilith/fasterRaster/reference/interpSplines.md),
[`fillNAs()`](https://github.com/adamlilith/fasterRaster/reference/fillNAs.md),
**GRASS** tool `v.surf.idw` (se `grassHelp("v.surf.idw")`)

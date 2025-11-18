# Change the cell size of a GRaster

`resample()` changes the cell size (resolution) of a `GRaster` using
either another raster as a template or a user-defined resolution. Note
that the extent of the output raster may be expanded to accommodate an
integer number of cells. The function is not guaranteed to recreate the
same output as
[`terra::resample()`](https://rspatial.github.io/terra/reference/resample.html),
even when the same resampling method is used.

## Usage

``` r
# S4 method for class 'GRaster,GRaster'
resample(x, y, method = NULL, fallback = TRUE)

# S4 method for class 'GRaster,numeric'
resample(x, y, method = NULL, fallback = TRUE)
```

## Arguments

- x:

  The `GRaster` to resample.

- y:

  Either a `GRaster` to serve as a template, or a numeric vector with
  two or three values. If a numeric vector, the values represent
  east-west and north-south resolution for 2D rasters, or east-west,
  north-south, and top-bottom resolution for 3D rasters.

- method:

  Character or `NULL`: Method to use to assign values to cells. Partial
  matching is used.

  - `NULL` (default): Automatically choose based on raster properties
    (`near` for categorical or integer rasters, `bilinear` for
    continuous data).

  - `"near"`: Nearest neighbor. Best for categorical data, and often a
    poor choice for continuous data. If
    [`nlevels()`](https://github.com/adamlilith/fasterRaster/reference/nlevels.md)
    is \>0, this method will be used regardless of the value of
    `method`. If you still want to use a different method, coerce the
    raster to a different type using
    [`as.int()`](https://github.com/adamlilith/fasterRaster/reference/as.int.md),
    [`as.float()`](https://github.com/adamlilith/fasterRaster/reference/as.int.md),
    or
    [`as.doub()`](https://github.com/adamlilith/fasterRaster/reference/as.int.md).

  - `"bilinear"`: Bilinear interpolation (default for non-categorical
    data; uses weighted values from 4 cells).

  - `"bicubic"`: Bicubic interpolation (uses weighted values from 16
    cells).

  - `"lanczos"`: Lanczos interpolation (uses weighted values from 25
    cells). Note that methods that use multiple cells will cause the
    focal cell to become `NA` if there is at least one cell with an `NA`
    in the cells it draws from. These `NA` cells can be filled using the
    `fallback` option.

- fallback:

  Logical: If `TRUE` (default), then use "lower" methods to fill in `NA`
  cells when a "higher" method is used. For example, if
  `method = "bicubic"`, `NA` cells will be filled in using the
  `bilinear` method, except when that results in `NA`s, in which case
  the `near` method will be used. Fallback causes fewer cells to revert
  to `NA` values, so can be better at resampling the edges of rasters.
  However, fallback does increase processing time because each "lower"
  method must be applied, then results merged.

## Value

A `GRaster`.

## See also

[`terra::resample()`](https://rspatial.github.io/terra/reference/resample.html),
**GRASS** modules `r.resample` and `r.resamp.interp` (see
`grassHelp("`r.resample`") and `grassHelp("`r.resamp.interp`")\`)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Elevation raster
madElev <- fastData("madElev")
elev <- fast(madElev)

### Resample raster to 120 x 120 m
elev120 <- resample(elev, c(120, 120), method="bilinear")
elev
elev120

### Resample using another raster as a template

template <- aggregate(elev, 4)

nearest <- resample(elev, template, method = "nearest")

bilinear <- resample(elev, template, method = "bilinear")
bilinearNoFB <- resample(elev, template, method = "bilinear", fallback = FALSE)

bicubic <- resample(elev, template, method = "bicubic")
bicubicNoFB <- resample(elev, template, method = "bicubic", fallback = FALSE)

lanczos <- resample(elev, template, method = "lanczos")
lanczosNoFB <- resample(elev, template, method = "lanczos", fallback = FALSE)

# rasters resampled without fallback have fewer non-NA cells
resampled <- c(nearest, bilinear, bilinearNoFB, bicubic, bicubicNoFB, lanczos,
    lanczosNoFB)
names(resampled) <- c("nearest", "bilinear", "bilinearNoFB", "bicubic",
    "bicubicNoFB", "lanczos", "lanczosNoFB")
ones <- resampled * 0 + 1
global(ones, "sum") # number of non-NA cells
global(resampled, c("mean", "sd", "min", "max")) # other statistics

# Compare fallback to no fallback
frLanczos <- rast(lanczos)
frLanczosNoFB <- rast(lanczosNoFB)

plot(frLanczos, col = "red",
    main = "Red: Cells in fallback not non-fallback", legend = FALSE)
plot(frLanczosNoFB, add=TRUE)

# Compare fasterRaster with terra
coarserTerra <- aggregate(madElev, 4)
terraLanczos <- resample(madElev, coarserTerra, method = "lanczos")

frLanczos <- extend(frLanczos, terraLanczos)
frLanczosNoFB <- extend(frLanczosNoFB, terraLanczos)

frLanczos - terraLanczos
frLanczosNoFB - terraLanczos

plot(frLanczos - terraLanczos, main = "Difference")
plot(frLanczosNoFB - terraLanczos, main = "Difference")

plot(terraLanczos, col = "red",
    main = "Red: Cells in terra not in FR", legend = FALSE)
plot(frLanczos, add=TRUE)

plot(frLanczos, col = "red",
    main = "Red: Cells in FR not in terra", legend = FALSE)
plot(terraLanczos, add=TRUE)

}
```

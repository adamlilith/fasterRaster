# Change the coordinate reference system of a GRaster or GVector

`project()` changes the coordinate reference system (CRS) of a `GRaster`
or `GVector`. It has three use cases:

- `x` is a `GRaster` and `y` is a `GRaster`: `x` will be projected to
  the CRS of `y` and resampled to have the same resolution as `y`. If
  argument `align` is `FALSE`, then it will also be cropped to the
  extent of `y`.

- `x` is a `GRaster` and `y` is a `GVector` or a CRS string (typically
  in Well-Known Text format): `x` will be projected to the CRS specified
  by `y` and resampled but not cropped.

- `x` is a `GVector` and `y` is a `GRaster`, `GVector`, or CRS string:
  The vector will be projected to the CRS of `y`.

## Usage

``` r
# S4 method for class 'GRaster'
project(
  x,
  y,
  align = FALSE,
  method = NULL,
  fallback = TRUE,
  res = "fallback",
  wrap = FALSE,
  verbose = FALSE
)

# S4 method for class 'GVector'
project(x, y, wrap = FALSE)
```

## Arguments

- x:

  A `GRaster` or `GVector` to be projected.

- y:

  A character or `GLocation` object (i.e., typically a `GRaster` or
  `GVector`): Used to set the focal `GRaster` or `GVector`'s new CRS
  (and resolution and possibly extent, for `GRaster`s).

- align:

  Logical: If `FALSE` (default), and `x` and `y` are `GRaster`s, then
  the extent of `x` will be cropped to the extent of `y`. If `TRUE`, no
  cropping is performed.

- method:

  Character or `NULL` (for `GRaster`s only): Method to use to conduct
  the transformation (rasters only). Partial matching is used.

  - `NULL` (default): Automatically choose based on raster properties
    (`near` for categorical data, `bilinear` for continuous data).

  - `"near"`: Nearest neighbor. Best for categorical data, and often a
    poor choice for continuous data. If
    [`datatype()`](https://github.com/adamlilith/fasterRaster/reference/datatype.md)
    is `integer`, this method will be used by default.

  - `"bilinear"`: Bilinear interpolation (default for non-categorical
    data; uses weighted values from 4 cells).

  - `"bicubic"`: Bicubic interpolation (uses weighted values from 16
    cells).

  - `"lanczos"`: Lanczos interpolation (uses weighted values from 25
    cells).

  *Note \#1*: If `x` and `y` are `GRaster`s, and `res = "terra"`, then
  the same `method` is used to resample `x` to the resolution of `y`
  before projecting `x`.

  *Note \#2*: Methods that use multiple cells will cause the focal cell
  to become `NA` if there is at least one cell with an `NA` in the cells
  it draws from. These `NA` cells can often be filled using the
  `fallback` argument.

- fallback:

  Logical (for projecting `GRaster`s only): If `TRUE` (default), then
  use "lower" resampling methods to fill in `NA` cells when a "higher"
  resampling method is used. For example, if `method = "bicubic"`, `NA`
  cells will be filled in using the `bilinear` method, except when that
  results in `NA`s, in which case the `near` method will be used.
  Fallback causes fewer cells to revert to `NA` values, so may be better
  at capturing complex "edges" (e.g., coastlines). Fallback does
  increase processing time because each "lower" method must be applied,
  then results merged. Fallback is not used if `method = "near"`.

- res:

  Character (for projecting `GRaster`s only): Method used to set the
  resolution of a `GRaster` in the new CRS. This can be one of three
  options. Partial matching is used and case ignored:

  - `"terra"`: This method creates an output raster that is as close as
    possible in values and resolution to the one that
    [`terra::project()`](https://rspatial.github.io/terra/reference/project.html)
    would create. However, for large rasters (i.e., many cells), this
    can fail because
    [`terra::project()`](https://rspatial.github.io/terra/reference/project.html)
    encounters memory limits (it is used internally to create a
    template). This method resamples the focal raster in its starting
    CRS, then projects it to the destination CRS.

  - `"template"`: This method can only be used if `y` is a `GRaster`.
    The output will have the same resolution as `y` and possibly the
    same extent (depending on the value of `align`). However, unlike the
    `"terra"` method, cell values will not necessarily be as close as
    possible to what
    [`terra::project()`](https://rspatial.github.io/terra/reference/project.html)
    would generate (unless `method = "near"`). Unlike the `"terra"`
    method, this method does not resample the focal raster in its
    starting CRS before projecting. For large rasters it will be faster
    than the `"terra"` method (especially if `"method = "near"`), and it
    should be less likely to fail because of memory limits.

  - Two numeric values: Values for the new resolution (x- and
    y-dimensions).

  - `"center"`: This method locates the centroid of the raster to be
    projected (in the same CRS as the original raster). It then creates
    four points north, south, east, and west of the centroid, each
    spaced one cell's width from the centroid. This set of points is
    then projected to the new CRS. The new cell size in the x-dimension
    will be the average of the distance between the east and west points
    from the centroid, and in the y-dimension the average from the
    centroid to the north and south points.

  - `"fallback"` (default): This applies the `terra` method first, but
    if that fails, then tries `template`, then `center`. This process
    can take a long time for large rasters.

- wrap:

  Logical:

  - `GRaster`s: When projecting rasters that "wrap around" (i.e.,
    whole-world rasters or rasters that have edges that actually circle
    around to meet on the globe), `wrap` should be `TRUE` to avoid
    removing rows and columns from the "edge" of the map. The default is
    `FALSE`.

  - `GVector`s: When projecting vectors that span the international date
    line at 180E/W, `wrap` should be `TRUE` to avoid an issue where the
    coordinates are incorrectly mapped to the range -180 to 180.

- verbose:

  Logical (for projecting `GRaster`s only): If `TRUE`, display progress.
  Default is `FALSE`.

## Value

A `GRaster` or `GVector`.

## Details

When projecting a raster, the "fallback" methods in **GRASS** tool
`r.import` are actually used, even though the `method` argument takes
the strings specifying non-fallback methods. See the manual page for the
`r.import` **GRASS** tool.

## See also

[`terra::project()`](https://rspatial.github.io/terra/reference/project.html),
[`sf::st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.html),
**GRASS** manual pages for modules `r.proj` and `v.proj` (see
`grassHelp("r.proj")` and `grassHelp("v.proj")`)

## Examples

``` r
if (grassStarted()) {

### Setup for all examples

library(sf)
library(terra)

# Climate raster, elevation raster, rivers vector
madElev <- fastData("madElev")
madRivers <- fastData("madRivers")
madChelsa <- fastData("madChelsa")

# Convert objects into fasterRaster formats
chelsa <- fast(madChelsa)
elev <- fast(madElev)
rivers <- fast(madRivers)

### Project raster without resampling
elevWGS84 <- project(elev, crs(chelsa))
elevWGS84

### Project raster and resample to resolution of another raster
elevWGS84Resamp <- project(elev, chelsa)
elevWGS84Resamp

res(elevWGS84)
res(elevWGS84Resamp)
res(chelsa)

### Project vector
riversWGS84 <- project(rivers, chelsa)
riversWGS84
cat(crs(rivers)) # using "cat()" to make it look nice
cat(crs(riversWGS84))

}
```

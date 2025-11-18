# Determine if GRasters and/or GVectors are geographically comparable

`compareGeom()` compares geographic metadata between two or more
`GRaster`s and/or `GVector`s. In many cases, spatial objects must be
comparable for them to "interact" (e.g., conducting arithmetic
operations, masking, etc.).

## Usage

``` r
# S4 method for class 'GRaster,GRaster'
compareGeom(
  x,
  y,
  ...,
  location = TRUE,
  mapset = TRUE,
  topo = TRUE,
  lyrs = FALSE,
  crs = TRUE,
  ext = TRUE,
  zext = TRUE,
  rowcol = TRUE,
  depths = TRUE,
  res = TRUE,
  zres = TRUE,
  stopOnError = TRUE,
  messages = TRUE
)

# S4 method for class 'GVector,GVector'
compareGeom(
  x,
  y,
  ...,
  location = TRUE,
  mapset = TRUE,
  topo = FALSE,
  crs = TRUE,
  ext = FALSE,
  zext = FALSE,
  geometry = FALSE,
  stopOnError = TRUE,
  messages = TRUE
)

# S4 method for class 'GRaster,GVector'
compareGeom(
  x,
  y,
  ...,
  location = TRUE,
  mapset = TRUE,
  topo = FALSE,
  crs = TRUE,
  ext = FALSE,
  zext = FALSE,
  stopOnError = TRUE,
  messages = TRUE
)

# S4 method for class 'GVector,GRaster'
compareGeom(
  x,
  y,
  ...,
  location = TRUE,
  mapset = TRUE,
  topo = FALSE,
  crs = TRUE,
  ext = FALSE,
  zext = FALSE,
  stopOnError = TRUE,
  messages = TRUE
)
```

## Arguments

- x, y, ...:

  `GRaster`s or `GVector`s. If `y` is `GRaster`, then the `...` must
  also be `GRaster`s (or missing). If `y` is `GVector`, then the `...`
  must also be `GVector`s (or missing).

- location, mapset:

  Logical: Compare **GRASS** "project/location" and "mapsets" (see
  [`vignette("projects_mapsets", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/projects_mapsets.md)).
  Default is `TRUE`.

- topo:

  Logical: Test for same topology (2D or 3D). By default, this is `TRUE`
  for raster-raster comparisons, and `FALSE` for all others.

- lyrs:

  Logical (rasters only): Compare number of layers of "stacked" rasters.
  Note this is different from number of vertical "depths" of a raster.
  Default is `FALSE`.

- crs:

  Logical: Compare coordinate reference systems. Default is `TRUE`.

- ext:

  Logical: If `TRUE`, test for same extent. By default, is `TRUE` for
  raster-raster comparison and `FALSE` for all others.

- zext:

  Logical: Test for same vertical extents (3D only). By default, this is
  `TRUE` for raster-raster comparisons, and `FALSE` for all others.

- rowcol:

  Logical (rasters only): Test for same number of rows and columns.
  Default is `TRUE`.

- depths:

  Logical (rasters only): Test for same number of depths. Default is
  `TRUE`.

- res:

  Logical (rasters only): Test for same resolution in x- and
  y-dimensions. Default is `TRUE`.

- zres:

  Logical (rasters only): Test for same resolution in z dimension.
  Default is `TRUE`.

- stopOnError:

  Logical: If `TRUE` (default), throw an error with an explanation if
  the objects are not comparable. If `FALSE` (default), return `TRUE` or
  `FALSE`.

- messages:

  Logical: If `TRUE` (default), display a warning if a condition is not
  met. This only comes into effect if `stopOnError` is `FALSE`.

- geometry:

  Logical (vector-vector comparison only): Compare geometry. Default is
  `FALSE`.

## Value

Logical (invisibly): `TRUE` for no mismatches detected, `FALSE` for
incompatibility), or side effect of throwing an error.

# Connect to "GRASS"

This function initializes a **GRASS** "project" (previously known in
**GRASS** as a "location"; see
[`vignette("projects_mapsets", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/projects_mapsets.md)).
You need to run this function (often just once) before you use most
functions in **fasterRaster**. This function is of use to developers,
not most users.

## Usage

``` r
# S4 method for class 'character'
.locationCreate(x, location = NULL, overwrite = FALSE, warn = TRUE)

# S4 method for class 'SpatRaster'
.locationCreate(x, location = NULL, overwrite = FALSE, warn = TRUE)

# S4 method for class 'SpatVector'
.locationCreate(x, location = NULL, overwrite = FALSE, warn = TRUE)

# S4 method for class 'sf'
.locationCreate(x, location = NULL, overwrite = FALSE, warn = TRUE)
```

## Arguments

- x:

  Any object from which a coordinate reference system (CRS) can be
  acquired. Ergo, any of:

  - A `SpatRaster`, `SpatVector`, `SpatExtent`, `stars`, or `sf` object

  - A `crs` object (i.e., from
    [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)).

  - A CRS (coordinate reference system) WKT string. Some PROJ4 strings
    *might* work, too.

- location:

  Character or `NULL` (default): Name of the location.

- overwrite:

  Logical: If `FALSE` (default), and a **GRASS** "coordinate reference
  frame" with the given name has already been created, then the function
  will fail. If `TRUE`, then the existing **GRASS** "coordinate
  reference frame" of the same name will be overwritten. *NOTE*: This
  will **not** remove any **R** objects associated with rasters or
  vectors in the "location", but they will no longer work because the
  objects they point to will be overwritten.

- warn:

  Logical: If `TRUE` (default) and `overwrite` is `TRUE`, then display a
  warning.

## Value

A
[GLocation](https://github.com/adamlilith/fasterRaster/reference/GLocation.md)
object (invisibly).

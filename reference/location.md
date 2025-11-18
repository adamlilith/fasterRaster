# GRASS "location" of an object or the active session

**GRASS** "projects" or "locations" are sets of one or more rasters
and/or vectors with the same coordinate reference systems, and may or
may not represent the same actual location on Earth. **GRASS** "mapsets"
are like sub-folders of locations, and are collections of rasters and/or
vectors typically related to the same general project. Typical users
will not need to make changes to the default location (called
"location") or mapset (called "PERMANENT"). See
[`vignette("projects_mapsets", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/projects_mapsets.md).

This function deletes a **GRASS** "project"/"location", rasters, and
vectors therein. This function should be used cautiously and is mainly
of use to developers. See
[`vignette("projects_mapsets", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/projects_mapsets.md).

## Usage

``` r
# S4 method for class 'GLocation'
.location(x)

# S4 method for class 'missing'
.location(x)

.locationDelete(location, mapset = NULL, workDir = NULL)
```

## Arguments

- x:

  Either:

  - Missing: Reports location of currently active project/location.

  - A `GLocation` object or an object that contains the `GLocation`
    class (i.e., a `GSpatial` object: a `GRaster` or `GVector`).

- location:

  Character: Name of the **GRASS** "location".

- mapset:

  Character or `NULL` (default): Name of the mapset to delete. If
  `NULL`, then all mapsets in the given "location" will be deleted.

- workDir:

  Character: Either `NULL` (default) or a character string of the
  directory in which the location to be removed resides. If `NULL`, then
  the working directory will be obtained from `faster("workDir")`.

## Value

A character string.

If successful, `TRUE` (invisibly). If not, then `FALSE` (also
invisibly), plus a warning.

## See also

[`.mapset()`](https://github.com/adamlilith/fasterRaster/reference/mapset.md)

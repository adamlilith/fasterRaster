# GRASS "mapset" of an object or the active session

**GRASS** "locations" are sets of one or more rasters and/or vectors
with the same coordinate reference systems, and may or may not represent
the same actual location on Earth. **GRASS** "mapsets" are like
subfolders of locations, and are collections of rasters and/or vectors
typically related to the same general project. This function returns the
mapset of an object or the current mapset. This said, **fasterRaster**
always uses the "PERMANENT" mapset, so there is very little reason to
use this function as-is. See
[`vignette("projects_mapsets", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/projects_mapsets.md).

## Usage

``` r
# S4 method for class 'GLocation'
.mapset(x)

# S4 method for class 'missing'
.mapset(x)
```

## Arguments

- x:

  A Either:

  - Missing: Reports mapset of currently active **GRASS**
    "projects"/"locations".

  - A `GLocation` object or an object that contains the `GLocation`
    class (i.e., a `GSpatial` object: a `GRaster` or `GVector`): Reports
    the CRS of the object.

## Value

A character string.

## See also

**GRASS** [locations and
mapsets](https://grass.osgeo.org/grass82/manuals/grass_database.html)

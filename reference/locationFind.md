# Match CRS of a GSpatial object and an existing "GRASS" location

The function searches the set of available **GRASS** "projects"
(previously known as "locations") for one that has a coordinate
reference system matching a `GSpatial` object. If none are found, or if
no connection with **GRASS** has yet been made, then it returns `NULL`.
Otherwise, it returns either the index or the name of the matching
location.

## Usage

``` r
# S4 method for class 'missing'
.locationFind(x, return = "name")

# S4 method for class 'GLocation'
.locationFind(x, return = "name", match = "name")

# S4 method for class 'SpatRaster'
.locationFind(x, return = "name", match = "name")

# S4 method for class 'SpatVector'
.locationFind(x, return = "name", match = "name")

# S4 method for class 'sf'
.locationFind(x, return = "name", match = "name")

# S4 method for class 'character'
.locationFind(x, return = "name", match = "name")
```

## Arguments

- x:

  Either:

  - Missing: Returns names and coordinate reference system strings of
    all "locations".

  - A character representing a coordinate reference system in WKT format

  - A `SpatRaster`, `SpatVector`, or `sf` vector with a coordinate
    reference system

  - A `GSpatial` object (usually a `GRaster` or `GVector`)

- return:

  Either:

  - `"name"` (default): Returns the name of the "location" with a
    coordinate reference system the same as `x`.

  - `"index"`: Returns the index of this "location" in
    `.fasterRaster$locations` of the `.fasterRaster` environment.

  - `"crs"`: Returns the coordinate reference system of this
    "project/location".

- match:

  Character: Method used to find the location. If `match` is "`name`""
  (default), then the name of the location is used. If `match` is
  "`crs`", then the coordinate reference system of each location is
  checked for a match.

## Value

Character, integer, or `NULL` (if no match is found).

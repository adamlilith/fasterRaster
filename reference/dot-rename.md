# Rename a raster or vector in an existing GRASS session

Rename a raster or vector in an existing **GRASS** session.

## Usage

``` r
.rename(from, to, rastOrVect = NULL)
```

## Arguments

- from, to:

  `sources` of the raster or vector to rename.

- rastOrVect:

  Either `NULL` (default), `"raster"`, or `"vector"`. This specifies the
  type of object to be renamed. Partial matching is allowed. If left as
  `NULL` (default), the function will try to identify if the object is a
  raster or vector, and return an error if there is both a raster and
  vector of given name. Note that unlike in **R**, **GRASS** can have
  rasters and vector"s with the same name.

## Value

The function invisibly returns `TRUE` if the desired rasters and/or
vectors were named, and `FALSE` if raster and/or vector to be renamed
did not exist in the `GRASS` session. Notably, a raster or vector or
both are renamed in an existing `GRASS` session.

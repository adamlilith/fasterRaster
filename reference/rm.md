# Delete objects in the active GRASS session

Delete the names of all rasters and/or vectors that have been exported
to or created in the active **GRASS** session's location and mapset.

## Usage

``` r
.rm(x, type = NULL, warn = TRUE, verify = TRUE)
```

## Arguments

- x:

  Character, a `GSpatial` object, or missing (default):

  - Character:

    - Any of `"rasters"` (all rasters), `"vectors"` (all spatial
      vectors), `"rasters3d"` (3D-rasters), and/or `"groups"` (groups),
      and all of these types will be deleted. Partial matching is
      supported.

    - The `sources` of the object to be deleted. Argument `type` must be
      specified.

  - `GSpatial` object (i.e., a `GRaster` or `GVector`): Delete this
    object.

  - Missing: Delete everything in the active **GRASS** session.

- type:

  The type of spatial objects to delete. This can include `"rasters"`
  (all rasters), `"vectors"` (all spatial vectors), `"rasters3d"`
  (3D-rasters), and/or `"groups"` (groups). Partial matching is
  supported. If missing, all objects are candidates for deletion if they
  match `x`.

- warn:

  Logical: If `TRUE` (default), display warning if no matches or if
  everything in **GRASS** is to be deleted.

- verify:

  Logical: If `TRUE` (default), the function will search for the item(s)
  to be deleted first to verify they exist. If this is `FALSE`, then `x`
  MUST be specified and `type` must be '`raster`' or `'vector'` (one
  value per value in `x`). This has no effect if `x` is a `GSpatial`
  object. It's main use is to save a bit of time.

## Value

`TRUE` (invisibly).

## See also

[`rm()`](https://rdrr.io/r/base/rm.html)

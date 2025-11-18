# Remove rasters and vectors from the GRASS cache

**fasterRaster** functions attempt to delete rasters and vectors in the
**GRASS** cache, but not all intermediate files can be removed. This
function can be used to clear the cache of extraneous rasters and
vectors.

Calling this function inside another function's environment and defining
`x` as `"*"` can be very **dangerous**, as it will detect objects
outside of that environment, and thus delete any rasters/vectors outside
that environment. Here is a guide:

- To delete files associated with a single `GRaster` or `GVector`, use
  `mow(GRaster_to_unlink)` or `mow(GVector_to_unlink)`.

- To delete files associated with more than one `GRaster`s and/or
  `GVector`s, provide them as a list. For example, use
  `mow(list(GRaster_to_unlink, GVector_to_unlink))`.

- To remove all rasters, all vectors, or all rasters and vectors in the
  **GRASS** cache that are not linked to a `GRaster` or `GVector`, use
  `mow("*")`.

- To remove all rasters or all vectors in the **GRASS** cache, use
  `mow("*", type = "rasters")` or `mow("*", type = "vectors")`.

- To remove all rasters or all vectors in the **GRASS** cache *except*
  for certain ones, use
  `mow("*", keep = list(GRaster_to_keep, GVector_to_keep))`. You can
  combine this with the `keep` argument to retain specific rasters or
  vectors. For example, you can use
  `mow("*", type = "rasters", keep = list(GRaster_to_keep))`.

## Usage

``` r
mow(
  x = "unlinked",
  pos = NULL,
  type = NULL,
  keep = NULL,
  verbose = TRUE,
  ask = TRUE
)
```

## Arguments

- x:

  Any of:

  - `"unlinked"` (default): Delete **GRASS** rasters and/or vectors that
    are unlinked to `GRaster`s or `GVector`s in the environment in which
    the function was called, or the environment named in `pos`.

  - A `GRaster` or `GVector`: Delete the **GRASS** raster or vector
    pointed to by this object.

  - A `list` of `GRaster`s and/or `GVector`s: Delete the **GRASS**
    raster(s) and/or vector(s) pointed to by these objects.

  - `"*"`: Delete *all* **GRASS** rasters and/or vectors pointed to by
    objects in the environment named in `pos`. Only objects in `keep`
    will not be deleted.

- pos:

  Either `NULL` (default), or an environment. This is used only if `x`
  is `"unlinked"` or `"*"`. In that case, if `pos` is `NULL`, the
  environment in which this function was called will be searched for
  `GRaster`s and `GVector`s for removal of their associated **GRASS**
  rasters and vectors. Otherwise, the named environment will be
  searched.

- type:

  Either `NULL` or a character vector. This is used only if `x` is
  `"unlinked"` or `"*"`. If `NULL`, all rasters and vectors in the
  **GRASS** cache are candidates for deletion. Otherwise, this can be
  either `"rasters"`, `"vectors"`, or both.

- keep:

  Either `NULL` (default) or a
  [`list()`](https://rdrr.io/r/base/list.html) of `GRaster`s and/or
  `GVector`s that you want to retain. This is used only if `x` is
  `"unlinked"` or `"*"`. The rasters and vectors in **GRASS** pointed to
  by these objects will not be deleted.

- verbose:

  Logical: If `TRUE` (default), report progress.

- ask:

  Logical: If `TRUE` (default), prompt for reassurance. This is used
  only if `x` is `"unlinked"` or `"*"`.

## Value

Invisibly returns a named vector with the number of rasters and vectors
deleted.

## See also

[`terra::tmpFiles()`](https://rspatial.github.io/terra/reference/tmpFile.html)

## Examples

``` r
if (grassStarted()) {

# Setup
madElev <- fastData("madElev")
elev <- fast(madElev)

mow(elev, ask = TRUE) # delete GRASS raster attached to `elev`

}
```

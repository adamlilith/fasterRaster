# Report or change the extent, dimensions, and/or resolution of a region GRASS

These functions either change the extent, dimensions, and/or resolution
of a **GRASS** "region" or report the current region's extent,
dimensions, and/or resolution (see
[`vignette("regions", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/regions.md)).
These functions are mostly used internally and rarely of interest to
most users.

- `.region()`: All 2D and 3D aspects of a region.

- `.regionDim()`: x- and y-dimensions.

- `.regionExt()`: x- and y-extent.

- `.regionRes()`: x- and y-resolution.

## Usage

``` r
# S4 method for class 'missing'
.region(x)

# S4 method for class 'SpatRaster'
.region(x)

# S4 method for class 'GRegion'
.region(x)

# S4 method for class 'GRaster'
.region(x, trim = NULL)

# S4 method for class 'GVector'
.region(x)

# S4 method for class 'missing'
.regionExt(x)

# S4 method for class 'numeric'
.regionExt(x, respect)

# S4 method for class 'GSpatial'
.regionExt(x, respect)

# S4 method for class 'missing'
.regionDim(x)

# S4 method for class 'numeric'
.regionDim(x, respect)

# S4 method for class 'GRegion'
.regionDim(x, respect)

# S4 method for class 'missing'
.regionRes(x)

# S4 method for class 'numeric'
.regionRes(x, respect)

# S4 method for class 'GRegion'
.regionRes(x, respect)
```

## Arguments

- x:

  Any of:

  - Missing (default): Reports the extent, resolution, and dimensions of
    the current region. All other arguments will be ignored. You can
    also use
    [`ext()`](https://github.com/adamlilith/fasterRaster/reference/ext.md),
    [`dim()`](https://github.com/adamlilith/fasterRaster/reference/dim.md),
    and
    [`res()`](https://github.com/adamlilith/fasterRaster/reference/res.md)
    and related functions with missing arguments.

  - A `GSpatial`, `GRegion`, `GRaster`, `GVector` object: Sets the
    region"s extent, dimensions, and/or resolution to those of the
    object.

  - A `numeric` vector. This will resize the region's extent, resample
    the region's resolution/dimensions, or both, to ensure the desired
    dimensions or resolution are retained: \* 2 values for
    `.regionDim()`: Number of rows and columns \* 4 values for
    `.regionExt()`: Westernmost and easternmost easting (longitude), and
    southernmost and northernmost northing (latitude) \* 2 values for
    `.regionRes()`: Size of cells in the x- and y-dimensions

- trim:

  A `GRaster` or `NULL` (default). If a `GRaster`, then the region will
  be trimmed to the non-`NA` cells in this raster. `trim` can only be
  non-`NULL` if `x` is a `GRaster`. Ignored if `NULL`.

- respect:

  Character or `GRaster`: Indicates what aspect(s) of the current region
  to retain. Different functions allow for a different aspect to be
  retained. Partial matching is used.

  - `.regionDim()`: `"extent"` (extent unchanged, resolution may be
    changed) or `"resolution"` (resolution unchanged, extent may be
    changed).

  - `.regionExt()`: `"dimensions"` (dimensions unchanged, extent may be
    changed) or `"resolution"` (resolution unchanged, extent may be
    changed).

  - `.regionRes()`: `"extent"` (extent may be changed and/or dimensions
    may be changed to accommodate desired cell size) or `"dimensions"`
    (extent may be changed, dimensions unchanged). Alternatively, a
    `GRaster` can be supplied:

  - `.regionDim()`: New region will have same extent and resolution.

  - `.regionExt()`: New region will have same dimensions and resolution.

  - `.regionRes()`: New region will have same extent and dimensions.

  In this case, the new region"s registration will be the same as this
  raster, and cell resolution will be the same

  Note: In most cases extent cannot be retained exactly if the
  resolution is changed. When resolution is changed, the actual extent
  will be the user-supplied extent expanded by zero to one rows or zero
  to one columns to accommodate an integer number of cells of the
  desired size. The western and northern limits of the extent will be
  retained, while the eastern and southern limits of the extent will be
  moved to accommodate an integer number of columns and rows.

## Value

The value returned depends on how the function is used:

- If used with no arguments, `.region()` returns a `GRegion` object.

- If used with no arguments, `.regionDim()`, `.regionExt()`, and
  `.regionRes()` return numeric or integer vectors.

- If the function is used to change reshape/resample the region, it
  returns a `GRegion` object reflecting the region *before* it was
  changed. This allows users to revert to the original region if
  desired.

## Details

When resizing extent, **terra** keeps the `xmin` (west) and `ymax`
(north) the fixed and shifts `xmax` (east) and `ymin` (south) as needed.
To retain as much fidelity between **fasterRaster** and **terra** as
possible, these functions do the same to the region.

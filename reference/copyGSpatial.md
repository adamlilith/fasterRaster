# Make a copy of an object in GRASS

Create a copy of a `GRaster` or `GVector` in **GRASS**. This function is
used internally and is of little use to most users. This only creates a
copy of the object in the **GRASS** session–to make a `GRaster` or
`GVector`,
[`makeGRaster()`](https://github.com/adamlilith/fasterRaster/reference/makeGRaster.md)
or
[`makeGVector()`](https://github.com/adamlilith/fasterRaster/reference/makeGVector.md)
need to be called after making the copy. Note that if the object is
multi-layered, then a copy is made of each layer.

## Usage

``` r
# S4 method for class 'GRaster'
.copyGSpatial(x, reshapeRegion = TRUE)

# S4 method for class 'GVector'
.copyGSpatial(x)

# S4 method for class 'character'
.copyGSpatial(x, type = NULL, topo = NULL, reshapeRegion = TRUE)

.copyGRaster(x, topo = "2D", reshapeRegion = TRUE)

.copyGVector(x)
```

## Arguments

- x:

  A `GVector` or the
  [`sources()`](https://github.com/adamlilith/fasterRaster/reference/sources.md)
  name of one.

- reshapeRegion:

  Logical. If `TRUE`, `x` must be a `GRaster`.

- type:

  Character or `NULL` (default): Either "raster" or "vector". If a
  character, there must be one per value in `x`. If `NULL`, will attempt
  to auto-detect (takes longer).

- topo:

  "2D" or "3D"

## Value

Character vector representing the
[`sources()`](https://github.com/adamlilith/fasterRaster/reference/sources.md)
names of the copied object(s), plus makes a copy of the given object(s)
in **GRASS**.

[`sources()`](https://github.com/adamlilith/fasterRaster/reference/sources.md)
names of copied rasters.

# Does the "GRASS" representation of a GRaster or GVector exist?

`GRaster`s and `GVector`s are **R** objects that contain a pointer to a
raster or vector in **GRASS**. Thus, for a `GRaster` or `GVector` to be
functional, the **GRASS** file must exist. This function indicates if
that is so.

## Usage

``` r
# S4 method for class 'GRaster'
.exists(x)

# S4 method for class 'GVector'
.exists(x)

# S4 method for class 'character'
.exists(x)
```

## Arguments

- x:

  A `GRaster`, `GVector`, or the
  [`sources()`](https://github.com/adamlilith/fasterRaster/reference/sources.md)
  name of one.

## Value

Logical.

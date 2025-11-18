# "Stack" GRasters

`GRaster`s can be "stacked" using this function, effectively creating a
multi-layered raster. This is different from creating a 3-dimensional
raster, though such an effect can be emulated using stacking. `GVector`s
can be combined into a single vector. Stacks can only be created when:

- All objects are the same class (either all `GRaster`s or all
  `GVector`s).

- All objects have the same coordinate reference system (see crs()).

- Horizontal extents are the same (see
  [`ext()`](https://github.com/adamlilith/fasterRaster/reference/ext.md)).

- Horizontal dimensions are the same (see
  [`res()`](https://github.com/adamlilith/fasterRaster/reference/res.md)).

- The topology (2- or 3-dimensional) must be the same. If 3D, then all
  rasters must have the same number of depths and vertical extents (see
  [`topology()`](https://github.com/adamlilith/fasterRaster/reference/topology-GSpatial-method.md)).

Data tables associated with `GVector`s will be combined if each vector
has a table and if each table has the same columns and data types.
Otherwise, the data table will be combined using
[`merge()`](https://github.com/adamlilith/fasterRaster/reference/merge.md).

## Usage

``` r
# S4 method for class 'GRaster'
c(x, ...)
```

## Arguments

- x:

  A `GRaster` or a `GVector`.

- ...:

  One or more `GRaster`s, one or more `GVector`s, a list of `GRaster`s,
  or a list of `GVector`s. You can use a mix of lists and individual
  rasters or vectors.

## Value

A `GRaster`.

## See also

add\<-,
[`terra::c()`](https://rspatial.github.io/terra/reference/c.html),
`add<-`

## Examples

``` r
if (grassStarted()) {

# Setup
madForest2000 <- fastData("madForest2000")
madForest2014 <- fastData("madForest2014")

# Convert SpatRasters to GRasters:
forest2000 <- fast(madForest2000)
forest2014 <- fast(madForest2014)

# Combine:
forest <- c(forest2000, forest2014)
forest

nlyr(forest)

}
```

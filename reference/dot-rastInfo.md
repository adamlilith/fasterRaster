# Metadata on rasters and vectors in GRASS

`.rastInfo()` and
[`.vectInfo()`](https://github.com/adamlilith/fasterRaster/reference/dot-vectInfo.md)
fetch metadata on rasters and vectors in **GRASS**. The
[`print()`](https://github.com/adamlilith/fasterRaster/reference/show.md),
[`show()`](https://github.com/adamlilith/fasterRaster/reference/show.md),
and
[`summary()`](https://github.com/adamlilith/fasterRaster/reference/show.md)
functions can be used to display this metadata.

## Usage

``` r
.rastInfo(x)
```

## Arguments

- x:

  A `GRaster`, `GVector`, or `sources`.

## Value

Metadata on the extent, dimensions, resolution, bottom/top, etc. of
rasters and vectors in **GRASS**.

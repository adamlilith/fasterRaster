# Get index of raster layers

Get index of raster layers

## Usage

``` r
.layerIndex(layer, x, recycle = TRUE, negate = FALSE)
```

## Arguments

- layer:

  Integer, numeric, logical, or character: Refers to one or more layers.

- x:

  A `GRaster`.

- recycle:

  Logical: If `TRUE` (default), and `layer` is logical and smaller in
  number than the number of layers, then recycle the vector of `layer`.

- negate:

  Logical: If `TRUE`, return indices of all layers *not* identified in
  `layer`.

## Value

An integer vector.

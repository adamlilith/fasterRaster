# Function to get extent from a "sources" name of a raster or vector

Function to get extent from a "sources" name of a raster or vector

## Usage

``` r
.ext(x, rastOrVect = NULL)
```

## Arguments

- x:

  A `GRaster`, `GSpatial`, or a character
  ([`sources()`](https://github.com/adamlilith/fasterRaster/reference/sources.md)
  name of a `GRaster` or `GVector`).

- rastOrVect:

  Either `NULL` (class taken from `x`, but `x` cannot be a character),
  or "`raster`" or "`vector`" (partial matching is used).

## Value

A numeric vector.

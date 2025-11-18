# Internal function for zonal() when y is a GVector

Internal function for zonal() when y is a GVector

## Usage

``` r
.zonalByVector(x, z, fun, probs, gtype)
```

## Arguments

- x:

  GRaster
  [`sources()`](https://github.com/adamlilith/fasterRaster/reference/sources.md)
  name.

- z:

  GVector
  [`sources()`](https://github.com/adamlilith/fasterRaster/reference/sources.md)
  name.

- fun:

  Character

- gtype:

  `geomtype(z, grass = TRUE)` ("area", "line", or "point")

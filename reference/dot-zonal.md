# Internal function for zonal()

Internal function for zonal()

## Usage

``` r
.zonal(x, z, fun, probs, zones, xnames)
```

## Arguments

- x:

  [`sources()`](https://github.com/adamlilith/fasterRaster/reference/sources.md)
  name of `GRaster`.

- z:

  [`sources()`](https://github.com/adamlilith/fasterRaster/reference/sources.md)
  name of "zones" `GRaster`.

- fun:

  Character: Name of function(s).

- probs:

  Numeric in the range 0 to 1, inclusive.

- zones:

  Vector of zone values (integers).

- xnames:

  Character: Names of `x`.

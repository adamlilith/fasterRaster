# Tests if a GRASS vector is linked to an attribute table

**GRASS** vectors can be lined to one or more attribute tables, or
"databases.". This function tests to see if the vector does indeed have
a database. This function is typically used by developers.

## Usage

``` r
.vHasDatabase(x)
```

## Arguments

- x:

  A `GVector` or the
  [`sources()`](https://github.com/adamlilith/fasterRaster/reference/sources.md)
  name of a vector in **GRASS**.

## Value

Logical.

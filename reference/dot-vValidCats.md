# Are the category values of a vector valid?

Category values of a **GRASS** vector can be invalid if **GRASS**
assigns more than one value to a geometry (e.g., "7/12"). This can occur
when the vector was created by software that does not use a topological
system (e.g., a shapefile).

## Usage

``` r
.vValidCats(x)
```

## Arguments

- x:

  A `GVector` or the
  [`sources()`](https://github.com/adamlilith/fasterRaster/reference/sources.md)
  name of one.

## Value

Logical.

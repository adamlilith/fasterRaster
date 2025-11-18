# Increment category values of a "GRASS" vector

Adds a constant to all category values of a **GRASS** vector. **This
function is mostly of use to developers.**

## Usage

``` r
.vIncrementCats(x, add)
```

## Arguments

- x:

  A `GVector` or the
  [`sources()`](https://github.com/adamlilith/fasterRaster/reference/sources.md)
  name of one.

- add:

  Integer: Value to add to each category value.

## Value

The
[`sources()`](https://github.com/adamlilith/fasterRaster/reference/sources.md)
name of a **GRASS** vector with category values incremented.

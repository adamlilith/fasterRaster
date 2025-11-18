# Has "GRASS" been started or not?

Returns `TRUE` or `FALSE`, depending on whether a **GRASS** connection
has been made or not within the current **R** session. Usually used only
by developers. **GRASS** is started the first time
[`fast()`](https://github.com/adamlilith/fasterRaster/reference/fast.md)
is used.

## Usage

``` r
grassStarted()
```

## Value

Logical.

## Examples

``` r
grassStarted()
#> [1] FALSE
```

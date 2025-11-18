# Returns .quiet() or NULL for "flags" argument to GRASS modules

A function for developers used for setting the "quiet' argument in
`flags` arguments passed to
[`rgrass::execGRASS()`](https://osgeo.github.io/rgrass/reference/execGRASS.html).
If `faster("debug")` is `TRUE`, the string "quiet" is returned. If
`FALSE`, then `NULL` is returned.

## Usage

``` r
.quiet()
```

## Value

A string (.quiet()) or `NULL`.

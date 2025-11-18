# Test if a coordinate reference system is unprojected

`is.lonlat()` attempts to determine if a coordinate reference system is
unprojected (e.g., WGS84, NAD83, NAD27, etc.). For `GRaster`s and
`GVector`s, the function should always be correct. For WKT character
strings and `sf` vectors, it does this by looking for the "CONVERSION\["
tag in the WKT string (or the object's WKT string), and if it finds one,
returns `FALSE`. This may not be truthful in all cases.

## Usage

``` r
# S4 method for class 'character'
is.lonlat(x)

# S4 method for class 'GLocation'
is.lonlat(x)

# S4 method for class 'sf'
is.lonlat(x)
```

## Arguments

- x:

  A WKT coordinate reference string or an object from which on can be
  obtained (e.g., a `GRaster`, `GVector`, `GRegion`, `GLocation`,
  `SpatRaster`, `SpatVector`, or `sf` object).

## Value

Logical (`TRUE` if unprojected, `FALSE` otherwise).

## See also

[`terra::is.lonlat()`](https://rspatial.github.io/terra/reference/is.lonlat.html)

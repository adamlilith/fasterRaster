# Revert to a previously-created "GRASS" "location"

This function resets the connection to a previously-created **GRASS**
"location". The session must have been already created using
[`fast()`](https://github.com/adamlilith/fasterRaster/reference/fast.md)
in the current **R** session. This function is typically only of use to
developers.

## Usage

``` r
# S4 method for class 'character'
.locationRestore(x)

# S4 method for class 'integer'
.locationRestore(x)

# S4 method for class 'numeric'
.locationRestore(x)

# S4 method for class 'GSpatial'
.locationRestore(x)
```

## Arguments

- x:

  Either:

  - A character: Name of the "location" in **GRASS**.

  - An integer: Index of the "location" in `.fasterRaster$locations`.

  - A `GSpatial` object (usually a `GRaster` or `GVector`).

  Any of these can be found using `.locationFind()`.

## Value

An object of class `GLocation` (invisibly) if successful. An error will
likely result if not.

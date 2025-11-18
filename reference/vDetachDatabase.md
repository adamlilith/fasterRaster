# Add a database table to a GRASS attribute table

`.vDetachDatabase()` detaches the database from a **GRASS** vector and
deletes it. This table is meant to be "invisible" to most users–they
should use interact with attribute tables using the `GVector` slot
`@table`. Some functions do require tables (e.g.,
[`extract()`](https://github.com/adamlilith/fasterRaster/reference/extract.md)
and
[`spatSample()`](https://github.com/adamlilith/fasterRaster/reference/spatSample.md)).
**This function is mostly of use to developers.**

## Usage

``` r
.vDetachDatabase(x)
```

## Arguments

- x:

  A `GVector` or the name of a vector in **GRASS**.

## Value

Invisibly returns the
[`sources()`](https://github.com/adamlilith/fasterRaster/reference/sources.md)
name of a vector in **GRASS**.

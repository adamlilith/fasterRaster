# Rows of a GRaster or GVector's table that have no NAs or that have NAs

When applied to a categorical `GRaster`, `compete.cases()` returns
`TRUE` for each row of the "levels" table that has no `NA`s in it. In
contrast, `missing.cases()` returns `TRUE` for each row that has at
least one `NA` in it. If the raster is not categorical, then `NA` is
returned.

When applied to a `GVector` with a data table, `complete.cases()`
returns `TRUE` for each row where there are no `NA`s. if the `GVector`
has no data table, then a vector of `TRUE` values the same length as the
total number of geometries will be returned. In contrast,
`missing.cases()` returns `TRUE` for every row that has at least one
`NA` in it. If the `GVector` has no data table, then a vector of `FALSE`
values is returned.

## Usage

``` r
# S4 method for class 'GRaster'
complete.cases(..., levels = TRUE)

# S4 method for class 'GVector'
complete.cases(...)

# S4 method for class 'GRaster'
missing.cases(..., levels = TRUE)

# S4 method for class 'GVector'
missing.cases(...)
```

## Arguments

- ...:

  A `GRaster` or `GVector`.

- levels:

  Logical (`GRaster`s only): If `TRUE` (default), then assess only the
  "value" and
  [`activeCat()`](https://github.com/adamlilith/fasterRaster/reference/activeCat.md)
  columns of the levels table (see
  [`levels()`](https://github.com/adamlilith/fasterRaster/reference/levels.md)).
  If `FALSE`, then assess all columns (see
  [`cats()`](https://github.com/adamlilith/fasterRaster/reference/levels.md)).

## Value

Both `complete.cases()` and `missing.cases()` return the same type of
object. The output depends on the input:

- A categorical `GRaster` with just one layer: A logical vector.

- An integer, float, or double `GRaster` with just one layer: `NA`.

- A `GRaster` with multiple layers: A list with one element per layer
  with either logical vectors or `NA`s, as per above.

- A `GVector` with a data table: A logical vector.

- A `GVector` without a data table: `NA`.

## See also

[`missingCats()`](https://github.com/adamlilith/fasterRaster/reference/missingCats.md)

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Plant specimens (points) and land cover
madDypsis <- fastData("madDypsis")
madCover <- fastData("madCover")

# Convert to GVector and GRaster
dypsis <- fast(madDypsis)
cover <- fast(madCover)

### GVector

# Look at the data table:
as.data.table(dypsis)

# Which rows have no NAs?
complete.cases(dypsis)

# Which rows have at least one NA (opposite of above)?
missing.cases(dypsis)

### GRaster

# Look at the levels table:
levels(cover)

# Which rows of levels table have no NAs?
complete.cases(cover)

# Which rows have at least one NA (opposite of above)?
missing.cases(cover)

}
```

# Re-make vector "category" (cat) values

Each geometry in a **GRASS** vector has a "category" number (abbreviated
"cat" in output and modules). Geometries can have the same or different
numbers, but for functions to work as intended, they often need to have
sequential category values, starting at 1, with no skips between
integers. This function reconstitutes the category values of a vector in
**GRASS** so they being with 1 and have no skips. **This function is
mostly of use to developers.**

## Usage

``` r
.vRecat(x, gtype, cats = NULL)
```

## Arguments

- x:

  A `GVector` or the
  [`sources()`](https://github.com/adamlilith/fasterRaster/reference/sources.md)
  name of a vector in **GRASS**.

- gtype:

  Character: Type of vector features in **GRASS** format (i.e., either
  `point`, `line`, or `area`). See
  [`geomtype()`](https://github.com/adamlilith/fasterRaster/reference/geomtype.md).

- cats:

  `NULL` (default) or `integer` or `character` vector: Category values
  of the **GRASS** vector. Supplying these can speed the re-assignment
  of categories. The values of `cats` is *not* used as the new category
  values. Rather, they will be used to indicate which geometries belong
  to the same multi-part feature.

## Value

The
[`sources()`](https://github.com/adamlilith/fasterRaster/reference/sources.md)
name of a vector.

## See also

[`.vIncrementCats()`](https://github.com/adamlilith/fasterRaster/reference/vIncrementCats.md),
[`.vCats()`](https://github.com/adamlilith/fasterRaster/reference/vCats.md)

# Convert a GRASS vector's attribute table to a data.table

**GRASS** vectors can be linked to an attribute table, which can be
exported from **GRASS** to **R** using this function. **This function is
mostly of use to developers.**

Values in the `cat` column are not necessarily unique–if a value appears
more than once, the set of features they index are (in other software)
called "multipart" features. The table can have more columns with
metadata for each feature.

This function is typically used by developers.

## Usage

``` r
.vAsDataTable(x)
```

## Arguments

- x:

  A `GVector` or the name of a vector in **GRASS**.

## Value

A `data.table` or `NULL` if the vector is not attached to a database.

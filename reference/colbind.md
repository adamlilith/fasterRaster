# Add columns to the data table of a GVector

`colbind()` adds columns to the data table of a `GVector`. You can
combine multiple a `GVector`'s data table with `data.frame`s,
`data.table`s, `matrices`, or the data table(s) from other `GVector`(s).
To combine two `GVector`s, see
[`rbind()`](https://github.com/adamlilith/fasterRaster/reference/rbind.md).

## Usage

``` r
# S4 method for class 'GVector'
colbind(x, ...)
```

## Arguments

- x, ...:

  The first argument must be a `GVector`. Subsequent arguments can be
  `data.frame`s, `data.table`s, `matrices`, or `GVector`s. Only the data
  tables of subsequent `GVector`s are added to the table in `x`; the
  geometries are ignored.

## Value

A `GVector`.

## See also

[`rbind()`](https://github.com/adamlilith/fasterRaster/reference/rbind.md),
`addTable<-`,
[`dropTable()`](https://github.com/adamlilith/fasterRaster/reference/addTable.md)

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)

# Rivers vector
madRivers <- fastData("madRivers")

# Convert sf to a GVector
rivers <- fast(madRivers)

# Convert GVector to data.frame or data.table
as.data.frame(rivers)
as.data.table(rivers)

# Subset rivers vector
rivers1 <- rivers[1:2]
rivers2 <- rivers[10:11]

# Concatenate rivers
riversCombo <- rbind(rivers1, rivers2)
riversCombo

# Add columns
newCol <- data.frame(new = 1:11)
riversCol <- colbind(rivers, newCol)
riversCol

# Remove table
riversCopy <- rivers
riversCopy # has data table
riversCopy <- dropTable(riversCopy)
riversCopy # no data table

# Add a new table
newTable <- data.frame(num = 1:11, letters = letters[1:11])
addTable(riversCopy) <- newTable
riversCopy

}
```

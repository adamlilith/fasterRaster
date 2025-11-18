# Combine one or more GVectors

`rbind()` combines two or more `GVector`s of the same type (points,
lines, or polygons) and same coordinate reference system. You can speed
operations by putting the vector that is largest in memory first in
`rbind(...)`. If the `GVector`s have data tables, these will also be
combined using `rbind()` if their column names and data types match.

## Usage

``` r
# S4 method for class 'GVector'
rbind(..., deparse.level = 1)
```

## Arguments

- ...:

  One or more `GVector`s.

- deparse.level:

  See `rbind()`.

## Value

A `GVector`.

## See also

[`colbind()`](https://github.com/adamlilith/fasterRaster/reference/colbind.md),
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

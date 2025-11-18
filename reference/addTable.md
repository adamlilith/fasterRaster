# Attach or detach GVector's data table

`addTable()` adds an entire table to a `GVector`. It will replace any
existing table. There must be one row in the table for each geometry
(see
[`ngeom()`](https://github.com/adamlilith/fasterRaster/reference/ngeom.md)).
You can also add a table column-by-column using the `$<-` operator.

`dropTable()` removes a data table associated with a `GVector`.

## Usage

``` r
# S4 method for class 'GVector,data.frame'
addTable(x, ...) <- value

# S4 method for class 'GVector,data.table'
addTable(x, ...) <- value

# S4 method for class 'GVector,matrix'
addTable(x, ...) <- value

# S4 method for class 'GVector'
dropTable(x)
```

## Arguments

- x:

  A `GVector`.

- ...:

  Other arguments (ignored).

- value:

  A `data.frame`, `data.table`, or `matrix`.

## Value

A `GVector`.

## See also

`$<-`,
[`colbind()`](https://github.com/adamlilith/fasterRaster/reference/colbind.md),
[`rbind()`](https://github.com/adamlilith/fasterRaster/reference/rbind.md),
[`as.data.frame()`](https://github.com/adamlilith/fasterRaster/reference/as.data.frame.md),
[`as.data.table()`](https://github.com/adamlilith/fasterRaster/reference/as.data.frame.md)

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

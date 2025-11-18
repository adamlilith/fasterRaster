# Create lines connecting nearest features of two GVectors

`connectors()` creates a lines `GVector` which represent the shortest
(Great Circle) paths between each feature of one `GVector` and the
nearest feature of another `GVector`.

## Usage

``` r
# S4 method for class 'GVector,GVector'
connectors(x, y, minDist = NULL, maxDist = NULL)
```

## Arguments

- x, y:

  `GVector`s.

- minDist, maxDist:

  Either `NULL` (default) or numeric values: Ignore features separated
  by less than or greater than these distances.

## Value

A `GVector` with a data table that has the length of each connecting
line in meters.

## See also

**GRASS** manual for tool `v.distance` (see `grassHelp("v.distance")`).

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)

# Rivers vector and locations of Dypsis plants
madRivers <- fastData("madRivers")
madDypsis <- fastData("madDypsis")

# Convert sf's to GVectors:
dypsis <- fast(madDypsis)
rivers <- fast(madRivers)

### Connections from each point to nearest river
consFromDypsis <- connectors(dypsis, rivers)

plot(rivers, col = "blue")
plot(dypsis, add = TRUE)
plot(consFromDypsis, col = "red", add = TRUE)

### Connections from each river to nearest point
consFromRivers <- connectors(rivers, dypsis)

plot(rivers, col = "blue")
plot(dypsis, add = TRUE)
plot(consFromRivers, col = "red", add = TRUE)

}
```

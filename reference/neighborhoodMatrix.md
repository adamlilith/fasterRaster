# Neighborhood matrix from a polygons GVector

This function returns a neighborhood matrix from a polygons `GVector`,
which represents which geometries touch one another. It is useful for
implementing geostatistical analyses that require indicators about which
area features are next to one another.

Polygons must share more than one point for them to be considered a
neighbors (i.e., same as `spdep::poly2nb(x, queen = FALSE)`).

This function needs the **GRASS** addon `v.neighborhoodmatrix`. If it is
not installed, it will try to install it.

## Usage

``` r
# S4 method for class 'GVector'
neighborhoodMatrix(x)

# S4 method for class 'GVector'
neighbourhoodMatrix(x)
```

## Arguments

- x:

  A polygons \`GVector.

## Value

A `list`. Each element represents a polygon. If an element is empty, it
has no neighbors. Otherwise, it is a vector of integers, which represent
the indices of the polygon(s) to which it is a neighbor.

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)

# Polygons vector:
madCoast4 <- fastData(madCoast4)
mc4 <- fast(madCoast4)

neighs <- neighborhoodMatrix(mc4)
neighs

}
```

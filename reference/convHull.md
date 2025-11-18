# Minimum convex hull around a spatial vector

Create a minimum convex hull around a spatial vector.

## Usage

``` r
# S4 method for class 'GVector'
convHull(x, by = "")
```

## Arguments

- x:

  A `GVector`.

- by:

  Character: If `""` (default), then a convex hull is created for all
  geometries together. Otherwise, this is the name of a field in the
  vector. Hulls will be created for each set of geometries with the same
  value in this column.

## Value

A `GVector`.

## See also

`link[terra]{convHull}`, `link[sf]{st_convex_hull}`, tool `v.hull` in
**GRASS** (see `grassHelp("v.hull")`)

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)

# Points vector of specimens of species in the plant genus Dypsis
madDypsis <- fastData("madDypsis")

# Convert sf to a GVector:
dypsis <- fast(madDypsis)

### Convex hull for all plant specimens together:
ch <- convHull(dypsis)

### Convex hull for each species:
head(dypsis) # See the "rightsHolder" column?
chHolder <- convHull(dypsis, by = "rightsHolder")

### Plot:
plot(dypsis)
plot(ch, add = TRUE)
n <- length(chHolder)
for (i in 1:n) {
   plot(chHolder[[i]], border = i, add = TRUE)
}

}
```

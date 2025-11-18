# Intersection of two GVectors

The `intersect()` function selects the area of overlap between two
`GVector`s of the same type (points, lines or polygons). You can also
use the `*` operator (e.g., `vect1 * vect2`).

## Usage

``` r
# S4 method for class 'GVector,GVector'
intersect(x, y)
```

## Arguments

- x, y:

  `GVector`s.

## Value

A `GVector`.

## See also

[`c()`](https://github.com/adamlilith/fasterRaster/reference/c.md),
[`aggregate()`](https://github.com/adamlilith/fasterRaster/reference/aggregate.md),
[`crop()`](https://github.com/adamlilith/fasterRaster/reference/crop.md),
[`union()`](https://github.com/adamlilith/fasterRaster/reference/union.md),
[`xor()`](https://github.com/adamlilith/fasterRaster/reference/xor.md)

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)

# Polygon of coastal Madagascar and Dypsis specimens
madCoast4 <- fastData("madCoast4") # polygons
madDypsis <- fastData("madDypsis") # points

# Convert vectors:
coast4 <- fast(madCoast4)
dypsis <- fast(madDypsis)

# Create another polygons vector from a convex hull around Dypsis points
hull <- convHull(dypsis)

### union()

unioned <- union(coast4, hull)
plot(unioned)

plus <- coast4 + hull # same as union()

### intersect

inter <- intersect(coast4, hull)
plot(coast4)
plot(hull, border = "red", add = TRUE)
plot(inter, border = "blue", add = TRUE)

### xor

xr <- xor(coast4, hull)
plot(coast4)
plot(xr, border = "blue", add = TRUE)

### erase

erased <- erase(coast4, hull)
plot(coast4)
plot(erased, border = "blue", add = TRUE)

minus <- coast4 - hull # same as erase()

}
```

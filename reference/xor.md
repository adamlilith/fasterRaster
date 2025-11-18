# Select parts of polygons not shared between two GVectors

The `xor()` function selects the area that does *not* overlap between
two "polygon" `GVector`s. You can also use the `/` operator, as in
`vect1 / vect2`.

## Usage

``` r
# S4 method for class 'GVector,GVector'
xor(x, y)
```

## Arguments

- x, y:

  `GVector`s.

## Value

A `GVector`.

## See also

[`crop()`](https://github.com/adamlilith/fasterRaster/reference/crop.md),
[`intersect()`](https://github.com/adamlilith/fasterRaster/reference/intersect.md),
[`union()`](https://github.com/adamlilith/fasterRaster/reference/union.md),
[`erase()`](https://github.com/adamlilith/fasterRaster/reference/erase.md)

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

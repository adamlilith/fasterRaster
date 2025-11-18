# Create a hexagonal grid

This function creates a `GVector` of "wall-to-wall" hexagons. The input
can be a `GVector` or `GRaster`, which provides the extent of the
output.

## Usage

``` r
# S4 method for class 'GRaster'
hexagons(x, ny = 10, expand = 0, angle = 0)

# S4 method for class 'GVector'
hexagons(x, ny = 10, expand = 0, angle = 0)
```

## Arguments

- x:

  A `GRaster` or `GVector`.

- ny:

  Integer or numeric integer: Number of rows of hexagons that span the
  extent of object `x`.

- expand:

  One or two numeric values: Expand the region by this proportion in
  both directions (a single value) or in the x- and y-dimensions
  separately. Expanding the region can be helpful to ensure the entire
  area of interest is covered by polygons, which can otherwise leave
  gaps at the edges. The number of rows and columns will be increased,
  but the number of hexagons that span `x` will still be `ny`.

- angle:

  Numeric: Degrees by which to rotate grid (from north, clockwise).

## Value

A `GVector`.

## See also

[`grid()`](https://github.com/adamlilith/fasterRaster/reference/grid.md),
tool `v.mkgrid` in **GRASS**

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)

# Points vector of specimens of species in the plant genus Dypsis
madCoast0 <- fastData("madCoast0")

# Convert sf to a GVector:
coast <- fast(madCoast0)

### grid

# grid specified by number of cells in x-dimension
g1 <- grid(coast, nx = 10)
plot(coast, col = "cornflowerblue")
plot(g1, add = TRUE)

# grid specified by number of cells in x- and y-dimension
g2 <- grid(coast, nx = 10, ny = 5)
plot(coast, col = "cornflowerblue")
plot(g2, add = TRUE)

# grid specified by size of cells in both dimensions
g3 <- grid(coast, nx = 1250, ny = 2000, use = "size")
plot(coast, col = "cornflowerblue")
plot(g3, add = TRUE)

### hexagons

hexes <- hexagons(coast, ny = 10)
plot(hexes)
plot(coast, lwd = 2, add = TRUE)

hexes <- hexagons(coast, ny = 10, expand = c(0.3, 0.1))
plot(hexes)
plot(coast, lwd = 2, add = TRUE)

}
```

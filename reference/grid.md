# Create a grid GVector

This function creates a `GVector` of "wall-to-wall" cells (like a
lattice). The input can be a `GVector` or `GRaster`, which provides the
extent of the output.

## Usage

``` r
# S4 method for class 'GRaster'
grid(x, nx = NULL, ny = NULL, use = "number", angle = 0)

# S4 method for class 'GVector'
grid(x, nx = NULL, ny = NULL, use = "number", angle = 0)
```

## Arguments

- x:

  A `GRaster` or `GVector`.

- nx, ny:

  Integer or numeric:

  - If `use` is `"number"`, then these values represent the number of
    rows and columns in the grid.

  - If `use` is `size`, then these values represent the size of the
    cells in the x- and y-dimensions.

- use:

  Character: How to generate the grid. If this is `number` (default),
  then `nx` and `ny` are taken to be the number of grid cells. If
  `size`, then `nx` and `ny` are taken to be the size of the grid cells.

- angle:

  Numeric: Degrees by which to rotate grid (from north, clockwise).

## Value

A `GVector`.

## See also

[`hexagons()`](https://github.com/adamlilith/fasterRaster/reference/hexagons.md),
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

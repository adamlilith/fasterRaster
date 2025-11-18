# GRaster with values equal to row, column, coordinate, regular, or "chess"

This function can be used to make a `GRaster` with cell values equal to
the cell center's longitude, latitude, row, or column, or in a
"chess"-like or "regular" pattern.

## Usage

``` r
# S4 method for class 'GRaster'
init(x, fun, odd = TRUE, vals = c(0, 1))
```

## Arguments

- x:

  A `GRaster` to be used as a template.

- fun:

  Character: Any of:

  - `"x"` or `"y"`: Cell longitude or latitude

  - `"row"` or `"col"`: Cell row or column

  - `"chess"`: Alternating values.

  - `"regular"`: Evenly-spaced cells with the same value.

- odd:

  Logical: If `TRUE` (default), and `fun` is `"chess"`, then the top
  left cell in the raster will be a "negative" cell. If `FALSE`, then
  the top left cell with be "positive".

- vals:

  Vector of two numeric values: If `fun` is `"chess"` or `"regular"`,
  then assign the first value to "positive" cells and the second value
  to "negative" cells. The default is `c(1, 0)`

## Value

A `GRaster` with as many layers as `x`.

## See also

[`terra::init()`](https://rspatial.github.io/terra/reference/init.html),
[`longlat()`](https://github.com/adamlilith/fasterRaster/reference/longlat.md)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Elevation raster, rivers vector
madElev <- fastData("madElev")

# Convert to a GRaster
elev <- fast(madElev)

# Cell coordinates
init(elev, "x")
init(elev, "y")

# Cell row or column
init(elev, "row")
init(elev, "col")

# Chess
elevAgg <- aggregate(elev, 32) # make cells bigger so we can see them

chessOdd <- init(elevAgg, "chess")
chessEven <- init(elevAgg, "chess", odd = FALSE)

chess <- c(chessOdd, chessEven)
names(chess) <- c("odd", "even")
plot(chess)

# Chess with user-defined values
elevAgg <- aggregate(elev, 32) # make cells bigger so we can see

chessOdd13 <- init(elevAgg, "chess", vals = c(0, 13))
chessEven13 <- init(elevAgg, "chess", odd = FALSE, vals = c(0, 13))

chess13 <- c(chessOdd13, chessEven13)
names(chess13) <- c("odd", "even")
plot(chess13)

# Regular
elevAgg <- aggregate(elev, 32) # make cells bigger so we can see

regOdd <- init(elevAgg, "regular")
regEven <- init(elevAgg, "regular", odd = FALSE)

reg <- c(regOdd, regEven)
names(reg) <- c("odd", "even")
plot(reg)

}
```

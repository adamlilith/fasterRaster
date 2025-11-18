# Calculate cell values based on values of nearby cells

This function calculates statistics on a moving "neighborhood" of cells
of a raster. The neighborhood can be a square, circle, or a user-defined
set of cells (with or without weights).

## Usage

``` r
# S4 method for class 'GRaster'
focal(x, w = 3, fun = "sum", circle = FALSE, quantile = 0.5)
```

## Arguments

- x:

  A `GRaster`.

- w:

  Numeric integer or a square matrix with an odd number of rows and
  columns: The size and nature of the neighborhood:

  - "Square" neighborhoods (when `circle = FALSE`): An odd integer \>=
    3, indicating indicates the size of a "square" neighborhood (number
    of cells wide and number or cells tall).

  - "Circular" neighborhoods (when `circle = TRUE`): An odd integer
    \>=3, indicating the diameter of the circle.

  - A matrix of cell weights: The matrix must be square and have an odd
    number of rows and columns (example:
    `matrix(c(0.5, 1, 0.5, 1, 2, 1, 0.5, 1, 0.5), nrow=3)`). You cannot
    use a weights matrix when `circle = TRUE`. Cells with `NA` as a
    weight will be ignored. Note that weighted matrices should not be
    used for function `min`, `max`, `count`, `nunique`, or
    `interspersion`.

- fun:

  Character: Name of the function to apply to the neighborhood:

  - "`mean`" (default)

  - "`median`"

  - "`mode`"

  - "`min`" or "`max`": Minimum or maximum. Should not use a weights
    matrix.

  - "`range`": Difference between the maximum and minimum. Should not
    use a weights matrix.

  - "`sd`": Sample standard deviation. NB: This is the same as the
    [`stats::sd()`](https://rdrr.io/r/stats/sd.html) function.

  - "`sdpop`": Population standard deviation. NB: This is the same as
    the function "stddev" in the **GRASS** tool `r.neighbors`.

  - "`sum`": Sum of non-\`NA“ cells.

  - "`count`": Number of non-\`NA cells. Should not use a weights
    matrix.

  - "`var`": Sample variance. NB: This is the same as the
    [`stats::var()`](https://rdrr.io/r/stats/cor.html) function.

  - "`varpop`": Population variance. NB: This is the same as the
    function "variance" in the **GRASS** tool `r.neighbors`.

  - "`nunique`": Number of unique values. Should not use a weights
    matrix.

  - "`interspersion`": Proportion of cells with values different from
    focal cell (e.g., if 6 of 8 cells have different values, then the
    interspersion is 6/8 = 0.75). NB: This is slightly different from
    how it is defined in the **GRASS** tool `r.neighbors`. Should not
    use a weights matrix.

  - "`quantile`": Quantile of values. The value in argument `quantile`
    is used to specify the quantile.

  The center cell value is always included in the calculations, and all
  calculations ignore `NA` cells (i.e., they do not count as cells in
  the focal neighborhood).

- circle:

  Logical: If `FALSE` (default), use a square neighborhood. If `TRUE`,
  use a circular neighborhood. When this is `TRUE`, argument `w` cannot
  be a matrix.

- quantile:

  Numeric between 0 and 1, inclusive: Quantile to calculate when
  `fun = "quantile"`. The default value is 0.5 (median), and valid
  values must be in the range between 0 and 1, inclusive.

## Value

A `GRaster`.

## See also

[`terra::focal()`](https://rspatial.github.io/terra/reference/focal.html),
**GRASS** manual page for tool `r.neighbors` (see
`grassHelp("r.neighbors")`)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

# Focal calculations:
sums <- focal(elev, fun = "sum")
means <- focal(elev, fun = "mean")

# Focal calculations on a circular window:
sds <- focal(elev, fun = "sd") # square
sdsCircle <- focal(elev, fun = "sd", circle = TRUE) # circle

sds
sdsCircle

plot(sds - sdsCircle)

# Focal calculations with user-defined weights:
w <- matrix(c(1, 0, 1, 0, 1, 0, 1, 0, 1), ncol = 3)
w
sumsWeighted <- focal(elev, fun = "sum", w = w)

s <- c(sums, sumsWeighted)
minmax(s)

}
```

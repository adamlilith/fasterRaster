# Logic-methods operations on GRasters

You can do logical operations on `GRaster`s. A cell with a value of 1 is
interpreted as `TRUE`, and a value of 0 is interpreted as `FALSE`. You
can compare:

- A `GRaster` to another `GRaster`

- A `GRaster` to a logical value (`TRUE` or `FALSE`, but not `NA`–see
  [`not.na()`](https://github.com/adamlilith/fasterRaster/reference/math.md))

- A `GRaster` to a numeric or integer value that is 0 or 1

Operators include:

- `|`: `TRUE` if either condition is `TRUE` (or 1), but returns `NA` if
  either condition is `NA`.

- `&`: `TRUE` if both conditions are `TRUE` (or 1), but `NA` if either
  is `NA`.

## Usage

``` r
# S4 method for class 'GRaster,GRaster'
Logic(e1, e2)

# S4 method for class 'logical,GRaster'
Logic(e1, e2)

# S4 method for class 'GRaster,logical'
Logic(e1, e2)

# S4 method for class 'GRaster,numeric'
Logic(e1, e2)

# S4 method for class 'numeric,GRaster'
Logic(e1, e2)

# S4 method for class 'GRaster,integer'
Logic(e1, e2)

# S4 method for class 'integer,GRaster'
Logic(e1, e2)
```

## Arguments

- e1, e2:

  Two `GRaster`s, or a `GRaster` and a logical value (`TRUE` or `FALSE`,
  but not `NA`), a numeric value that is 0 or 1 (but not `NA_real_`), or
  an integer value that is 0 or 1 (but not `NA_integer_`).

## Value

A binary `GRaster` (1 ==\> `TRUE`, 0 ==\> `FALSE`, plus `NA` when
comparison results in `NA`).

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster
elev <- fast(madElev)
elevs <- c(elev, elev, log10(elev) - 1, sqrt(elev))
names(elevs) <- c("elev1", "elev2", "log_elev", "sqrt_elev")

elev
elevs

# Comparisons
elev < 100
elev <= 100
elev == 100
elev != 100
elev > 100
elev >= 100

elev + 100 < 2 * elev

elevs > 10
10 > elevs

# logic
elev < 10 | elev > 200
elev < 10 | cos(elev) > 0.9

elev < 10 | TRUE
TRUE | elev > 200

elev < 10 | FALSE
FALSE | elev > 200

elev < 10 & cos(elev) > 0.9

elev < 10 & TRUE
TRUE & elev > 200

elev < 10 & FALSE
FALSE & elev > 200

}
```

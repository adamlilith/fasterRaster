# Mathematical operations on each layer of a GRasters

You can apply mathematical functions to each layer of a `GRaster`. The
output is a `GRaster` with the same number or layers as the input.
Available functions include:  

- `NA`s:

  - `is.na()`

  - `not.na()`

- Absolute value: `abs()`

- Trigonometric functions (assumes values are in radians):

  - `cos()`

  - `sin()`

  - `tan()`

  - `acos()`

  - `asin()`

  - `atan()`

  - `atan2()`

- Exponential and logarithmic functions:

  - `exp()`

  - `log()` (natural log)

  - `ln()` (also natural log)

  - `log2()` (log, base 2)

  - `log10()` (log, base 10)

  - `log1p()` (same as `log(x + 1)`)

  - `log10p()` (same as `log(x + 1, base = 10)`)

- Power functions:

  - `sqrt()`

  - `x^y`

- Rounding:

  - `round()`

  - `floor()` (round down)

  - `ceiling()` (round up)

  - `trunc()` (remove decimal portion)

## Usage

``` r
# S4 method for class 'GRaster'
is.na(x)

# S4 method for class 'GRaster'
not.na(x, falseNA = FALSE)

# S4 method for class 'GRaster'
abs(x)

# S4 method for class 'GRaster'
sin(x)

# S4 method for class 'GRaster'
cos(x)

# S4 method for class 'GRaster'
tan(x)

# S4 method for class 'GRaster'
asin(x)

# S4 method for class 'GRaster'
acos(x)

# S4 method for class 'GRaster'
atan(x)

# S4 method for class 'GRaster,GRaster'
atan2(y, x)

# S4 method for class 'GRaster'
exp(x)

# S4 method for class 'GRaster'
log1p(x)

# S4 method for class 'GRaster'
log10p(x)

# S4 method for class 'GRaster'
log(x, base = exp(1))

# S4 method for class 'GRaster'
ln(x)

# S4 method for class 'GRaster'
log2(x)

# S4 method for class 'GRaster'
log10(x)

# S4 method for class 'GRaster'
sqrt(x)

# S4 method for class 'GRaster'
round(x, digits = 0)

# S4 method for class 'GRaster'
floor(x)

# S4 method for class 'GRaster'
ceiling(x)

# S4 method for class 'GRaster'
trunc(x)
```

## Arguments

- x, y:

  `GRaster`s.

- falseNA:

  Logical (function `not.na()`): If `FALSE` (default), non-`NA` cells
  will be converted to 1, and `NA` cells to 0. If `TRUE`, non-`NA` cells
  will be converted to and `NA` cells will stay as `NA`.

- base:

  Numeric: Base of the logarithm.

- digits:

  Numeric: Number of digits to round to. If negative, then rounding is
  to the nearest positive power of 10. For example, if `digits = -2`,
  then the `GRaster` values are rounded to the nearest 100.

## Value

A `GRaster`.

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster
elev <- fast(madElev)
elevs <- c(elev, elev, log10(elev) - 1, sqrt(elev))
names(elevs) <- c("elev1", "elev2", "log_elev", "sqrt_elev")

elev
elevs

# do some math
elev + 100
elev - 100
elev * 100
elev / 100
elev ^ 2
elev %/% 100 # divide then round down
elev %% 100 # modulus

100 + elev
100 %/% elev
100 %% elev

elevs + 100
100 + elevs

# math with logicals
elev + TRUE
elev - TRUE
elev * TRUE
elev / TRUE
elev ^ TRUE
elev %/% TRUE # divide then round down
elev %% TRUE # modulus

elevs + TRUE
TRUE + elevs

# Raster interacting with raster(s):
elev + elev
elev - elev
elev * elev
elev / elev
elev ^ log(elev)
elev %/% sqrt(elev) # divide then round down
elev %% sqrt(elev) # modulus

elevs + elev
elev * elevs

# sign
abs(-1 * elev)
abs(elevs)

# powers
sqrt(elevs)

# trigonometry
sin(elev)
cos(elev)
tan(elev)

asin(elev)
acos(elev)
atan(elev)

atan(elevs)
atan2(elev, elev^1.2)
atan2(elevs, elev^1.2)
atan2(elev, elevs^1.2)
atan2(elevs, elevs^1.2)

# logarithms
exp(elev)
log(elev)
ln(elev)
log2(elev)
log1p(elev)
log10(elev)
log10p(elev)
log(elev, 3)

log(elevs)

# rounding
round(elev + 0.5)
floor(elev + 0.5)
ceiling(elev + 0.5)
trunc(elev + 0.5)

}
```

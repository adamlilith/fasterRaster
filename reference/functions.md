# Mathematical operations on two or more GRasters

These functions can be applied to a "stack" of `GRaster`s with two or
more layers. They return a single-layered `GRaster`. If you want to
summarize across cells in a raster (e.g., calculate the mean value of
all cells on a raster), use
[`global()`](https://github.com/adamlilith/fasterRaster/reference/global.md).
Options include:

- Numeration: `count()` (number of non-`NA` cells), `sum()`.

- Central tendency: `mean()`, `mmode()` (mode), `median()`.

- Extremes: `min()`, `max()`, `which.min()` (index of raster with the
  minimum value), `which.max()` (index of the raster with the maximum
  value)

- Dispersion: `range()`, `stdev()` (standard deviation), `var()` (sample
  variance), `varpop()` (population variance), `nunique()` (number of
  unique values), `quantile()` (use argument `probs`), `skewness()`, and
  `kurtosis()`.

- `NA`s: `anyNA()` (any cells are `NA`?), `allNA()` (are all cells
  `NA`?)

## Usage

``` r
# S4 method for class 'GRaster'
mean(x, na.rm = FALSE)

# S4 method for class 'GRaster'
mmode(x, na.rm = FALSE)

# S4 method for class 'GRaster'
median(x, na.rm = FALSE)

# S4 method for class 'GRaster'
count(x)

# S4 method for class 'GRaster'
sum(x, na.rm = FALSE)

# S4 method for class 'GRaster'
min(x, na.rm = FALSE)

# S4 method for class 'GRaster'
max(x, na.rm = FALSE)

# S4 method for class 'GRaster'
which.min(x)

# S4 method for class 'GRaster'
which.max(x)

# S4 method for class 'numeric'
sdpop(x, na.rm = FALSE)

# S4 method for class 'GRaster'
varpop(x, na.rm = FALSE)

# S4 method for class 'numeric'
varpop(x, na.rm = FALSE)

# S4 method for class 'GRaster'
stdev(x, pop = TRUE, na.rm = FALSE)

# S4 method for class 'GRaster'
var(x, na.rm = FALSE)

# S4 method for class 'GRaster'
nunique(x, na.rm = FALSE)

# S4 method for class 'GRaster'
skewness(x, na.rm = FALSE)

# S4 method for class 'GRaster'
kurtosis(x, na.rm = FALSE)

# S4 method for class 'GRaster'
range(x, na.rm = FALSE)

# S4 method for class 'GRaster'
quantile(x, prob, na.rm = FALSE)

# S4 method for class 'GRaster'
anyNA(x)

# S4 method for class 'GRaster'
allNA(x)
```

## Arguments

- x:

  A `GRaster`. Typically, this raster will have two or more layers.
  Values will be calculated within cells across rasters.

- na.rm:

  Logical: If `FALSE` (default), of one cell value has an `NA`, the
  result will be `NA`. If `TRUE`, `NA`s are ignored.

- pop:

  Logical (for `stdev()`): If `TRUE` (default), calculate the population
  standard deviation across layers. If `FALSE`, calculate the sample
  standard deviation.

- prob:

  Numeric: Quantile to calculate. Used for `quantile()`.

## Value

A `GRaster`.

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Example data
madChelsa <- fastData("madChelsa")

# Convert a SpatRaster to a GRaster
chelsa <- fast(madChelsa)
chelsa # 4 layers

# Central tendency
mean(chelsa)
mmode(chelsa)
median(chelsa)

# Statistics
nunique(chelsa)
sum(chelsa)
count(chelsa)
min(chelsa)
max(chelsa)
range(chelsa)
skewness(chelsa)
kurtosis(chelsa)

stdev(chelsa)
stdev(chelsa, pop = FALSE)
var(chelsa)
varpop(chelsa)

# Which layers have maximum/minimum?
which.min(chelsa)
which.max(chelsa)

# Regression

# Note the intercept is different for fasterRaster::regress().
regress(chelsa)
regress(madChelsa, 1:nlyr(madChelsa))

# Note: To get quantiles for each layer, use global().
quantile(chelsa, 0.1)

# NAs
madForest2000 <- fastData("madForest2000")
forest2000 <- fast(madForest2000)
forest2000 <- project(forest2000, chelsa, method = "near")

chelsaForest <- c(chelsa, forest2000)

nas <- anyNA(chelsaForest)
plot(nas)

allNas <- allNA(chelsaForest)
plot(allNas)

}
```

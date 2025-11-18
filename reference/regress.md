# Regression intercept, slope, r2, and t-value across each set of cells

This function performs a regression on each set of cells in a
multi-layered `GRaster`. The output is a `GRaster` with the intercept,
slope, r^2 value, and Student's t value. The regression formula is as
`y ~ 1 + x`, where `x` is the layer number of each layer (e.g., 1 for
the first or top layer in the input `GRaster`, 2 for the second or
second-to-top layer, etc.). Note that this is restricted version of the
functionality in
[`terra::regress()`](https://rspatial.github.io/terra/reference/regress.html).

## Usage

``` r
# S4 method for class 'GRaster,missing'
regress(y, x, na.rm = FALSE)
```

## Arguments

- y:

  A multi-layer `GRaster`.

- x:

  Ignored.

- na.rm:

  Logical: If `FALSE`, any series of cells with `NA` in at least one
  cell results in an `NA` in the output.

## Value

A multi-layer `GRaster`.

## See also

[`terra::regress()`](https://rspatial.github.io/terra/reference/regress.html)

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

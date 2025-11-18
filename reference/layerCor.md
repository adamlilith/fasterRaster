# Correlation between GRasters

This function returns a correlation or covariance matrix between two or
more `GRaster` layers. This function returns the sample correlation and
covariance (i.e., the denominator is n - 1).

## Usage

``` r
# S4 method for class 'GRaster'
layerCor(x, fun = "cor")
```

## Arguments

- x:

  A `GRaster` with two or more layers.

- fun:

  Character: Name of the statistic to calculate; either `"cor"`
  (default) or `"cov"`.

## Value

A numeric `matrix`.

## See also

[`terra::layerCor()`](https://rspatial.github.io/terra/reference/layerCor.html),
[`stats::cor()`](https://rdrr.io/r/stats/cor.html),
[`stats::cov()`](https://rdrr.io/r/stats/cor.html)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data
madChelsa <- fastData("madChelsa")

# Convert a SpatRaster to a GRaster:
chelsa <- fast(madChelsa)

# Correlation
layerCor(chelsa, "cor")

# Covariance
layerCor(chelsa, "cov")

}
```

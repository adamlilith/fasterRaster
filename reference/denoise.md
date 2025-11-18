# Remove or retain "noise" in a raster using PCA

`denoise()` applies a principal component analysis (PCA) to layers of a
`GRaster`, then uses the PCA to predict values back to a raster. This
retains only coarse-scale trends, thereby removing "noise" (locally
extreme values that fall far from a PC axis).

`noise()` does the opposite by first constructing the PCA, predicting
values back to the raster, then subtracting these values from the
original, removing coarse-scale trends and thereby leaving "noise".

## Usage

``` r
# S4 method for class 'GRaster'
denoise(x, scale = FALSE, percent = 80)

# S4 method for class 'GRaster'
noise(x, scale = FALSE, percent = 80)
```

## Arguments

- x:

  A `GRaster` with two or more layers.

- scale:

  Logical: If `TRUE`, input layers will be rescaled by dividing each
  layer by its overall population standard deviation. Note that rasters
  will always be centered (have their mean subtracted from values).
  Centering and scaling is recommended when rasters values are in
  different units. The default is `FALSE` (do not scale).

- percent:

  Numeric integer or integer in the range 50 to 99 (default is 80):
  Minimum total variation explained in the retained PC axes. Higher
  values will cause `denoise()` to remove less noise, and `noise()` to
  return less noise. If this value to too low to retain even one axis,
  the function will fail with a message to that effect.

## Value

A multi-layer `GRaster` with one layer per input.

## See also

[`princomp()`](https://github.com/adamlilith/fasterRaster/reference/princomp.md),
[`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html), **GRASS**
manual page for tool `i.pca` (see `grassHelp("i.pca")`)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Climate raster:
madChelsa <- fastData("madChelsa")

# Convert a SpatRaster to a GRaster:
chelsa <- fast(madChelsa)

### Denoise:
quiet <- denoise(chelsa, scale = TRUE)

compare1 <- c(chelsa[["bio1"]], quiet[["bio1"]])
plot(compare1)

compare2 <- c(chelsa[["bio7"]], quiet[["bio7"]])
plot(compare2)

### Noise:
loud <- noise(chelsa, scale = TRUE)

compare1 <- c(chelsa[["bio1"]], loud[["bio1"]])
plot(compare1)

compare2 <- c(chelsa[["bio7"]], loud[["bio7"]])
plot(compare2)

}
```

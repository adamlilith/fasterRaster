# Retrieve a principal components model from a PCA GRaster

Retrieve a principal components model from a PCA GRaster

## Usage

``` r
pcs(x)
```

## Arguments

- x:

  A `GRaster` created by
  [`princomp()`](https://github.com/adamlilith/fasterRaster/reference/princomp.md)

## Value

An object of class `prcomp`.

## See also

[`princomp()`](https://github.com/adamlilith/fasterRaster/reference/princomp.md),
[`terra::princomp()`](https://rspatial.github.io/terra/reference/princomp.html),
tool `i.pca` in **GRASS**

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Climate raster:
madChelsa <- fastData("madChelsa")

# Convert a SpatRaster to a GRaster:
chelsa <- fast(madChelsa)

# Generate raster with layers representing principal component predictions:
pcRast <- princomp(chelsa, scale = TRUE)
plot(pcRast)

# Get information on the PCA:
prinComp <- pcs(pcRast)

prinComp
summary(prinComp)
plot(prinComp)

}
```

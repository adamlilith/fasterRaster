# Create red-green-blue plot from a raster with RGB layers

This function takes as its main argument a `GRaster` with at least three
layers typically representing red, green, and blue components (plus
possibly an "alpha", or transparency layer). As with
[`plot()`](https://github.com/adamlilith/fasterRaster/reference/plot.md),
this function is somewhat of a hack in that it downsamples the layers to
a coarser resolution using
[`aggregate()`](https://github.com/adamlilith/fasterRaster/reference/aggregate.md),
saves the raster to disk, then uses
[`terra::plotRGB()`](https://rspatial.github.io/terra/reference/plotRGB.html)
to do the actual plotting.

## Usage

``` r
# S4 method for class 'GRaster'
plotRGB(x, r = 1, g = 2, b = 3, a = NULL, simplify = TRUE, ...)
```

## Arguments

- x:

  A `GRaster`. Values must be in the range from 0 to 255.

- r, g, b:

  Either a numeric integer or the
  [`names()`](https://github.com/adamlilith/fasterRaster/reference/names.md)
  of layers representing red, green, and blue components.

- a:

  Either `NULL` (default), or a numeric integer or the
  [`names()`](https://github.com/adamlilith/fasterRaster/reference/names.md)
  of a layer representing transparency.

- simplify:

  Logical: If `TRUE` (default), then downsample the `GRaster` before
  plotting. This can save time for very dense rasters.

- ...:

  Arguments to pass to
  [`terra::plotRGB()`](https://rspatial.github.io/terra/reference/plotRGB.html).

## Value

Nothing (makes a plot).

## See also

[`terra::plotRGB()`](https://rspatial.github.io/terra/reference/plotRGB.html),
[`plot()`](https://github.com/adamlilith/fasterRaster/reference/plot.md),
[`compositeRGB()`](https://github.com/adamlilith/fasterRaster/reference/compositeRGB.md)

## Examples

``` r
if (grassStarted()) {

# Example data
madElev <- fastData("madElev") # elevation raster
madLANDSAT <- fastData("madLANDSAT") # multi-layer raster
madRivers <- fastData("madRivers") # lines vector

# Convert SpatRaster to GRaster and SpatVector to GVector
elev <- fast(madElev)
rivers <- fast(madRivers)
landsat <- fast(madLANDSAT)

# Plot:
plot(elev)
plot(rivers, add = TRUE)

# Histograms:
hist(elev)
hist(landsat)

# Plot surface reflectance in RGB:
plotRGB(landsat, 3, 2, 1) # "natural" color
plotRGB(landsat, 4, 1, 2, stretch = "lin") # emphasize near-infrared (vegetation)

# Make composite map from RGB layers and plot in grayscale:
comp <- compositeRGB(r = landsat[[3]], g = landsat[[2]], b = landsat[[1]])
grays <- paste0("gray", 0:100)
plot(comp, col = grays)

}
```

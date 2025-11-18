# Plot a histogram of raster values

This function creates a histogram of values in `GRaster`. The function
is modeled after
[`graphics::hist()`](https://rdrr.io/r/graphics/hist.html), but actually
uses [`graphics::barplot()`](https://rdrr.io/r/graphics/barplot.html).

## Usage

``` r
# S4 method for class 'GRaster'
hist(x, layer, maxnl = 16, bins = 30, freq = TRUE, ...)
```

## Arguments

- x:

  A `GRaster`.

- layer:

  Character, numeric, or integer: Indicates which layer of a multi-layer
  `GRaster` for which to plot a histogram. The layer can be identified
  using its
  [`terra::name()`](https://rspatial.github.io/terra/reference/names.html)
  (character) or index (numeric or integer). If this is missing, then up
  to `maxnl` layers are plotted.

- maxnl:

  Maximum number of layers for which to create histograms. This is 16 by
  default, but ignored if `layer` is defined.

- bins:

  Positive numeric integer: Number of bins in which to divide values of
  a raster with continuous values. For `integer` and categorical
  rasters, each value is tallied.

- freq:

  Logical: If `TRUE` (default), plot the frequency of values. If
  `FALSE`, plot the density of values (i.e., the number in each bin
  divided by the total number of cells with non-`NA` values).

- ...:

  Arguments to pass to
  [`graphics::barplot()`](https://rdrr.io/r/graphics/barplot.html).

## Value

A named list of `data.frame`s (invisibly), one per layer plotted, and
creates a graph.

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

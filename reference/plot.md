# Display a raster or vector

`plot()` displays a `GRaster` or `GVector`.

This function is essentially a hack, as it it not possible to dependably
call the appropriate **GRASS** modules and display a raster or vector
without potential confusion on the user side. Instead, this function 1)
simplifies the focal `GRaster` or `GVector` to make it smaller when
saved to disk; 2) writes the object to disk; 3) (internally) creates a
`SpatRaster` or `SpatVector` object; then 4) plots the object using
[`terra::plot()`](https://rspatial.github.io/terra/reference/plot.html).
Thus, if you are interested in making maps, it will always be faster to
make them directly with **terra** or **sf**.

## Usage

``` r
# S4 method for class 'GRaster,missing'
plot(x, y, simplify = TRUE, ...)

# S4 method for class 'GVector,missing'
plot(x, y, maxGeoms = 10000, ...)
```

## Arguments

- x:

  A `GRaster` or `GVector`.

- y:

  Missing–leave as empty.

- simplify:

  Logical: If `TRUE` (default) and the raster has an x- and/or
  y-resolution that is greater than the screen resolution, then
  [`aggregate()`](https://github.com/adamlilith/fasterRaster/reference/aggregate.md)
  will be applied to the raster before it is saved to reduce the time it
  takes to save the raster.

- ...:

  Other arguments to send to
  [`terra::plot()`](https://rspatial.github.io/terra/reference/plot.html).

- maxGeoms:

  Positive integer (vectors only): Maximum number of features before
  vector simplification is applied before saving to disk then creating a
  `SpatVector` for plotting. The default is 10000.

## Value

Nothing (displays a raster or vector).

## See also

[`terra::plot()`](https://rspatial.github.io/terra/reference/plot.html)

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

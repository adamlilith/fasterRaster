# Combine red, green, and blue color bands to make a composite GRaster

This function takes as arguments three rasters typically representing
red, green, and blue color bands, and returns a single raster with
values based on their combination. Typically, this raster should be
plotted in grayscale.

## Usage

``` r
# S4 method for class 'GRaster'
compositeRGB(r, g = NULL, b = NULL, levels = 256, dither = FALSE)
```

## Arguments

- r, g, b:

  Either:

  - One `GRaster` with one band each for `r`, `g`, or `b` representing
    red, green, and blue color bands; or

  - `r` is single `GRaster` with 3 bands (R, G, and B bands), and `g`
    and `b` are `NULL`.

- levels:

  Either a single value that is an integer, or a vector of integers:
  Number of levels of red, green, and blue intensities represented in
  `r`, `g`, and `b`. If a single value is supplied, it is assumed that
  all three have the same number of levels. If three values are
  supplied, then they correspond to the R, G, and B bands. The default
  is 256 (assume that R, G, and B rasters have values between 0 and
  255).

- dither:

  Logical: If `TRUE`, apply Floyd-Steinberg dithering. Default is
  `FALSE`.

## Value

A `GRaster`.

## See also

[`plotRGB()`](https://github.com/adamlilith/fasterRaster/reference/plotRGB.md),
[`terra::plotRGB()`](https://rspatial.github.io/terra/reference/plotRGB.html),
**GRASS** manual page for tool `r.composite` (see
`grassHelp("r.composite")`)

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

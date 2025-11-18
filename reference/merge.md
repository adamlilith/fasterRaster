# Combine two or more rasters with different extents and fill in NAs

`merge()` combines two or more `GRaster`s, possibly with different
extents, into a single larger `GRaster`. Where the same cell has
different values in each raster, the value of the first raster's cell is
used. If this is `NA`, then the value of the second raster's cell is
used, and so on.

## Usage

``` r
# S4 method for class 'GRaster,GRaster'
merge(x, y, ...)
```

## Arguments

- x, y, ...:

  `GRaster`s.

## Value

A `GRaster`.

## See also

[`terra::merge()`](https://rspatial.github.io/terra/reference/merge.html),
[`terra::mosaic()`](https://rspatial.github.io/terra/reference/mosaic.html),
and **GRASS** tool `r.patch`

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Example data
madElev <- fastData("madElev")
madCoast4 <- fastData("madCoast4")
madCoast4 <- vect(madCoast4)

# For the example, crop the elevation raster to two communes
madAnt <- madCoast4[madCoast4$NAME_4 == "Antanambe", ]
madMan <- madCoast4[madCoast4$NAME_4 == "Manompana", ]

elevAnt <- crop(madElev, madAnt)
elevMan <- crop(madElev, madMan)

plot(madElev)
plot(elevAnt, col = "red", legend = FALSE, add = TRUE)
plot(elevMan, col = "blue", legend = FALSE, add = TRUE)

# Convert a SpatRaster to a GRaster
ant <- fast(elevAnt)
man <- fast(elevMan)

# merge
antMan <- merge(ant, man)
plot(antMan, main = "Antman!")

}
```

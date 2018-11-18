# fasterRaster
Faster raster processing in R using GRASS GIS

This package uses OS-Geo's [GRASS GIS Version 7](https://grass.osgeo.org/grass7/) to speed up some commonly used raster operations. Most/all of these operations can be done using the **raster** package by Robert Hijmans.  However, when the input raster is very large in memory, functions in that package can take a long time (days!) and fail. The **fasterRaster** package attempts to address these problems by calls to GRASS which is faster for large rasters. To use **fasterRaster** Version 7 of GRASS must be installed (the sub-version is unimportant) on the local system you are using.

To install this package and its dependencies, do the following:

`install.packages('devtools') # if you haven't done this already`

`library(devtools)`

`install_github('adamlilith/omnibus')`

`install_github('adamlilith/enmSdm')`

`install_github('adamlilith/fasterRaster')`

## *Faster* raster processing functions ##
* `fasterBufferRast`: Add buffer to cells in a raster (using GRASS).
* `fasterFocal`: Faster focal calculations (using multi-core).
* `fasterFragmentation`: Fragmentation indices following Riitters et al. (2000 Conservation Ecology 4:3; using multi-core).
* `fasterLongLatRasters`: Create rasters with values equal to cell longitude and latitude (using GRASS).
* `fasterProjectRaster`: Project and resample raster (using GRASS).
* `fasterQuantile`: Quantiles of values in a raster (using GRASS).
* `fasterRastDistance`: Distance from cells with `NA`s to closest non-`NA` cell (or the inverse of this) (using GRASS).
* `fasterRasterize`: Convert vector to a raster (using GRASS).
* `fasterTerrain`: Slope, aspect, and curvature (using GRASS).
* `fasterVectorize`: Convert raster to spatial points, lines, or polygons (using GRASS).
* `fasterVectToRastDistance`: Distance between raster cells and a vector (using GRASS).

## Normal-speed raster processing ##
* `fragmentation`: Calculate landscape fragmentation indices as per Riitter et al. (2000 Conservation Ecology 4:3)

## Utility functions ##
* `exportRasterToGrass`: Export raster to an open GRASS session with support for large rasters.

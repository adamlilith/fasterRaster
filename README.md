# fasterRaster

This package uses [GRASS GIS Verison 7](https://grass.osgeo.org/grass7/) to speed up some commonly used raster operations. Most/all of these operations can be done using the **raster** package by Robert Hijmans.  However, when the input raster is very large in memory, functions in that package can take a long time (days!) and fail. The **fasterRaster** package attempts to address these problems by calls to GRASS which is faster for large rasters. To use **fasterRaster** Version 7 of GRASS must be installed (the sub-version is unimportant) on the local system you are using.

## *Faster* raster processing functions ##
* `fasterLongLatRasters`: Create rasters with values equal to cell longitude and latitude.
* `fasterProjectRaster`: Project and resample raster.
* `fasterRastDistance`: Distance from cells with `NA`s to closest non-`NA` cell (or the inverse of this).
* `fasterRasterize`: Convert vector to a raster.
* `fasterTerrain`: Slope, aspect, and curvature.
* `fasterVectorize`: Convert raster to spatial points, lines, or polygons
* `fasterVectToRastDistance`: Distance between raster cells and a vector.

## Normal-speed raster processing ##
* `frag`: Calculate landscape fragmentation indices as per Riitter et al. (2000 Conservation Ecology 4:3)

## Utility functions ##
* `exportRasterToGrass`: Export raster to an open GRASS session with support for large rasters.

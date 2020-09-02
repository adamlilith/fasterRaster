# fasterRaster
Faster raster processing in R using GRASS GIS

This package uses the stand-alone installer of Open Source Geospatial's [GRASS GIS Version 7](https://grass.osgeo.org/grass7/) to speed up some commonly used raster operations. Most of these operations can be done using the **raster** package by Robert Hijmans.  However, when the input raster is very large in memory, functions in that package can take a long time and fail. The **fasterRaster** package attempts to address these problems by calls to GRASS which is faster for large rasters. The successor to the **raster** package, **terra**, is also much faster and may be the better solution for functions that this package implements.

To use **fasterRaster** Version 7.8 of GRASS must be installed on the local system you are using.

To install `fasterRaster` and its dependencies, just do the following:  
`remotes::install_github('adamlilith/fasterRaster')`  

NB: If for some reason this command does not work, you can install the package by downloading the latest zip/tar file from the `zipTarFiles` directory and installing the package manually.

### *Faster* raster processing functions ##
* `fasterBufferRast`: Add buffer to cells in a raster (using GRASS).
* `fasterContour`: Calculate contour vectors from a raster (using GRASS).
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

### Normal-speed raster processing ##
* `fragmentation`: Calculate landscape fragmentation indices as per Riitter et al. (2000 Conservation Ecology 4:3)

### Utility functions ##
* `exportRasterToGrass`: Export raster to an open GRASS session with support for large rasters.

## Getting started ##
To use **fasterRaster** you will need to install [GRASS version 7.8](https://grass.osgeo.org/) on your operating system. You will need to use the stand-alone installer, not the Open Source Geospatial (OS Geo) installer (as per Roger Bivand, the main developer and maintainer of the **rgrass7** package on which this package depends). You will also need to know the install path for the program. On my machine (Windows), it is `C:/Program Files/GRASS GIS 7.8`, so before running any **faster** functions that need GRASS I need to define:

`grassDir <- 'C:/Program Files/GRASS GIS 7.8'`  

Let's get started! We'll do a simple operation in which we calculate the distance to rivers (represented by a spatial lines vector object) and burn the distance values into a raster. To do this, we'll be using maps representing the middle of the eastern coast of Madagascar.

`library(fasterRaster)`  
`data(madForest2000)`  
`data(madRivers)`  

`# calculate and plot distance to rivers`  
`distToRiver <- fasterVectToRastDistance(madForest2000, madRivers, grassDir=grassDir)`  
`plot(distToRiver, main='Distance to Rivers (m)')`  
`plot(madRivers, col='blue', add=TRUE)`  

`# calculate quantiles of distance to rivers`  
`quants <- fasterQuantile(distToRiver, probs=c(0.05, 0.5, 0.95), grassDir=grassDir)`  
`quants`  

### Chaining functions for speed ###
That was easy enough. Under the hood, many of the `faster` functions are:  
1) starting a GRASS session;  
2) writing a raster and/or vector to that session;  
3) doing the requested operation;  
4) and then exporting the raster/vector output to R.  

Note that you really probably aren't interested in steps 1 and 4, but they create a computing overhead.  Thus, it is possible to chain operations together so that they use the same GRASS session by keeping the rasters/vectors in GRASS and then exporting them when you need them in the end.  

Here's an example in which we'll chain the `fasterVectToRastDistance` and `fasterQuantile` functions together. The latter will return

`distToRiver <- fasterVectToRastDistance(madForest2000, madRivers, grassDir=grassDir, grassToR=FALSE)`  
`quants <- fasterQuantile('distToVect', probs=c(0.05, 0.5, 0.95), grassDir=grassDir, alreadyInGrass=TRUE)`  
`quants`  

This was faster because we told `fasterVectToRastDistance` *not* to export the raster to **R**. We then told `fasterQuantile` to look in the current GRASS session and use the raster named `distToVect`. How did we know it would be called this?  Because the function *fasterVectToRastDistance* always names its output raster in GRASS `distToVect` (see help for `fasterVectToRastDistance`). Other functions that use GRASS will have different names for their output vectors/rasters.

You can see that by chaining a series of `faster~` functions together, the process can be made faster because all of the operations are done in GRASS with less back-and-forth between GRASS and R.  The one exception to this is that some **faster~** functions do not use GRASS (e.g., `fasterFragmentation`, `fasterFocal`, and `fasterCalc`), so you can't use this trick.

### The generic *faster* function ###
The `faster` function is a generic wrapper for GRASS modules. You can use it to call many of the modules in GRASS.  It may not always work, but it simplifies the task of initiating a GRASS instance, importing the raster/vector, and executing the call:

`data(madForest2000)`  
`latRast <- faster('r.latlong', rast=madForest2000, outType='rast', flags=c('quiet', 'overwrite'), grassDir=grassDir)`  
`longRast <- faster('r.latlong', rast=madForest2000, outType='rast', flags=c('quiet', 'overwrite', 'l'), grassDir=grassDir)`  
`ll1 <- stack(latRast, longRast)`  

This is the same as:

`ll2 <- fasterLongLatRasters(madForest2000, grassDir=grassDir)`

Here is an example of chaining with the *faster* function. The second function uses the GRASS session initiated by the first function. It then uses the raster created in the GRASS session by the first function as the input for its module.

`latRast <- faster('r.latlong', rast=madForest2000, outType='rast', outName='lat', flags=c('quiet', 'overwrite'), grassDir=grassDir)`  
`longRast <- faster('r.latlong', input='lat', outType='rast', outName='long', flags=c('quiet', 'overwrite', 'l'), init=FALSE, grassDir=grassDir)`  
`ll3 <- stack(latRast, longRast)`  

~ Adam

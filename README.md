# fasterRaster
Faster raster processing in R using GRASS GIS

<img align="right" src="fasterRaster.png" height="190"/>  

This package uses the stand-alone installer of Open Source Geospatial's [GRASS GIS Version 7](https://grass.osgeo.org/grass7/) to speed up some commonly used raster operations. Most of these operations can be done using the **raster** or **terra** packages by Robert Hijmans.  However, when the input raster is very large in memory, in some cases functions in those packages can take a long time and fail. The **fasterRaster** package attempts to address these problems by calls to GRASS which is faster for large rasters. The successor to the **raster** package, **terra**, is much faster and may be the better solution for functions that this package implements. However, in some cases **fasterRaster** is still faster!

To use **fasterRaster** Version 7.8 of GRASS must be installed on the local system you are using. Please note version 8 of GRASS (forthcoming?) has not been tested with this package, but it may work, and that in the meantime you'll get warning messages about that when running many of these functions.

To install `fasterRaster` and its dependencies, just do the following:  
`remotes::install_github('adamlilith/fasterRaster', dependencies=TRUE)`  

NB: If for some reason this command does not work, you can install the package by downloading the latest zip/tar file from the `zipTarFiles` directory and installing the package manually.

## `raster`, `terra`, `sp`, and `sf` package objects ##

**fasterRaster** "works" using rasters from the **raster** package and vectors from the **sp** package. However, as of June 2022, it now supports objects that are of class `SpatRaster`s or `SpatVector`s (**terra** package), or `sf` (**sf** package). It does this cheaply by converting these kind of objects to `raster` or `sp` objects, then sending them to GRASS. Since most of the processing is done in GRASS anyway, there is not much overhead to accommodate these "advanced" formats. The output, though, is still in `raster` or `sp` format. You can re-covert these back to the desired class using:

`terraRastFromRasterRaster <- as(terraRastObject, 'raster') # from raster to terra`  
`terraVectFromSp <- terra::vect(terraVectorObject) # from raster to terra`  
`sfVectFromSp <- sf::st_as_sf(spObject) # from sp to sf`  

## Functions ###

### *Faster* raster processing functions ##
* `fasterBufferRast`: Add buffer to cells in a raster (using GRASS).
* `fasterContour`: Calculate contour vectors from a raster (using GRASS).
* `fasterConvertDegree`: Convert degrees from GRASS format (0 = east, 90 = north) to standard (0 = north, 90 = east) (using GRASS).
* `fasterFocal`: Faster focal calculations (using multi-core; see also `fasterMapcalc`).
* `fasterFragmentation`: Fragmentation indices following Riitters et al. (2000 Conservation Ecology 4:3; using multi-core).
* `fasterLongLatRasters`: Create rasters with values equal to cell longitude and latitude (using GRASS).
* `fasterMapcalc`: Apply user-defined function to one or more rasters (using GRASS; see also `fasterFocal`).
* `fasterProjectRaster`: Project and resample raster (using GRASS).
* `fasterQuantile`: Quantiles of values in a raster (using GRASS).
* `fasterRastDistance`: Distance from cells with `NA`s to closest non-`NA` cell (or the inverse of this) (using GRASS).
* `fasterRasterize`: Convert vector to a raster (using GRASS).
* `fasterSurfFract`: Generate a raster with a fractal pattern (using GRASS).
* `fasterTerrain`: Slope, aspect, and curvature (using GRASS).
* `fasterTopidx`: Topographic wetness index (using GRASS).
* `fasterVectorize`: Convert raster to spatial points, lines, or polygons (using GRASS).
* `fasterVectToRastDistance`: Distance between raster cells and a vector (using GRASS).

### Generic "faster" operations: ##
* `faster`: Generic call to a GRASS module.

### Normal-speed raster processing ##
* `fragmentation`: Calculate landscape fragmentation indices as per Riitter et al. (2000 Conservation Ecology 4:3)

### Utility functions ##
* `exportRastToGrass`: Export raster to an open GRASS session.
* `exportVectToGrass`: Export vector to an open GRASS session.
* `initGrass`: Initialize a GRASS session using a raster or vector as a template.

## Getting started ##
To use **fasterRaster** you will need to install [GRASS version 7.8](https://grass.osgeo.org/) on your operating system. You will need to use the stand-alone installer, not the Open Source Geospatial (OS Geo) installer (as per Roger Bivand, the main developer and maintainer of the **rgrass7** package on which this package depends). You will also need to know the install path for the program. On my machine (Windows), it is `C:/Program Files/GRASS GIS 7.8`, so before running any **faster** functions that need GRASS I need to define:

`grassDir <- 'C:/Program Files/GRASS GIS 7.8'`  

Let's get started! We'll do a simple operation in which we calculate the distance to rivers (represented by a spatial lines vector object) and burn the distance values into a raster. To do this, we'll be using maps representing the middle of the eastern coast of Madagascar.

`library(fasterRaster)`  
`library(raster)`  
`library(sp)`  

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

Here's an example in which we'll chain the `fasterVectToRastDistance` and `fasterQuantile` functions together.  

`distToRiver <- fasterVectToRastDistance(madForest2000, madRivers, grassDir=grassDir, grassToR=FALSE, outGrassName='distToVect')`  
`quants <- fasterQuantile('distToVect', probs=c(0.05, 0.5, 0.95), grassDir=grassDir, alreadyInGrass=TRUE)`  
`quants`  

The first function imports the raster and vector, does its calculations, but does not export the output raster to R. Rather, the output raster stays in the GRASS session and is named `distToVect`. In the second function we set the argument `alreadyInGrass` to `TRUE` so it knows the raster is already in GRASS, and we tell it to use `distToVect` so it know under what name to find the raster.  

You can see that by chaining a series of `faster~` functions together, the process can be made faster because all of the operations are done in GRASS with less back-and-forth between GRASS and R.  The one exception to this is that some `faster~` functions do not use GRASS (e.g., `fasterFragmentation`, `fasterFocal`, and `fasterCalc`), so you can't use this trick.

### The generic *faster* function ###
The `faster` function is a generic wrapper for GRASS modules. You can use it to call many of the modules in GRASS.  It may not always work, but it simplifies the task of initiating a GRASS instance, importing the raster/vector, and executing the call:

`data(madForest2000)`  
`latRast <- faster('r.latlong', rast=madForest2000, outType='rast', flags=c('quiet', 'overwrite'), grassDir=grassDir)`  
`longRast <- faster('r.latlong', rast=madForest2000, outType='rast', flags=c('quiet', 'overwrite', 'l'), grassDir=grassDir)`  
`ll1 <- stack(latRast, longRast)`  

Bye the way, this is the same as:

`ll2 <- fasterLongLatRasters(madForest2000, grassDir=grassDir)`

However, you can use `faster` to call GRASS functions that do not have a dedicated functon in **fasterRaster**, so `faster` is very flexible!

Here is an example of chaining with the `faster` function. The second function uses the GRASS session initiated by the first function. It then uses the raster created in the GRASS session by the first function as the input for its module.

`latRast <- faster('r.latlong', rast=madForest2000, outType='rast', output='latitude', flags=c('quiet', 'overwrite'), grassDir=grassDir)`  
`longRast <- faster('r.latlong', input='latitude', outType='rast', output='longitude', flags=c('quiet', 'overwrite', 'l'), grassDir=grassDir, alreadyInGrass=TRUE)`  
`ll3 <- stack(latRast, longRast)`  
`plot(ll3)`  

### Citation ###
As of October, 2020, there is not a package-specific citation for **fasterRaster**, but the package was first used in:

Morelli*, T.L., Smith*, A.B., Mancini, A.N., Balko, E. A., Borgenson, C., Dolch, R., Farris, Z., Federman, S., Golden, C.D., Holmes, S., Irwin, M., Jacobs, R.L., Johnson, S., King, T., Lehman, S., Louis, E.E. Jr., Murphy, A., Randriahaingo, H.N.T., Lucien, Randriannarimanana, H.L.L., Ratsimbazafy, J., Razafindratsima, O.H., and Baden, A.L. 2020. The fate of Madagascar’s rainforest habitat.  Nature Climate Change 10:89-96. * Equal contribution https://doi.org/10.1038/s41558-019-0647-x

Abstract. Madagascar has experienced extensive deforestation and overharvesting, and anthropogenic climate change will compound these pressures. Anticipating these threats to endangered species and their ecosystems requires considering both climate change and habitat loss effects. The genus Varecia (ruffed lemurs), which is composed of two Critically Endangered forest-obligate species, can serve as a status indicator of the biodiverse eastern rainforest of Madagascar. Here, we combined decades of research to show that the suitable habitat for ruffed lemurs could be reduced by 29–59% from deforestation, 14–75% from climate change (representative concentration pathway 8.5) or 38–93% from both by 2070. If current protected areas avoid further deforestation, climate change will still reduce the suitable habitat by 62% (range: 38–83%). If ongoing deforestation continues, the suitable habitat will decline by 81% (range: 66–93%). Maintaining and enhancing the integrity of protected areas, where rates of forest loss are lower, will be essential for ensuring persistence of the diversity of the rapidly diminishing Malagasy rainforests.

~ Adam

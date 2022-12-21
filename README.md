# fasterRaster
<!-- badges: start -->

[![R-CMD-check](https://github.com/adamlilith/fasterRaster/workflows/R-CMD-check/badge.svg)](https://github.com/adamlilith/fasterRaster/actions)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![cran version](https://www.r-pkg.org/badges/version/fasterRaster)](https://cran.r-project.org/package=fasterRaster)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/fasterRaster?color=yellow)](https://r-pkg.org/pkg/fasterRaster)
![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/fasterRaster?color=lightgrey)
[![GPLv3 license](https://img.shields.io/badge/License-GPLv3-blue.svg)](http://perso.crans.org/besson/LICENSE.html)

<!-- badges: end -->



Faster raster processing in **R** using **GRASS GIS**

<img align="right" src="fasterRaster.png" height="230"/>  

**fasterRaster** uses the stand-alone installer of Open Source Geospatial's [GRASS GIS Version 8](https://grass.osgeo.org/rgrass/) to speed up some commonly used raster and vector operations. Most of these operations can be done using the **raster** or newer **terra** packages by Robert Hijmans, or the **rgeos** or newer **sf** packages.  However, when the input raster or vector is very large in memory, in some cases functions in those packages can take a long time and fail. The **fasterRaster** package attempts to address these problems by calls to **GRASS** which is faster. Please note that **terra** and **sf** may be faster and thus the better solution for functions that this package implements. However, in some cases **fasterRaster** is still faster!

# Getting started #

## Installation ###

You can install **fasterRaster** from CRAN.  Alternatively, you can get the latest development version from **GitHub** using:  

`remotes::install_github('adamlilith/fasterRaster', dependencies=TRUE)`  

To use **fasterRaster** you will also need to install [GRASS version 8+](https://grass.osgeo.org/) on your operating system. You will need to use the stand-alone installer, not the Open Source Geospatial (OS Geo) installer.

You will also need to know the install path for **GRASS**. On my machine (Windows), it is `C:/Program Files/GRASS GIS 8.2`. A friend whose computer was infected with Mac OS told me it was `"/Applications/GRASS-8.2.app/Contents/Resources"` for them (note the double quotes). So before running any `fasterRaster` functions that need **GRASS** you need to tell the functions where **GRASS** is. There are two ways to do this.  

First, you can pass this to each function that needs it. For example, you can call functions like this:  

`grassDir <- 'C:/Program Files/GRASS GIS 8.2' # PC`  
`grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # Mac`  
`grassDir <- '/usr/local/grass' # Linux... maybe`  

`output <- fasterSomething(various_input_arguments, grassDir = grassDir)`  

This is fine if you need to just run a few functions, but it can be cumbersome. The other way is to set an "option" equal to the install path:  

`grassDir <- 'C:/Program Files/GRASS GIS 8.2'`  
`options(grassDir = grassDir)`  

Now, whenever you run a **fasterRaster** function that needs `grassDir`, it will just look for it there. So you can just do:

`output <- fasterSomething(various_input_arguments)`  

...which is much simpler.  

## An example ##

Let's get started! We'll do a simple operation in which we calculate the distance to rivers (represented by a spatial lines vector object) and burn the distance values into a raster. To do this, we'll be using maps representing the middle of the eastern coast of Madagascar.

`library(fasterRaster)`  
`library(terra)`  
`library(sf)`  

`# please change the next line to fit your installation path for \code{GRASS}`  
`grassDir <- 'C:/Program Files/GRASS GIS 8.2'`  
`options(grassDir = grassDir)`  

`# load a raster to serve as a template and spatial vector file with rivers`  
`madForest2000 <- fasterData('madForest2000')`  
`data(madRivers)`  

`# calculate and plot distance to rivers`  
`distToRiver <- fasterVectToRastDistance(madForest2000, madRivers)`  
`plot(distToRiver, main='Distance to Rivers (m)')`  
`plot(madRivers, col='blue', add=TRUE)`  

`# calculate quantiles of distance to rivers`  
`quants <- fasterQuantile(distToRiver, probs=c(0.05, 0.5, 0.95))`  
`quants`  

## Chaining functions for speed ##
That was easy enough. Under the hood, many of the `faster` functions are:  
1) starting a **GRASS** session;  
2) writing a raster and/or vector to that session;  
3) doing the requested operation;  
4) and then exporting the raster/vector output to**R**.  

Note that you really probably aren't interested in steps 1 and 4, but they create a computing overhead.  However, it is possible to "chain" functions together so that they use the same **GRASS** session by keeping the rasters/vectors in **GRASS** and then exporting them when you need them in the end.  

Here's an example in which we'll chain the `fasterVectToRastDistance` and `fasterQuantile` functions together.  

`distToRiver <- fasterVectToRastDistance(madForest2000, madRivers, grassToR=FALSE, outGrassName='distToVect')`  
`quants <- fasterQuantile(rast='distToVect', probs=c(0.05, 0.5, 0.95))`  
`quants`  

The first function imports the raster and vector, and does its calculations, but does not export the output raster to **R**. Rather, the output raster stays in the **GRASS** session and is named `distToVect`. In the second function the first argument is `distToVect`, the name of the raster we just created in the first function. This tells the second function to look in the **GRASS** session for the raster, not in**R**. If we had instead set `rast` in the second function equal to an actual raster, then the function would have imported the raster into a **GRASS**S session, which takes some time to do.

You can see that by chaining a series of `faster~` functions together, the process can be made faster because all of the operations are done in **GRASS** with less back-and-forth between **GRASS** and **R**.  The one exception to this is that some `faster~` functions do not use **GRASS** (e.g., `fasterFragmentation` and `fasterFocal`), so you can't use this trick.

## The generic `faster()` function ##
The `faster` function is a generic wrapper for **GRASS** modules. You can use it to call many of the modules in **GRASS**.  It may not always work, but it simplifies the task of initiating a **GRASS** instance, importing the raster/vector, and executing the call:

`grassDir <- 'C:/Program Files/GRASS GIS 8.2'`  
`options(grassDir = grassDir)`  

`madForest2000 <- fasterData('madForest2000')`  
`latRast <- faster('r.latlong', rast=madForest2000, outType='rast', flags=c('quiet', 'overwrite'))`  
`longRast <- faster('r.latlong', rast=madForest2000, outType='rast', flags=c('quiet', 'overwrite', 'l'))`  
`ll1 <- stack(latRast, longRast)`  

By the way, this is the same as:

`ll2 <- fasterLongLatRasts(madForest2000)`

However, you can use `faster` to call **GRASS** functions that do not have a dedicated functon in **fasterRaster**, so `faster` is very flexible!

## Chaining with the `faster()` function ##

Here is an example of chaining with the `faster` function. The second function uses the **GRASS** session initiated by the first function. It then uses the raster created in the **GRASS** session by the first function as the input for its module.

`latRast <- faster('r.latlong', rast=madForest2000, outType='rast', output='latitude', flags=c('quiet', 'overwrite'))`  
`longRast <- faster('r.latlong', input='latitude', outType='rast', output='longitude', flags=c('quiet', 'overwrite', 'l'))`  
`ll3 <- c(latRast, longRast)`  
`plot(ll3)`  

# Functions #

### Getting help
* Typing `?fasterRaster` will pull up a table of package contents (much like this).
* `fasterHelp`: Find equivalent functions in `fasterRaster`, `GRASS`, and `terra`.

### *Faster* raster processing functions ##
* `fasterBufferRast`: Add buffer to cells in a raster (using **GRASS**).
* `fasterContour`: Calculate contour vectors from a raster (using **GRASS**).
* `fasterConvertDegree`: Convert degrees from **GRASS** format (0 = east, 90 = north) to standard (0 = north, 90 = east) (using **GRASS**).
* `fasterFocal`: Faster focal calculations (using multi-core; see also `fasterApp`).
* `fasterFragmentation`: Fragmentation indices following**R**iitters et al. (2000 Conservation Ecology 4:3; using multi-core).
* `fasterLongLatRasts`: Create rasters with values equal to cell longitude and latitude (using **GRASS**).
* `fasterApp`: Apply user-defined function to one or more rasters (using **GRASS**; see also `fasterFocal`).
* `fasterProjectRast`: Project and resample raster (using **GRASS**).
* `fasterQuantile`: Quantiles of values in a raster (using **GRASS**).
* `fasterRastDistance`: Distance from cells with `NA`s to closest non-`NA` cell (or the inverse of this) (using **GRASS**).
* `fasterRasterize`: Convert vector to a raster (using **GRASS**).
* `fasterSurfFract`: Generate a raster with a fractal pattern (using **GRASS**).
* `fasterTerrain`: Slope, aspect, and curvature (using **GRASS**).
* `fasterTopidx`: Topographic wetness index (using **GRASS**).
* `fasterVectorize`: Convert raster to spatial points, lines, or polygons (using **GRASS**).
* `fasterVectToRastDistance`: Distance between raster cells and a vector (using **GRASS**).

### Generic "faster" operations: ##
* `faster`: Generic call to a **GRASS** module.

### Normal-speed raster processing ##
* `fragmentation`: Calculate landscape fragmentation indices as per**R**iitter et al. (2000 Conservation Ecology 4:3)

### Utility functions ##
* `exportRastToGrass`: Export raster to an open **GRASS** session.
* `exportVectToGrass`: Export vector to an open **GRASS** session.
* `initGrass`: Initialize a **GRASS** session using a raster or vector as a template.

### Data ###
All spatial data represents features in a portion of eastern Madagascar.
* `fasterData`: Load any of the spatial datasets
* `madCoast0` and `madCoast4`: Outlines of a portion of eastern Madagascar (`sf` spatial vectors)
* `madElev`: Elevation (`SpatRaster` raster)
* `madForest2000` and `madForest2014`: Forest cover in 2000 and 2014 (`SpatRaster` raster)
* `madRivers`: Major rivers (`sf` spatial vector)

# Citation #
As of October, 2020, there is not a package-specific citation for **fasterRaster**, but the package was first used in:

Morelli*, T.L., Smith*, A.B., Mancini, A.N., Balko, E. A., Borgenson, C., Dolch,**R**., Farris, Z., Federman, S., Golden, C.D., Holmes, S., Irwin, M., Jacobs,**R**.L., Johnson, S., King, T., Lehman, S., Louis, E.E. Jr., Murphy, A.,**R**andriahaingo, H.N.T., Lucien,**R**andriannarimanana, H.L.L.,**R**atsimbazafy, J.,**R**azafindratsima, O.H., and Baden, A.L. 2020. The fate of Madagascar’s rainforest habitat.  *Nature Climate Change* 10:89-96. * Equal contribution https://doi.org/10.1038/s41558-019-0647-x

*Abstract*. Madagascar has experienced extensive deforestation and overharvesting, and anthropogenic climate change will compound these pressures. Anticipating these threats to endangered species and their ecosystems requires considering both climate change and habitat loss effects. The genus Varecia (ruffed lemurs), which is composed of two Critically Endangered forest-obligate species, can serve as a status indicator of the biodiverse eastern rainforest of Madagascar. Here, we combined decades of research to show that the suitable habitat for ruffed lemurs could be reduced by 29–59% from deforestation, 14–75% from climate change (representative concentration pathway 8.5) or 38–93% from both by 2070. If current protected areas avoid further deforestation, climate change will still reduce the suitable habitat by 62% (range: 38–83%). If ongoing deforestation continues, the suitable habitat will decline by 81% (range: 66–93%). Maintaining and enhancing the integrity of protected areas, where rates of forest loss are lower, will be essential for ensuring persistence of the diversity of the rapidly diminishing Malagasy rainforests.




~ Adam

#' @name tutorial_getting_started
#'
#' @title Getting started with fasterRaster
#'
#' @description **fasterRaster** interfaces with **GRASS GIS** to process rasters and spatial vector data. It is intended as an add-on to the **terra** and **sf** packages, and relies heavily on them. For most rasters and vectors that are small or medium-sized in memory/disk, those packages will almost always be faster. They may also be faster for very large objects.  But when they aren't, **fasterRaster** can step in.
#'
#' ## Installing **fasterRaster**
#'
#' You probably already have **fasterRaster** installed on your computer, but if not, you can install the latest release version from CRAN using:
#' ```
#' install.packages("fasterRaster")
#' ```
#' and the latest development version using:
#' ```
#' remotes::install_github("adamlilith/fasterRaster", dependencies = TRUE)
#' ```
#' (You may need to install the `remotes` package first.)
#' 
#' ## Installing **GRASS GIS**
#'
#' **fasterRaster** uses **GRASS** to do its operations. To use **fasterRaster**, you will need to install **GRASS** using the "stand-alone" installer, available through the [GRASS GIS](https://grass.osgeo.org/). Be sure to *use the "stand-alone" installer, not the "OSGeo4W" installer!*
#'
#' ## Starting a **fasterRaster** session
#'
#' I recommend attaching the **data.table**, **terra**, and **sf** packages before attaching **fasterRaster** package to avoid function conflicts. The **data.table** package is not required, but you most surely will use at least one of the other two.
#' ```
#' library(data.table)
#' library(sf)
#' library(terra)
#' library(fasterRaster)
#' ```
#' Now, we need to start a **fasterRaster** "session", which makes a connection to **GRASS**. When you start **GRASS**, you create a "location". A "location" has a coordinate reference system (CRS).  All objects (rasters and vectors) in that "location" have to have the same CRS. However, these objects do not have to actually represent the same location on Earth--they just need to have the same CRS. To help differentiate, throughout **fasterRaster** documentation, we'll use "location" (in quotes) to refer to a **GRASS** location, and *location* (no quotes--only italicized this once) to refer to a place on Earth (or any other spherical heavenly body).
#'
#' To make your first location, use the [faster()] function. This function needs at least two arguments:
#' 1. `x`: A spatial object from which a coordinate reference system (CRS) can be obtained. This can be a `SpatRaster` or `SpatVector` (both from **terra**), an `sf` vector (**sf** package), or the Well-Known Text representation of a CRS. For our example, we'll use a raster that ships with **fasterRaster** that represents elevation along a portion of the eastern coast of Madagascar:
#' ```
#' madElev <- fastData("madElev") # get raster
#' madElev # look at raster properties
#' plot(madElev) # look at the raster
#' ```
#' 
#' 2. `grassDir`: The folder in which **GRASS** is installed on your system. Depending on your operating system and the version of **GRASS** installed, this will look something like:
#' ```
#' grassDir <- "C:/Program Files/GRASS GIS 8.4" # Windows
#' grassDir <- "/Applications/GRASS-8.3.app/Contents/Resources" # Mac
#' grassDir <- "/usr/local/grass" # Linux
#' ```
#' 
#' To initiate the **GRASS** connection, using [faster()]:
#' ````
#' faster(madElev, grassDir = grassDir)
#' ```
#' # Importing rasters and vectors using `fast()`
#'
#' In **fasterRaster**, rasters are called `GRaster`s and vectors are called `GVector`s. The easiest (but not always fastest) way to start using a `GRaster` or `GVector` is to convert it from one already in **R**. In the example below, we use a raster that comes with the **fasterRaster** package. We will convert the elevation `SpatRaster` we loaded into a `GRaster` using [fast()].
#' ```
#' elev <- fast(madElev)
#' elev
#' ```
#' You can see the properties of the `GRaster` representation of the `madElev` raster. Converting rasters that are already in **R** to `GRaster`s takes some time. Instead, you can load a raster directly from disk to a **fasterRaster** session using `fast()`. To do this, you just replace the argument in [fast()] with a string representing the folder path and file name of the raster you want to load into the session.
#' 
#' Now, let's create a `GVector`. The [fast()] function can take a `SpatVector` from the **terra** package, an `sf` object from the **sf** package, or a string representing the file path and file name of a vector file (e.g., a GeoPackage file or a shapefile).
#' ```
#' madRivers <- fastData("madRivers")
#' rivers <- fast(madRivers)
#' rivers
#' ```
#' ## Operations on `GRaster`s and `GVector`s
#'
#' You can do operations on these objects just like you would if they were `SpatRaster`s, `SpatVector`s, and `sf` objects. For example, you can use mathematical operators and functions:
#'
#' ```
#' elev_feet <- elev * 3.28084
#' elev_feet
#'
#' logElev <- log10(elev)
#' logElev
#' ```
#'
#' You can also use the many **fasterRaster** functions. In general, these functions have the same names as their **terra** counterparts and usually the same arguments. The following code creates a raster where cell values reflect the distance between them and the nearest river, then converts the output to a `SpatRaster` for plotting:
#' ```
#' dist <- distance(elev, rivers)
#' plot(dist)
#' ```
#' And that's how you get started! Now that you have a raster and a vector in your **fasterRaster** "location", you can start doing manipulations and analyses using any of the **fasterRaster** functions!  To see an annotated list of these functions, use `?fasterRaster`.
#' 
#' ## Exporting `GRaster`s and `GVector`s from a **GRASS** session
#'
#' You can convert a `GRaster` to a `SpatRaster` raster using [rast()]:
#' ```
#' terraRast <- rast(elev)
#' ```
#' To convert a `GVector` to the **terra** package's `SpatVector` format or to an `sf` vector, use [vect()] or [st_as_sf()]:
#' ```
#' terraVect <- vect(rivers)
#' sfVect <- st_as_sf(rivers)
#' ```
#' Finally, you can use [writeRaster()] and [writeVector()] to save **fasterRaster** rasters and vectors directly to disk. This will be faster than converting to another format then saving.
#'
#' ## Further reading
#' 
#' * The [GRASS GIS](https://grass.osgeo.org/) website is authoritative and contains the manual on all the **GRASS** functions used in this package.
#' * The Wiki on [how to run GRASS in R or R in GRASS](https://grasswiki.osgeo.org/wiki/R_statistics/rgrass) is a good place to start if you want to become a power-user of **GRASS** in **R**.
#' 
#' @keywords tutorial
NULL

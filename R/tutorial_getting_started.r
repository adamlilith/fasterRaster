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
#' install.packages('fasterRaster')
#' ```
#' and the latest development version using:
#' ```
#' remotes::install_github('adamlilith/fasterRaster', dependencies = TRUE)
#' ```
#' (You may need to install the `remotes` package first.)
#' 
#' ## Installing **GRASS GIS**
#'
#' **fasterRaster** uses **GRASS** to do its operations. To use **fasterRaster**, you will need to install **GRASS** using the "stand-alone" installer, available through the [GRASS GIS](https://grass.osgeo.org/). Be sure to *use the "stand-alone" installer, not the "OSGeo4W" installer!*
#'
#' Once you have installed **GRASS**, you will need to know where on your computer it was installed. The exact folder will depend on your operating system, the version installed, and perhaps other things, but in general, the install folder will look something like:
#'
#' Windows: `'C:/Program Files/GRASS GIS 8.2'`\cr
#' Mac: `"/Applications/GRASS-8.2.app/Contents/Resources"`\cr
#' Linux: `'/usr/local/grass'`\cr
#'
#' Whatever the name of this folder, it is passed to functions using an argument named `grassDir`. So, it is helpful to define `grassDir`, as in:
#' ```
#' grassDir <- 'C:/Program Files/GRASS GIS 8.2' # Windows
#' grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # Mac
#' grassDir <- '/usr/local/grass' # Linux
#' ```
#' 
#' ## Starting a **fasterRaster** session
#'
#' First, we attach **fasterRaster** in **R**. In most cases, we will also want to be using the **terra** and/or **sf** package, so let's attach them, too.
#' ```
#' library(fasterRaster)
#' library(sf)
#' library(terra)
#' ```
#' Now, we need to start a **fasterRaster** "session", which makes a connection to **GRASS**. You generally only need to do this one time each time you start **R** and wish to use **fasterRaster**. We do this with the [fastStart()] function, which needs the `grassDir` argument and a `crs` argument. This latter sets the coordinate reference system (CRS) for the session--all rasters and vectors used in the session must have the same CRS, or be projected so they have this CRS.  You can start other sessions using `fasteStart()` with different CRSs, and switch between them using [fastRestore()].
#'
#' The `crs` argument takes a coordinate reference string or an object from which such a string can be obtained. We will use a raster that comes with **fasterRaster** to set the `crs` argument.
#' ```
#' madElev <- fastData('madElev') # get raster
#' fastStart(madElev, grassDir = grassDir)
#' ```
#' Here, the `grassDir` is the installation directory we defined earlier. Now, we can start using **fasterRaster**!
#'
#' # Importing rasters and vectors using `fast()`
#'
#' In **fasterRaster**, rasters are called `GRaster`s and vectors are called `GVector`s. The easiest way to start using a `GRaster` or `GVector` is to convert it from one already in **R**. In the example below, we use a raster that comes with the **fasterRaster** package. This raster represents elevation of an eastern portion of Madagascar. First, we use [fastData()] to get the raster. This is a `SPatRaster`, which is a type of raster representation used in the **terra** package.  Then, we use [fast()] to convert it to a `GRaster`. **fasterRaster** can also handle `stars` rasters (**stars** packages).
#' ```
#' elev <- fast(madElev)
#' elev
#' ```
#' You can see the properties of the `GRaster` representation of the `madElev` raster. Converting rasters that are already in **R** to `GRaster`s takes some time. Instead, you can load a raster directly from disk to a **fasterRaster** session using `fast()`. To do this, you just replace the argument in [fast()] with a string representing the folder path and file name of the raster you want to load into the session.
#' 
#' Now, let's create a `GVector`. The [fast()] function can take a `SpatVector` from the **terra** package, an `sf` object from the **sf** package, or a string representing the file path and file name of a vector file (e.g., a GeoPackage file or a shapefile).
#' ```
#' madRivers <- fastData('madRivers')
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
#' You can also use the many **fasterRaster** functions. In general, these functions have the same names as their **terra** and *8sf(( counterparts and usually the same arguments. The following code creates a raster where cell values reflect the distance between them and the nearest river, then converts the output to a `SpatRaster` for plotting:
#' ```
#' dist <- distance(elev, rivers)
#' distRast <- rast(dist)
#' plot(distRast)
#' ```
#' And that's how you get started! Now that you have a raster and a vector in your **fasterRaster** session, you can start doing manipulations and analyses using any of the **fasterRaster** functions!  To see an annotated list of these functions, use `?fasterRaster`.
#' 
#' ## Exporting `GRaster`s and `GVector`s from a **GRASS** session
#'
#' You can convert a `GRaster` to a `SpatRaster` or `stars` raster using [rast()] or [stars()]:
#' ```
#' terraRast <- rast(elev)
#' starsRast <- stars(elev)
#' ```
#' Note that the data type of `GRaster`s converted to other types may not be best for retaining the values saved within them (i.e., if you use [terra::writeRaster()], values might bet rounded). See [writeRaster4()] for more details.
#' 
#' To convert a `GVector` to the **terra** package's `SpatVector` format or to an `sf` vector, use [vect()] or [st_as_sf():
#' ```
#' terraVect <- vect(rivers)
#' sfVect <- st_as_sf(rivers)
#' ```
#' Finally, you can use [writeRaster()] and [writeVector()] to save **fasterRaster** rasters and vectors directly to disk. This will be faster than converting to another format then saving.
#' 
#' @keywords tutorial
NULL

#' @name tutorial_getting_started
#'
#' @title Getting started with "fasterRaster"
#'
#' @description **fasterRaster** interfaces with **GRASS GIS** to process rasters and spatial vector data. It is intended as an add-on to the **terra** and **sf** packages, and relies heavily upon them. For most rasters and vectors that are small or medium-sized in memory/disk, those packages will almost always be faster. They may also be faster for very large objects.  But when they aren't, **fasterRaster** can step in.
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
#' **fasterRaster** uses **GRASS** to do its operations. You will need to install **GRASS** using the "stand-alone" installer, available through the [GRASS GIS](https://grass.osgeo.org/). *Be sure to use the "stand-alone" installer, not the "OSGeo4W" installer!*
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
#' To begin, you need to tell **fasterRaster** the full file path of the folder where **GRASS** is installed on your system. Where this is well depend on your operating system and the version of **GRASS** installed.  Three examples below show you what this might look like, but you may need to change the file path to match your case:
#'
#' ```
#' grassDir <- "C:/Program Files/GRASS GIS 8.3" # Windows
#' grassDir <- "/Applications/GRASS-8.3.app/Contents/Resources" # Mac OS
#' grassDir <- "/usr/local/grass" # Linux
#' ```
#' 
#' To tell **fasterRaster** where **GRASS** is installed, use the [faster()] function:
#'
#' ```
#' faster(grassDir = grassDir)
#' ```
#' You can also use the [faster()] function to set options that affect how **fasterRaster** functions run. This includes setting the amount of maximum memory and number of computer cores allocated to operations.
#'
#' In **fasterRaster**, rasters are called `GRaster`s and vectors are called `GVector`s. The easiest (but not always fastest) way to start using a `GRaster` or `GVector` is to convert it from one already in **R**. In the example below, we use a raster that comes with the **fasterRaster** package. The raster represents elevation of a portion of eastern Madagascar. We first load the `SpatRaster` using [fastData()], a helper function for loading example data objects that come with the **fasterRaster** package.
#' ```
#' madElev <- fastData("madElev") # example SpatRaster
#' madElev
#' ```
#'
#' Now, we do the conversion to a `GRaster` and a `GVector` using [fast()]. This function can create a `GRaster` or `GVector` from a `SpatRaster`, a `SpatVector`, a `sf` vector, or a file representing a raster or vector.
#' ```
#' elev <- fast(madElev)
#' elev
#' ```
#' Converting rasters and vectors that are already in **R** to `GRaster`s and `GVector`s takes some time. Instead, you can load a raster or vector directly from disk to a **fasterRaster** session using `fast()`. To do this, you just replace the first argument in [fast()] with a string representing the folder path and file name of the raster you want to load into the session. For example, you can do:
#'
#' ```
#' rastFile <- system.file("extdata", "madElev.tif"), package = "fasterRaster")
#' elev2 <- fast(rastFile)
#' ```
#' 
#' Now, let's create a `GVector`. The [fast()] function can take a `SpatVector` from the **terra** package, an `sf` object from the **sf** package, or a string representing the file path and file name of a vector file (e.g., a GeoPackage file or a shapefile).
#' ```
#' madRivers <- fastData("madRivers") # sf vector
#' madRivers
#' 
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
#' log10Elev <- log10(elev)
#' log10Elev
#' ```
#'
#' You can also use the many **fasterRaster** functions. In general, these functions have the same names as their **terra** counterparts and often the same arguments. Note that even many **terra** and **fasterRaster** functions have the same name, they do not necessarily produce the exact same output. Much care has been taken to ensure they do, but sometimes there are multiple ways to do the same task, so choices made by the authors of **terra** and **GRASS** can lead to differences.
#'
#' The following code creates a a) raster where cell values reflect the distance between them and the nearest river; b) creates a buffer around the rivers; then c) plots the output:
#' ```
#' dist <- distance(elev, rivers)
#' dist
#'
#' riverBuff <- buffer(rivers, 10000)
#' riversBuff
#'
#' plot(dist)
#' plot(rivers, col = 'blue', add = TRUE)
#' plot(riversBuff, add = TRUE)
#' ```
#'
#' And that's how you get started! Now that you have a raster and a vector in your **fasterRaster** "location", you can start doing manipulations and analyses using any of the **fasterRaster** functions!  To see an annotated list of these functions, use `?fasterRaster`.
#'
#' ## Converting and saving `GRaster`s and `GVector`s
#'
#' You can convert a `GRaster` to a `SpatRaster` raster using [rast()]:
#' ```
#' terraElev <- rast(elev)
#' ```
#' To convert a `GVector` to the **terra** package's `SpatVector` format or to an `sf` vector, use [vect()] or [st_as_sf()]:
#' ```
#' terraRivers <- vect(rivers)
#' sfRivers <- st_as_sf(rivers)
#' ```
#'
#' Finally, you can use [writeRaster()] and [writeVector()] to save `GRaster`s and `GVector`s directly to disk. This will always be faster than using [rast()], [vect()], or [st_as_sf()] then saving the result from those functions.
#' ```
#' elevTempFile <- tempfile(fileext = ".tif") # save as GeoTIFF
#' writeRaster(elev, elevTempFile)
#'
#' vectTempFile <- tempfile(fileext = ".shp") # save as shapefile
#' writeVector(rivers, vectTempFile)
#' ```
#'
#' ## Tips for masking **fasterRaster** faster
#'
#' 1. Loading rasters and vectors directly from disk using [fast()], rather than converting **terra** or **sf** objects is faster. Why? Because if the object does not have a file to which the **R** object points, `fast()` has to save it to disk first as a GeoTIFF or GeoPackage file, then load it into **GRASS**.
#'
#' 2. Similarly, saving `GRaster`s and `GVector`s directly to disk will always be faster than converting them to `SpatRaster`s or `SpatVector`s using [rast()] or [vect()], then saving them. Why? Because `rast()` and `vect() actually save the object to a temporary file then uses the respective function from the respective package to create the `SpatRaster`/`SpatVector`/`sf` vector, which you would then proceed to save to disk again.
#'
#' 3. Every time you switch between using a `GRaster` or `GVector` with a different coordinate reference system (CRS), **GRASS** has to spend a few second changing to that CRS. So, you can save some time by doing as much work as possible with objects in one CRS, then switching to work on objects in another CRS.
#'
#' 4. By default, **GRASS**/**fasterRaster** use 2 cores and 2048 MB (2 GB) of memory for functions that allow users to specify these values. You can set these to higher values using [faster()] and thus potentially speed up some calculations. Functions in newer versions of **GRASS** have more capacity to use these options, so updating **GRASS** to the latest version can help, too.
#'
#' 6. To obviate problems with disk space filling up, by default most **fasterRaster** functions delete intermediate files. However, if you are not creating a lot of very big `GRaster`s or `GVector`s, you can skip this time-taking step by setting the `clean` option to `FALSE` using `faster(clean = FALSE)`. You can also use the [mow()] function to remove from the disk cache any **GRASS** files that are not associated with a `GRaster` or `GVector` in memory. This can be helpful, say, if you create a series of objects, then re-assign them using, say `old_name <- new_object` or remove them using `rm(old_name)`. You removed them from **R**, but the files they pointed to are still in the **GRASS** cache.
#'
#' 5. Its name notwithstanding, **fasterRaster** is just not going to be as fast as **terra** or **sf** for all operations, even when the objects are big in memory or on disk. If you are struggling to analyze an object, you can try respective functions in the other packages.
#'
#' ## Further reading
#' 
#' * Robert Hijman's [**terra**](https://cran.r-project.org/package=terra) package and Edzer Pebesma's [**sf**](https://cran.r-project.org/package=sf) package are good places to start if you are not familiar with doing GIS in **R**.
#' * The [GRASS GIS](https://grass.osgeo.org/) website is authoritative and contains the manual on all the **GRASS** functions used in this package and more.
#' * The Wiki on [how to run **GRASS** in **R** or **R** in **GRASS**](https://grasswiki.osgeo.org/wiki/R_statistics/rgrass) is a good place to start if you want to become a power-user of **GRASS** in **R**.
#' * Roger Bivand's [**rgrass**](https://cran.r-project.org/package=rgrass) package allows users to call any **GRASS** function with all of its functionality, which in some cases is far beyond what is allowed by **fasterRaster**.
#' 
#' @keywords tutorial
NULL

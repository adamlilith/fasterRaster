#' @title 'fasterRaster': Faster raster and spatial vector processing using 'GRASS GIS'
#'
#' @description Processing of large-in-memory/-on disk rasters and spatial vectors using **GRASS GIS**. Most functions in the **terra** and **sf** packages are recreated, and can stand in for them when rasters or vectors are extremely large in memory and/or on disk. Processing of medium-sized and smaller spatial objects will nearly always be faster using **terra** or **sf**. To use most of the functions you must have the stand-alone version of **GRASS** 8.0 or higher (not the **OSGeoW4*installer version).
#'
#' ## Most useful functions/links:
#' A [quick-start tutorial][tutorial]\cr
#' [fastStart()]: Initiate a **GRASS*session\cr
#' [fast()]: Create `GRaster`s or `GVector`s from `SpatRaster`s, `SpatVector`s, or `sf` or `stars` objects, or from files\cr
#' [rast()], [vect()], [st_as_sf()], and [stars()]: Convert `GRaster`s and `GVector`s to `SpatRaster`s, `SpatVector`s, `sf` vectors, or `stars` rasters\cr
#' [writeRaster()] and [writeVector()]: Save `GRaster`s or `GVector`s to disk\cr
#' [setFastOptions()] and [getFastOptions()]: Set options for working with **fasterRaster**\cr
#'
#' ## Properties of **GRASS*rasters
#' [crs()]: Coordinate reference system\cr
#' [datatype()]: Data type\cr
#' [dim()]: Number of rows and columns\cr
#' [ext()]: Spatial extent\cr
#' [location()]: GRASS "location" of an object or the active session\cr
#' [mapset()]: GRASS "mapset" of an object or the active session\cr
#' [minmax()]: Minimum and maximum values across all non-`NA` cells\cr
#' [names()]: `GRaster` names\cr
#' [ncol()]: Number of columns\cr
#' [ncell()]: Number of cells\cr
#' [ncell3d()]: Number of cells of a 3D raster\cr
#' [ndepth()]: Number of depths of a 3D raster\cr
#' [nlyr()]: Number of layers\cr
#' [nrow()]: Number of rows\cr
#' [ncats()]: Number of categories\cr
#' [res()]: Spatial resolution\cr
#' [st_crs()]: Coordinate reference system\cr
#' [topology()]: Dimensionality of a raster (2D or 3D)\cr
#' [zExt()]: Vertical extent
#' 
#' ## Functions that operate on `GRasters`
#' [arithmetic]: Mathematical operations on `GRaster`s (`+`, `-`, `*`, `/`, `^`, `%%` (modulus), `%/%` (integer division))\cr
#' [as.contour()]: Contour lines from a `GRaster`\cr
#' [c()]: "Stack" two or more `GRaster`s\cr
#'
#' ## Properties of `GVectors'
#' [crs()]: Coordinate reference system\cr
#' [ext()]: Spatial extent\cr
#' [geometry()]: Type of vector (points, lines, polygons)\cr
#' [location()]: GRASS "location" of an object or the active session\cr
#' [mapset()]: GRASS "mapset" of an object or the active session\cr
#' [st_crs()]: Coordinate reference system\cr
#' [topology()]: Dimensions of vector coordinates (2D or 3D)\cr
#' [zExt()]: Vertical extent\cr
#' 
#' ## Functions that operate on vectors
#' Something here
#'
#'
#' ## Converting between data types
#' [as.points()], [as.lines()], and [as.polygons()]: Convert a `GRaster` to a `GVector`\cr
#' [rasterize()]: Convert a `GVector` to a `GRaster`\cr
#' [fast()]: Create `GRaster`s or `GVector`s from `SpatRaster`s, `SpatVector`s, or `sf` or `stars` objects, or from files\cr
#' [rast()]: Convert a `GRaster` to a `SpatRaster`\cr
#' [vect()]: Convert a `GVector` to a `SpatVector`\cr
#' [st_as_sf()]: Convert a `GVector` to a `sf` vectors\cr
#' [stars()]: Convert a `GVector` to a `stars` raster\cr
#' 
#' ## General purpose functions
#' [appendLists()]: Append values to elements of a list from another list\cr
#' [comparable()]: Determine if geographic metadata is same between `GRaster`s and/or `GVector`s\cr
#' [compareFloat()]: Compare values accounting for differences due to floating point precision\cr
#' [forwardSlash()]: Replace backslash with forward slash\cr
#' [removeSession()]: Delete a GRASS session (location, mapset(s), and all associated files)\cr
#'
#' ## Functions that operate on **GRASS*"sessions" (seldom used by most users):
#' [fastRestore()]: Restore a previous **GRASS** session or switch **GRASS** locations/mapsets\cr
#' [location()]: **GRASS** "location" of an object or the active session\cr
#' [mapset()]: **GRASS** "mapset" of an object or the active session\cr
#'
#' ## Functions that operate on **GRASS*"regions" (seldom used by most users):
#' [regionDim()]: Change or reprot the active region's resolution\cr
#' [regionExt()]: Change or report the active region's extent\cr
#' [regionRes()]: Change or reprot the active region's dimensions\cr
#' [regionReshape()]: Change or report the active region's extent and resolution\cr
#'
#' ## Esoteric tutorials
#' [working directories and **GRASS*locations and mapsets][location]\cr
#
#' @docType package
#' @author Adam B. Smith
#' @name fasterRaster
NULL

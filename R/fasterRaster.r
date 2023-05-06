#' 'fasterRaster': Faster raster and spatial vector processing using 'GRASS GIS'
#'
#' @description Processing of large-in-memory/-on disk rasters and spatial vectors using **GRASS GIS**. Most functions in the **terra** and **sf** packages are recreated, and can stand in for them when rasters or vectors are extremely large in memory and/or on disk. Processing of medium-sized and smaller spatial objects will nearly always be faster using **terra** or **sf**. To use most of the functions you must have the stand-alone version of **GRASS** version 8.0 or higher (not the **OSGeoW4** installer version).
#'
#' ## Most useful functions/links:
#' A [quick-start tutorial][tutorial_getting_started]\cr
#' [fastStart()]: Initiate a **GRASS** session\cr
#' [fast()]: Create `GRaster`s or `GVector`s from `SpatRaster`s, `SpatVector`s, or `sf` or `stars` objects, or from files\cr
#' [rast()], [vect()], [st_as_sf()], and [stars()]: Convert `GRaster`s and `GVector`s to `SpatRaster`s, `SpatVector`s, `sf` vectors, or `stars` rasters\cr
#' [writeRaster()] and [writeVector()]: Save `GRaster`s or `GVector`s to disk\cr
#' [fastRestore()]: Re-starts a **GRASS** session saved to disk\cr
#' [setFastOptions()] and [getFastOptions()]: Set options for working with **fasterRaster**\cr
#'
#' ## Properties of **fasterRaster** rasters (`GRasters`)
#' [crs()]: Coordinate reference system\cr
#' [datatype()]: Data type\cr
#' [dim()]: Number of rows and columns\cr
#' [ext()]: Spatial extent\cr
#' [gnames()]: Name of the object in **GRASS**\cr
#' [location()]: **GRASS** "location" of an object or the active session\cr
#' [mapset()]: **GRASS** "mapset" of an object or the active session\cr
#' [minmax()]: Minimum and maximum values across all non-`NA` cells\cr
#' [names()]: `GRaster` names\cr
#' [ncol()]: Number of columns\cr
#' [ncell()]: Number of cells\cr
#' [ncell3d()]: Number of cells of a 3D raster\cr
#' [ndepth()]: Number of depths of a 3D raster\cr
#' [nlyr()]: Number of layers\cr
#' [nrow()]: Number of rows\cr
#' [ncat()]: Number of categories\cr
#' [res()]: Spatial resolution\cr
#' [st_bbox()]: Spatial extent\cr
#' [st_crs()]: Coordinate reference system\cr
#' [topology()]: Dimensionality of a raster (2D or 3D)\cr
#' [zext()]: Vertical extent
#'
#' ## Functions that operate on or create `GRasters`
#' [Arithmetic]: Mathematical operations on `GRaster`s: `+`, `-`, `*`, `/`, `^`, `%%` (modulus), `%/%` (integer division)\cr
#' Math: Mathematical functions applied to `GRaster`s: [abs()], [sin()], [cos()], [tan()], [asin()], [acos()], [atan()], [atan2()], [exp()], [log()], [log1p()], [log2()], [log10()], [sqrt()], [round()], [floor()], [ceiling()], [trunc()]\cr
#' [as.contour()]: Contour lines from a `GRaster`\cr
#' [buffer()]: Create a buffer around non-`NA` cells\cr
#' [c()]: "Stack" two or more `GRaster`s\cr
#' [distance()]: Distance to non-`NA` cells, or vice versa\cr
#' [makeGRaster()]: Makes a `GRaster` from a raster already in a **GRASS** session\cr
#' `[[[]`[subset]: Subset a `GRaster` with multiple layers\cr
#'
#' ## Properties of **fasterRaster** vectors (`GVectors')
#' [crs()]: Coordinate reference system\cr
#' [datatype()]: Data type of fields\cr
#' [ext()]: Spatial extent\cr
#' [geomtype()]: Type of vector (points, lines, polygons)\cr
#' [gnames()]: Name of the object in **GRASS**\cr
#' [is.points()], [is.lines()], [is.polygons()]: Does a `GVector` represent points, lines, or polygons?\cr
#' [location()]: **GRASS** "location" of an object or the active session\cr
#' [mapset()]: **GRASS** "mapset" of an object or the active session\cr
#' [names()]: Names of `GVector` fields\cr
#' [ncol()]: Number of fields\cr
#' [nrow()]: Names of geometries\cr
#' [st_bbox()]: Spatial extent\cr
#' [st_crs()]: Coordinate reference system\cr
#' [topology()]: Dimensions of vector coordinates (2D or 3D)\cr
#' [zext()]: Vertical extent\cr
#'
#' ## Functions that operate on or create `GVectors`
#' [buffer()]: Create a polygon around/inside a vector\cr
#' [connectors()]: Create lines connecting nearest features of two vectors\cr
#' [distance()]: Distance between geometries in two vectors, or from a vector to cells of a raster\cr
#' [makeGVector()]: Makes a `GVector` from a vector already in a **GRASS** session.
#' [st_buffer()]: Create a polygon around/inside a vector\cr
#' [st_distance()]: Distance between geometries in two vectors\cr
#'
#' ## Converting between data types
#' [as.contour()]: Contour lines from a `GRaster`\cr
#' [as.points()], [as.lines()], and [as.polygons()]: Convert a `GRaster` to a `GVector`\cr
#' [fast()]: Create `GRaster`s or `GVector`s from `SpatRaster`s, `SpatVector`s, or `sf` or `stars` objects, or from files\cr
#' [rast()]: Convert a `GRaster` to a `SpatRaster`\cr
#' [rasterize()]: Convert a `GVector` to a `GRaster`\cr
#' [st_as_sf()]: Convert a `GVector` to a `sf` vectors\cr
#' [stars()]: Convert a `GVector` to a `stars` raster\cr
#' [vect()]: Convert a `GVector` to a `SpatVector`\cr
#' [vectToGrass()]: Convert a `SpatVector` to a **GRASS** vector\cr
#'
#' ## General purpose functions
#' [appendLists()]: Append values to elements of a list from another list\cr
#' [comparable()]: Determine if geographic metadata is same between two `GRaster`s and/or `GVector`s\cr
#' [compareFloat()]: Compare values accounting for differences due to floating point precision\cr
#' [fastClass()]: Class of a **fasterRaster** object\cr
#' [forwardSlash()]: Replace backslash with forward slash\cr
#' [grassInfo()]: **GRASS** version and citation\cr
#' [vectToGrass()]: Send a `SpatVector` or `sf` vector to an open **GRASS** connection\cr
#'
#' ## Functions that operate on **GRASS** "sessions" (seldom used by most users):
#' [fastRemove()]: Delete a **GRASS** session (location, mapset(s), and all associated files)\cr
#' [fastRestore()]: Restore a previous **GRASS** session or switch **GRASS** locations/mapsets\cr
#' [location()]: **GRASS** "location" of an object or the active session\cr
#' [mapset()]: **GRASS** "mapset" of an object or the active session\cr
#'
#' ## Functions that operate on **GRASS** "regions" (seldom used by most users):
#' [regionDim()]: Change or report the active region's resolution\cr
#' [regionExt()]: Change or report the active region's extent\cr
#' [regionRes()]: Change or report the active region's dimensions\cr
#' [regionShape()]: Change or report the active region's extent and resolution\cr
#'
#' ## Esoteric tutorials
#' **fasterRaster** [Working directories, locations, and mapsets][tutorial_sessions]\cr
#' **GRASS** [regions][tutorial_regions]\cr
#
#' @docType package
#' @author Adam B. Smith
#' @name fasterRaster
NULL

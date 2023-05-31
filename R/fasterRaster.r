#' 'fasterRaster': Faster raster and spatial vector processing using 'GRASS GIS'
#'
#' @description Processing of large-in-memory/-on disk rasters and spatial vectors using **GRASS GIS**. Most functions in the **terra** and **sf** packages are recreated, and can stand in for them when rasters or vectors are extremely large in memory and/or on disk. Processing of medium-sized and smaller spatial objects will nearly always be faster using **terra** or **sf**. To use most of the functions you must have the stand-alone version of **GRASS** version 8.0 or higher (not the **OSGeoW4** installer version).
#'
#' ## Most useful functions/links:
#' A [quick-start tutorial][tutorial_getting_started]\cr
#' [faster()]: Initiate a **GRASS** session\cr
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
#' [freq()]: Frequencies of cell values in a raster\cr
#' [global()]: Summary statistics\cr
#' [gnames()]: Name of the object in **GRASS**\cr
#' [is.2d()] and [is.3d()]: Is an object 2- or 3-dimensional?\cr
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
#' Single-layer functions: Applied to each layer of a `GRaster`:
#' - Trigonometry: [sin()], [cos()], [tan()], [asin()], [acos()], [atan()], [atan2()] \cr
#' - Logarithms and powers: [exp()], [log()], [log1p()], [log2()], [log10()], [sqrt()] \cr
#' - Rounding: [round()], [floor()], [ceiling()], [trunc()] \cr
#' - Signs: [abs()] \cr
#' Multi-layer functions: Applied across multiple `GRaster`s:
#' - Numeration: [sum()], [count()] \cr
#' - Central tendency: [mean()], [mmode()], [median()] \cr
#' - Dispersion: [sd()], [var()], [sdpop()], [varpop()], [nunique()], [range()], [quantile()], [skewness()], [kurtosis()]
#' - Extremes: [min()], [max()], [which.min()], [which.max()] \cr
#' [as.contour()]: Contour lines from a raster\cr
#' [aggregate()]: Aggregate values of raster cells into larger cells\cr
#' [buffer()]: Create a buffer around non-`NA` cells\cr
#' [c()]: "Stack" two or more rasters\cr
#' [crop()]: Remove parts of a raster\cr
#' [distance()]: Distance to non-`NA` cells, or vice versa\cr
#' [global()]: Summary statistics\cr
#' [horizonHeight()]: Horizon height\cr
#' [makeGRaster()]: Makes a `GRaster` from a raster already in a **GRASS** session\cr
#' [merge()]: Combine two or more rasters with different extents and fill in `NA`s\cr
#' [project()]: Change coordinate reference system and cell size\cr
#' [resample()]: Change cell size\cr
#' [sun()]: Solar radiance and irradiance\cr
#' [terrain()]: Slope, aspect, curvature, and partial slopes\cr
#' `[[[]`[subset]: Subset a raster with multiple layers\cr
#'
#' ## Properties of **fasterRaster** vectors (`GVectors')
#' [crs()]: Coordinate reference system\cr
#' [datatype()]: Data type of fields\cr
#' [ext()]: Spatial extent\cr
#' [geomtype()]: Type of vector (points, lines, polygons)\cr
#' [gnames()]: Name of the object in **GRASS**\cr
#' [is.2d()] and [is.3d()]: Is an object 2- or 3-dimensional?\cr
#' [is.points()], [is.lines()], [is.polygons()]: Does a `GVector` represent points, lines, or polygons?\cr
#' [location()]: **GRASS** "location" of an object or the active session\cr
#' [mapset()]: **GRASS** "mapset" of an object or the active session\cr
#' [names()]: Names of `GVector` fields\cr
#' [ncol()]: Number of fields\cr
#' [nrow()]: Number of geometries\cr
#' [st_bbox()]: Spatial extent\cr
#' [st_crs()]: Coordinate reference system\cr
#' [topology()]: Dimensions of vector coordinates (2D or 3D)\cr
#' [zext()]: Vertical extent\cr
#'
#' ## Functions that operate on or create `GVectors`
#' [as.data.frame()]: Convert a vector to a `data.frame`\cr
#' [buffer()]: Create a polygon around/inside a vector\cr
#' [connectors()]: Create lines connecting nearest features of two vectors\cr
#' [convHull()]: Minimum convex hull\cr
#' [crds()]: Coordinates of a vector's features\cr
#' [crop()]: Remove parts of a vector\cr
#' [distance()]: Distance between geometries in two vectors, or from a vector to cells of a raster\cr
#' [head()]: First rows of a vector's data frame.\cr
#' [makeGVector()]: Makes a `GVector` from a vector already in a **GRASS** session.\cr
#' [project()]: Change coordinate reference system\cr
#' [st_buffer()]: Create a polygon around/inside a vector\cr
#' [st_distance()]: Distance between geometries in two vectors\cr
#' [tail()]: Last rows of a vector's data frame.\cr
#'
#' ## Converting between data types
#' [as.contour()]: Contour lines from a `GRaster`\cr
#' [as.data.frame()]: Convert `GVector` to a `data.frame`\cr
#' [as.points()], [as.lines()], and [as.polygons()]: Convert a `GRaster` to a `GVector`\cr
#' [fast()]: Create `GRaster`s or `GVector`s from `SpatRaster`s, `SpatVector`s, or `sf` or `stars` objects, or from files\cr
#' [head()]: First rows of a `GVector`'s data frame.\cr
#' [rastToGrass()]: Convert a `SpatRaster` to a **GRASS** raster\cr
#' [rast()]: Convert a `GRaster` to a `SpatRaster`\cr
#' [rasterize()]: Convert a `GVector` to a `GRaster`\cr
#' [st_as_sf()]: Convert a `GVector` to a `sf` vectors\cr
#' [stars()]: Convert a `GVector` to a `stars` raster\cr
#' [tail()]: Last rows of a `GVector`'s data frame.\cr
#' [vect()]: Convert a `GVector` to a `SpatVector`\cr
#' [vectToGrass()]: Convert a `SpatVector` to a **GRASS** vector\cr
#'
#' ## General purpose functions
#' [appendLists()]: Append values to elements of a list from another list\cr
#' [comparable()]: Determine if geographic metadata is same between two `GRaster`s and/or `GVector`s\cr
#' [copyGSpatial()]: Copy a raster or vector already in **GRASS**\cr
#' [compareFloat()]: Compare values accounting for differences due to floating point precision\cr
#' [forwardSlash()]: Replace backslash with forward slash\cr
#' [grassInfo()]: **GRASS** version and citation\cr
#' [rastToGrass()]: Convert a `SpatRaster` to a **GRASS** raster\cr
#' [rstring()]: Create a statistically unique string\cr
#' [vectToGrass()]: Convert a `SpatVector` or `sf` vector to a `GVector`\cr
#'
#' ## Functions that operate on **GRASS** "sessions" (seldom used by most users):
#' [fastRemove()]: Delete a **GRASS** session (location, mapset(s), and all associated files)\cr
#' [fastRestore()]: Restore a previous **GRASS** session or switch **GRASS** locations/mapsets\cr
#' [location()]: **GRASS** "location" of an object or the active session\cr
#' [mapset()]: **GRASS** "mapset" of an object or the active session\cr
#'
#' ## Functions that operate on **GRASS** "regions" (seldom used by most users):
#' [region()]: Change or report the active region's extent and resolution\cr
#' [regionDim()]: Change or report the active region's resolution\cr
#' [regionExt()]: Change or report the active region's extent\cr
#' [regionRes()]: Change or report the active region's dimensions\cr
#' [regionDim3d()]: Change or report the active region's 3D dimensions\cr
#' [regionRes3d()]: Change or report the active region's 3D resolution\cr
#' [regionZExt()]: Change or report the active region's vertical extent\cr
#'
#' ## Esoteric tutorials
#' **fasterRaster** [Working directories, locations, and mapsets][tutorial_sessions]\cr
#' **GRASS** [regions][tutorial_regions]\cr
#
#' @docType package
#' @author Adam B. Smith
#' @name fasterRaster
NULL

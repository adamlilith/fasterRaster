#' "fasterRaster": Faster raster and spatial vector processing using "GRASS GIS"
#'
#' \figure{fasterRaster.png}{options: alt="fasterRaster" align="right" vertical-align="top" width=1.2in}
#'
#' @description Processing of large-in-memory/-on disk rasters and spatial vectors using **GRASS GIS**. Most functions in the **terra** and **sf** packages are recreated. Processing of medium-sized and smaller spatial objects will nearly always be faster using **terra** or **sf**. To use most of the functions you must have the stand-alone version of **GRASS** version 8.0 or higher (not the **OSGeoW4** installer version). Note that due to developer choices, results will not always be strictly comparable between **terra**, **sf**, and **fasterRaster**.
#'
#' ## Most useful tutorials and functions:
#' A [quick-start tutorial][tutorial_getting_started]\cr
#' [faster()]: Initiate a **GRASS** session\cr
#' [fast()]: Convert a `SpatRaster`, `SpatVector`, or `sf` vector to **fasterRaster**'s raster format (`GRaster`s) or vector format (`GVector`s), or load one from a file\cr
#' [rast()], [vect()], and [st_as_sf()]: Convert `GRaster`s and `GVector`s to `SpatRaster`s, `SpatVector`s, or `sf` vectors\cr
#' [writeRaster()] and [writeVector()]: Save `GRaster`s or `GVector`s to disk\cr
#' [fastRestore()]: Revert to another **GRASS** ["location" or "mapset"][tutorial_sessions], or restart a **GRASS** session saved to disk\cr
#' [setFastOptions()] and [getFastOptions()]: Set options for working with **fasterRaster**\cr
#'
#' ## Properties of **fasterRaster** rasters (`GRasters`)
#' [crs()]: Coordinate reference system\cr
#' [datatype()]: Data type\cr
#' [dim()]: Number of rows and columns\cr
#' [ext()], [north()], [south()], [east()], [west()], [top()], and [bottom()]: Spatial extent\cr
#' [freq()]: Frequencies of cell values in a raster\cr
#' [global()]: Summary statistics\cr
#' [is.2d()] and [is.3d()]: Is an object 2- or 3-dimensional?\cr
#' [is.cell()], [is.fcell()], [is.dcell()]: Raster data type (integer/float/double)\cr
#' [is.lonlat]: Is an object projected (e.g., in WGS84)?\cr
#' [location()]: **GRASS** "location" of an object or the active session\cr
#' [mapset()]: **GRASS** "mapset" of an object or the active session\cr
#' [minmax()]: Minimum and maximum values across all non-`NA` cells\cr
#' [names()]: `GRaster` names\cr
#' [ncol()]: Number of columns\cr
#' [nacell()]: Number of `NA` cells\cr
#' [ncell()]: Number of cells\cr
#' [ncell3d()]: Number of cells of a 3D raster\cr
#' [ndepth()]: Number of depths of a 3D raster\cr
#' [nlyr()]: Number of layers\cr
#' [nonnacell()]: Number of non-`NA` cells\cr
#' [nrow()]: Number of rows\cr
#' [ncat()]: Number of categories\cr
#' [res()], [xres()], [yres()], and [zres()]: Spatial resolution\cr
#' [st_bbox()]: Spatial extent\cr
#' [st_crs()]: Coordinate reference system\cr
#' [topology()]: Dimensionality (2D or 3D)\cr
#' [zext()]: Vertical extent\cr
#' [zres()]: Vertical resolution\cr
#' 
#' ## Functions that operate on or create `GRasters`
#' [Arithmetic]: Mathematical operations on `GRaster`s: `+`, `-`, `*`, `/`, `^`, `%%` (modulus), `%/%` (integer division)\cr
#'
#' Single-layer functions (applied to each layer of a `GRaster`):
#' - Trigonometry: [sin()], [cos()], [tan()], [asin()], [acos()], [atan()], [atan2()] \cr
#' - Logarithms and powers: [exp()], [log()], [log1p()], [log2()], [log10()], [sqrt()] \cr
#' - Rounding: [round()], [floor()], [ceiling()], [trunc()] \cr
#' - Signs: [abs()] \cr
#' 
#' Multi-layer functions (applied across layers of a "stack" of `GRaster`s):
#' - Numeration: [sum()], [count()] \cr
#' - Central tendency: [mean()], [mmode()], [median()] \cr
#' - Dispersion: [sd()], [var()], [sdpop()], [varpop()], [nunique()], [range()], [quantile()], [skewness()], [kurtosis()]
#' - Extremes: [min()], [max()], [which.min()], [which.max()] \cr
#' 
#' Other functions:\cr
#' `[<-` ([assign][subset_assign]): Assign values to a raster's cells\cr
#' `[[` ([subset][subset_assign]): Subset a raster with multiple layers\cr
#' `[[<-` ([assign][subset_assign]): Replace or add layers to a raster\cr
#' [as.cell()], [as.fcell()], [as.dcell()]: Change data type (integer/float/double)\cr
#' [as.contour()]: Contour lines from a raster\cr
#' [as.lines()]: Convert a raster to a "lines" vector\cr
#' [as.points()]: Convert a raster to a "points" vector\cr
#' [as.polygons()]: Convert a raster to a "polygons" vector\cr
#' [aggregate()]: Aggregate values of raster cells into larger cells\cr
#' [buffer()]: Create a buffer around non-`NA` cells\cr
#' [app()]: Apply a user-defined function to multiple layers of a raster (with helper functions [appFuns()] and [appCheck()])\cr
#' [c()]: "Stack" two or more rasters\cr
#' [clump()]: Group adjacent cells with similar values\cr
#' [crop()]: Remove parts of a raster\cr
#' [distance()]: Distance to non-`NA` cells, or vice versa\cr
#' [extend()]: Add rows and columns to a raster\cr
#' [extract()]: Extract values from a raster at locations in a points vector\cr
#' [focal()]: Calculate cell values based on values of nearby cells\cr
#' [fractalRast()]: Create a fractal raster\cr
#' [global()]: Summary statistics across cells of each raster layer\cr
#' [`hillshade()`][shade]: Create a hillshade raster\cr
#' [horizonHeight()]: Horizon height\cr
#' [longlat()]: Create longitude/latitude rasters.\cr
#' [mask()]: Remove values in a raster based on values in another raster or vector\cr
#' [merge()]: Combine two or more rasters with different extents and fill in `NA`s\cr
#' [plot()]: Display a raster\cr
#' [project()]: Change coordinate reference system and cell size\cr
#' [resample()]: Change cell size\cr
#' [rnormRast()]: A random raster with or without spatial dependence\cr
#' [spatSample()]: Randomly points from a raster\cr
#' [spDepRast()]: Create a random raster with or without spatial dependence\cr
#' [sun()]: Solar radiance and irradiance\cr
#' [terrain()]: Slope, aspect, curvature, and partial slopes\cr
#' [thin()]: Reduce linear features on a raster so linear features are 1 cell wide\cr
#' [trim()]: Remove rows and columns from a raster that are all `NA`\cr
#' [viewshed()]: Areas visible from points on a raster\cr
#'
#' ## Properties of **fasterRaster** vectors (`GVectors`)
#' [crs()]: Coordinate reference system\cr
#' [datatype()]: Data type of fields\cr
#' [dim()]: Number of geometries and columns\cr
#' [ext()], [north()], [south()], [east()], [west()], [top()], and [bottom()]: Spatial extent\cr
#' [geomtype()]: Type of vector (points, lines, polygons)\cr
#' [is.2d()] and [is.3d()]: Is an object 2- or 3-dimensional?\cr
#' [is.lonlat]: Is an object projected (e.g., in WGS84)?\cr
#' [is.points()], [is.lines()], [is.polygons()]: Does a `GVector` represent points, lines, or polygons?\cr
#' [location()]: **GRASS** "location" of an object or the active session\cr
#' [mapset()]: **GRASS** "mapset" of an object or the active session\cr
#' [names()]: Names of `GVector` fields\cr
#' [ncol()]: Number of fields\cr
#' [nrow()]: Number of geometries\cr
#' [st_bbox()]: Spatial extent\cr
#' [st_crs()]: Coordinate reference system\cr
#' [topology()]: Dimensionality (2D or 3D)\cr
#' [zext()]: Vertical extent\cr
#'
#' ## Functions that operate on or create `GVectors`
#' `[` ([subset][subset_assign]): Select geometries/rows of a vector"s data frame\cr
#' `[[` ([subset][subset_assign]): Subset columns of a vector"s data frame\cr
#' [as.data.frame()]: Convert a vector to a `data.frame`\cr
#' [as.points()]: Extract vertex coordinates from a "lines" or "polygons" `GVector`\cr
#' [buffer()]: Create a polygon around/inside a vector\cr
#' [cleanGeom()]: Fix undesirable geometries of a vector\cr
#' [connectors()]: Create lines connecting nearest features of two vectors\cr
#' [convHull()]: Minimum convex hull\cr
#' [crds()]: Extract coordinates of a vector\cr
#' [crop()]: Remove parts of a vector\cr
#' [delaunay()]: Delaunay triangulation\cr
#' [distance()]: Distance between geometries in two vectors, or from a vector to cells of a raster\cr
#' [head()]: First rows of a vector"s data frame.\cr
#' [project()]: Change coordinate reference system\cr
#' [simplifyGeom()]: Remove vertices\cr
#' [smoothGeom()]: Remove "angular" aspects of features\cr
#' [st_as_sf()]: Convert a `GVector` to a `sf` vector\cr
#' [st_buffer()]: Create a polygon around/inside a vector\cr
#' [st_distance()]: Distance between geometries in two vectors\cr
#' [tail()]: Last rows of a vector"s data frame.\cr
#'
#' ## Converting between data types
#' [as.contour()]: Convert a `GRaster` to a `GVector` representing contour lines\cr
#' [as.data.frame()]: Convert `GVector` to a `data.frame`\cr
#' [as.points()], [as.lines()], and [as.polygons()]: Convert a `GRaster` to a `GVector`\cr
#' [fast()]: Convert a `SpatRaster`, `SpatVector`, or `sf` vector to a `GRaster` or `GVector`, or load one from a file\cr
#' [rast()]: Convert a `GRaster` to a `SpatRaster`\cr
#' [rasterize()]: Convert a `GVector` to a `GRaster`\cr
#' [st_as_sf()]: Convert a `GVector` to a `sf` vector\cr
#' [vect()]: Convert a `GVector` to a `SpatVector`\cr
#'
#' ## General purpose functions
#' [appendLists()]: Append values to elements of a list from another list\cr
#' [compareGeom()]: Determine if geographic metadata is same between `GRaster`s and/or `GVector`s\cr
#' [compareFloat()]: Compare values accounting for differences due to floating point precision\cr
#' [forwardSlash()]: Replace backslash with forward slash\cr
#' [grassInfo()]: **GRASS** version and citation\cr
#' [pmatchSafe()]: Partial matching of strings with error checking\cr
#' [rstring()]: Create a string statistically likely to be unique\cr
#'
#' ## Functions that operate on **GRASS** "sessions":
#' [crs()]: Coordinate reference system of the current location\cr
#' [fastRemove()]: Delete a **GRASS** session (location, mapset(s), and all associated files)\cr
#' [fastRestore()]: Restore a previous **GRASS** session or switch **GRASS** locations/mapsets\cr
#' [location()]: **GRASS** "location" of an object or the active session\cr
#' [mapset()]: **GRASS** "mapset" of an object or the active session\cr
#'
#' ## Functions that operate on **GRASS** "regions" (seldom used by most users):
#' [region()]: Change or report the active region's extent and resolution\cr
#' [regionDim()]: Change or report the active region's resolution (also [dim()] and related functions, with no arguments)
#' [regionExt()]: Change or report the active region's extent (also [ext()] and related functions, with no arguments)\cr
#' [regionRes()]: Change or report the active region's dimensions (also [res()] and related functions, with no arguments)\cr
#'
#' ## Data objects
#' [appFunsTable][appFunsTable] (see also [appFuns()]): Functions usable by the [app()] function\cr
#' [madChelsa][madChelsa]: Climate rasters for of a portion of eastern Madagascar\cr
#' [madCoast0][madCoast0], [madCoast4][madCoast4], and [madCoast][madCoast]: Borders of an eastern portion of Madagascar\cr
#' [madDypsis][madDypsis]: Specimens records of species in the genus *Dypsis*\cr
#' [madElev][madElev]: Elevation raster\cr
#' [madForest2000][madForest2000] and [madForest2014][madForest2014]: Forest cover in 2000 and 2014\cr
#' [madRivers][madRivers]: Rivers vector\cr
#' 
#' ## Esoteric tutorials
#' [Sessions, locations, and mapsets][tutorial_sessions]\cr
#' [Regions][tutorial_regions]\cr
#' [Undocumented functions][tutorial_undocumented_functions]\cr
#
#' @docType package
#' @author Adam B. Smith
#' @name fasterRaster
NULL

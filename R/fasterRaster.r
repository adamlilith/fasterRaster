#' "fasterRaster": Faster raster and spatial vector processing using "GRASS GIS"
#'
#' @description Processing of large-in-memory/-on disk rasters and spatial vectors using **GRASS GIS**. Most functions in the **terra** and **sf** packages are recreated. Processing of medium-sized and smaller spatial objects will nearly always be faster using **terra** or **sf**. To use most of the functions you must have the stand-alone version of **GRASS** version 8.0 or higher (not the **OSGeoW4** installer version). Note that due to developer choices, results will not always be strictly comparable between **terra**, **sf**, and **fasterRaster**.
#'
#' ## Most useful tutorials and functions:
#' A [quick-start][tutorial_getting_started] tutorial\cr
#' A tutorial on [raster data types][tutorial_raster_data_types]\cr
#' [faster()]: Initiate a **GRASS** session\cr
#' [fast()]: Convert a `SpatRaster`, `SpatVector`, or `sf` vector to **fasterRaster**'s raster format (`GRaster`s) or vector format (`GVector`s), or load one from a file\cr
#' [rast()], [vect()], and [st_as_sf()]: Convert `GRaster`s and `GVector`s to `SpatRaster`s, `SpatVector`s, or `sf` vectors\cr
#' [writeRaster()] and [writeVector()]: Save `GRaster`s or `GVector`s to disk\cr
#' [restoreSession()]: Revert to another **GRASS** ["location" or "mapset"][tutorial_sessions], or restart a **GRASS** session saved to disk\cr
#' [setFastOptions()] and [getFastOptions()]: Set options for working with **fasterRaster**\cr
#'
#' ## Properties of `GRasters`
#' [crs()]: Coordinate reference system\cr
#' [datatype()]: Data type\cr
#' [dim()]: Number of rows and columns\cr
#' [ext()], [N()], [S()], [E()], [W()], [top()], and [bottom()]: Spatial extent\cr
#' [freq()]: Frequencies of cell values in a raster\cr
#' [global()]: Summary statistics\cr
#' [is.2d()] and [is.3d()]: Is an object 2- or 3-dimensional?\cr
#' [is.int()], [is.float()], [is.doub()]: `GRaster` data type (integer/float/double)\cr
#' [is.factor()]: Does a raster represent categorical data?\cr
#' [is.lonlat()]: Is an object projected (e.g., in WGS84)?\cr
#' [levels()]: Names of levels in a categorical `GRaster`\cr
#' [location()]: **GRASS** "location" of an object or the active session\cr
#' [mapset()]: **GRASS** "mapset" of an object or the active session\cr
#' [minmax()]: Minimum and maximum values across all non-`NA` cells\cr
#' [names()]: `GRaster` names\cr
#' [ncol()]: Number of columns\cr
#' [nacell()]: Number of `NA` cells\cr
#' [ncell()]: Number of cells\cr
#' [ncell3d()]: Number of cells of a 3D `GRaster`\cr
#' [ndepth()]: Number of depths of a 3D `GRaster`\cr
#' [nlyr()]: Number of layers\cr
#' [nonnacell()]: Number of non-`NA` cells\cr
#' [nrow()]: Number of rows\cr
#' [nlevels()]: Number of categories\cr
#' [res()], [xres()], [yres()], and [zres()]: Spatial resolution\cr
#' [sources()]: Name of the `GRaster` in **GRASS**\cr
#' [st_bbox()]: Spatial extent\cr
#' [st_crs()]: Coordinate reference system\cr
#' [topology()]: Dimensionality (2D or 3D)\cr
#' [zext()]: Vertical extent\cr
#' [zres()]: Vertical resolution\cr
#' 
#' ## Functions that operate on or create `GRasters`
#' [Arithmetic]: Mathematical operations on `GRaster`s: `+`, `-`, `*`, `/`, `^`, `%%` (modulus), `%/%` (integer division)\cr
#' [Equality][Comparison]: `<`, `<=`, `==`, `!=`, `>=`, and `>`, plus `%in%` (for categorical rasters only)\cr
#'
#' Single-layer functions (applied to each layer of a `GRaster`):
#' - `NA`s: [is.na()] and [not.na()] \cr
#' - Trigonometry: [sin()], [cos()], [tan()], [asin()], [acos()], [atan()], [atan2()] \cr
#' - Logarithms and powers: [exp()], [log()], [ln()], [log1p()], [log2()], [log10()], [sqrt()] \cr
#' - Rounding: [round()], [floor()], [ceiling()], [trunc()] \cr
#' - Signs: [abs()] \cr
#'
#' Multi-layer functions (applied across layers of a "stack" of `GRaster`s):
#' - Numeration: [sum()], [count()] \cr
#' - Central tendency: [mean()], [mmode()], [median()] \cr
#' - Dispersion: [sd()], [var()], [sdpop()], [varpop()], [nunique()], [range()], [quantile()], [skewness()], [kurtosis()]
#' - Extremes: [min()], [max()], [which.min()], [which.max()] \cr
#' 
#' ## Functions that operate on or create `GRaster`s
#' The operators `$` ([dollar][subset_dollar]) and `[[` ([double-square brackets][subset_double_square_brackets]) can be used to subset or remove specific layers of a `GRaster`.\cr
#' The `[<-` ([single-bracket assign][replace_single_square_bracket]) operator can be used to replace values of cells of a `GRaster`.\cr
#' The assign operators, `$<-` ([dollar assign][replace_dollar]), `[[<-` ([double-square brackets assign][replace_double_square_brackets]), and `[add<-][add]` can be used to replace specific layers of a `GRaster`.\cr
#' [as.int()], [as.float()], [as.doub()]: Change data type (integer/float/double)\cr
#' [as.contour()]: Contour lines from a `GRaster`\cr
#' [as.lines()]: Convert a `GRaster` to a "lines" vector\cr
#' [as.points()]: Convert a `GRaster` to a "points" vector\cr
#' [as.polygons()]: Convert a `GRaster` to a "polygons" vector\cr
#' [aggregate()]: Aggregate values of `GRaster` cells into larger cells\cr
#' [buffer()]: Create a buffer around non-`NA` cells\cr
#' [app()]: Apply a user-defined function to multiple layers of a `GRaster` (with helper functions [appFuns()] and [appCheck()])\cr
#' [c()]: "Stack" two or more rasters\cr
#' [clump()]: Group adjacent cells with similar values\cr
#' [cor()]: Correlation matrix between layers of a `GRaster`\cr
#' [cov()]: Covariance matrix between layers of a `GRaster`\cr
#' [crop()]: Remove parts of a `GRaster`\cr
#' [denoise()]: Remove "noise" from a `GRaster` using a principal components analysis (PCA)\cr
#' [distance()]: Distance to non-`NA` cells, or vice versa\cr
#' [extend()]: Add rows and columns to a `GRaster`\cr
#' [extract()]: Extract values from a `GRaster` at specific points\cr
#' [focal()]: Calculate cell values based on values of nearby cells\cr
#' [fractalRast()]: Create a fractal `GRaster`\cr
#' [global()]: Summary statistics across cells of each `GRaster` layer\cr
#' [`hillshade()`][shade]: Create a hillshade `GRaster`\cr
#' [hist()]: Histogram of `GRaster` values\cr
#' [horizonHeight()]: Horizon height\cr
#' [longlat()]: Create longitude/latitude rasters.\cr
#' [mask()]: Remove values in a `GRaster` based on values in another `GRaster` or vector\cr
#' [merge()]: Combine two or more rasters with different extents and fill in `NA`s\cr
#' [noise()]: Remove coarse-scale trends from a `GRaster`, leaving just fine-scale "noise"\cr
#' [pairs()]: Plot correlations between `GRaster` layers\cr
#' [pca()]: Apply a principal components analysis (PCA) to a `GRaster`\cr
#' [pcs()]: Retrieve a principal components model from a PCA `GRaster` generated using `pca()`\cr
#' [plot()]: Display a `GRaster`\cr
#' [plotRGB()]: Display a multispectral `GRaster` using red, blue, green, and alpha channels\cr
#' [project()]: Change coordinate reference system and cell size\cr
#' [resample()]: Change cell size\cr
#' [rnormRast()]: A random `GRaster` with values drawn from a normal distribution\cr
#' [runifRast()]: A random `GRaster` with values drawn from a uniform distribution\cr
#' [selectRange()]: Select values from rasters in a stack based on values in another `GRaster`\cr
#' [spatSample()]: Randomly points from a `GRaster`\cr
#' [spDepRast()]: Create a random `GRaster` with or without spatial dependence\cr
#' [sun()]: Solar radiance and irradiance\cr
#' [terrain()]: Slope, aspect, curvature, and partial slopes\cr
#' [thinLines()]: Reduce linear features on a `GRaster` so linear features are 1 cell wide\cr
#' [trim()]: Remove rows and columns from a `GRaster` that are all `NA`\cr
#' [viewshed()]: Areas visible from points on a raster\cr
#'
#' ## Functions operating on categorical rasters
#' [activeCat()]: Column that defines category labels\cr
#' `[activeCat<-]`: Set column that defines category labels\cr
#' [addCats()]: Add new columns to a "levels" table\cr
#' `[addCats<-]`: Add new rows (levels) to a "levels" table\cr
#' [categories()]: Set "levels" table for specific layers of a categorical raster\cr
#' [catNames()]: Names of each "levels" table\cr
#' [cats()]: "Levels" table of a categorical raster\cr
#' [droplevels()]: Remove one or more levels\cr
#' [freq()]: Frequency of each category across cells of a raster\cr
#' [is.factor()]: Is a raster categorical?\cr
#' [levels()]: "Levels" table of a categorical raster\cr
#' [levels<-]: Set "levels" table of a categorical raster\cr
#' [minmax()]: "Lowest" and "highest" category values of categorical rasters (when argument `levels = TRUE`)\cr
#' [missingCats()]: Values that have no category assigned to them\cr
#' [nlevels()]: Number of levels\cr
#' 
#' ## Functions for analysis of remote sensing rasters
#' [toarAster()]: Convert digital number ASTER rasters to radiance, and radiance to reflectance\cr
#' [toarLandsat()]: Convert digital number LANDSAT rasters to radiance, and radiance to reflectance\cr
#' [vegIndex()]: Vegetation indices from surface reflectance\cr
#'
#' ## Properties of **fasterRaster** vectors (`GVector`s)
#' [crs()]: Coordinate reference system\cr
#' [datatype()]: Data type of fields\cr
#' [dim()]: Number of geometries and columns\cr
#' [ext()], [N()], [S()], [E()], [W()], [top()], and [bottom()]: Spatial extent\cr
#' [geomtype()]: Type of vector (points, lines, polygons)\cr
#' [is.2d()] and [is.3d()]: Is an object 2- or 3-dimensional?\cr
#' [is.lonlat()]: Is an object projected (e.g., in WGS84)?\cr
#' [is.points()], [is.lines()], [is.polygons()]: Does a `GVector` represent points, lines, or polygons?\cr
#' [location()]: **GRASS** "location" of an object or the active session\cr
#' [mapset()]: **GRASS** "mapset" of an object or the active session\cr
#' [names()]: Names of `GVector` fields\cr
#' [ncol()]: Number of fields\cr
#' [ngeom()]: Number of geometries (points, lines, polygons)\cr
#' [nrow()]: Number of rows in a vector data table\cr
#' [nsubgeom()]: Number of subgeometries (points, lines, polygons that make up single- and multipart geometries)\cr
#' [sources()]: Name of the vector in **GRASS**\cr
#' [st_bbox()]: Spatial extent\cr
#' [st_crs()]: Coordinate reference system\cr
#' [topology()]: Dimensionality (2D or 3D)\cr
#' [zext()]: Vertical extent\cr
#'
#' ## Functions that operate on or create `GVector`s
#' The `[` ([single bracket][subset_single_bracket]) operator, can be used to subset geometries of a `GRaster`.\cr
#' The `$` [dollar][subset_dollar] and `[[` ([double-square brackets][subset_double_square_brackets]) operators can be used to get columns of a `GVector`'s data table.\cr
#' The `$<-` ([dollar replace][replace_dollar]) operator can be used to replace specific columns of a `GVector`'s data table.\cr
#' [aggregate()]: Combine `GVector` geometries\cr
#' [as.data.frame()]: Convert a `GVector`'s attribute table to a `data.frame`\cr
#' [as.data.table()]: Convert a `GVector`'s attribute table to a `data.table`\cr
#' [as.points()]: Extract vertex coordinates from a "lines" or "polygons" `GVector`\cr
#' [buffer()]: Create a polygon around/inside a `GVector`\cr
#' [cleanGeom()]: Fix undesirable geometries of a `GVector`\cr
#' [connectors()]: Create lines connecting nearest features of two `GVector`\cr
#' [convHull()]: Minimum convex hull\cr
#' [crds()]: Extract coordinates of a `GVector`\cr
#' [crop()]: Remove parts of a `GVector`\cr
#' [delaunay()]: Delaunay triangulation\cr
#' [disagg()]: Separate multipart geometries into singlepart geometries\cr
#' [distance()]: Distance between geometries in two `GVector`, or from a `GVector` to cells of a `GRaster`\cr
#' [extract()]: Extract values from a `GVector` at specific points\cr
#' [head()]: First rows of a `GVector`'s data table\cr
#' [project()]: Change coordinate reference system\cr
#' [simplifyGeom()]: Remove vertices\cr
#' [smoothGeom()]: Remove "angular" aspects of features\cr
#' [st_as_sf()]: Convert a `GVector` to a `sf` vector\cr
#' [st_buffer()]: Create a polygon around/inside a `GVector`\cr
#' [st_distance()]: Distance between geometries in two `GVector`\cr
#' [tail()]: Last rows of a `GVector`'s data table\cr
#' [thinPoints()]: Reduce number of points in same raster cell\cr
#'
#' ## Converting between data types
#' [as.contour()]: Convert a `GRaster` to a `GVector` representing contour lines\cr
#' [as.doub()]: Convert a `GRaster` to a double-floating point raster (***GRASS** data type `DCELL`)\cr
#' [as.data.frame()]: Convert `GVector` to a `data.frame`\cr
#' [as.data.table()]: Convert `GVector` to a `data.table`\cr
#' [as.float()]: Convert a `GRaster` to a floating-point raster (***GRASS** data type `FCELL`)\cr
#' [as.int()]: Convert a `GRaster` to an integer raster (***GRASS** data type `CELL`)\cr
#' [as.points()], [as.lines()], and [as.polygons()]: Convert a `GRaster` to a `GVector`\cr
#' [fast()]: Convert a `SpatRaster`, `SpatVector`, or `sf` vector to a `GRaster` or `GVector`, or load one from a file\cr
#' [categories()] and [levels<-]: Convert an integer raster to a categorical ("factor") raster.
#' [rast()]: Convert a `GRaster` to a `SpatRaster`\cr
#' [rasterize()]: Convert a `GVector` to a `GRaster`\cr
#' [st_as_sf()]: Convert a `GVector` to a `sf` vector\cr
#' [vect()]: Convert a `GVector` to a `SpatVector`\cr
#'
#' ## Functions that operate on **GRASS** "sessions":
#' [crs()]: Coordinate reference system of the current location\cr
#' [location()]: **GRASS** "location" of an object or the active session\cr
#' [mapset()]: **GRASS** "mapset" of an object or the active session\cr
#' [restoreSession()]: Restore a previous **GRASS** session or switch **GRASS** locations/mapsets\cr
#' [removeSession()]: Delete a **GRASS** session (location, mapset(s), and all associated files)\cr
#'
#' ## General purpose functions
#' [appendLists()]: Append values to elements of a list from another list\cr
#' [compareFloat()]: Compare values accounting for differences due to floating point precision\cr
#' [compareGeom()]: Determine if geographic metadata is same between `GRaster`s and/or `GVector`s\cr
#' [dropRows()]: Remove rows from a `data.frame` or `data.table`\cr
#' [forwardSlash()]: Replace backslash with forward slash\cr
#' [grassInfo()]: **GRASS** version and citation\cr
#' [pmatchSafe()]: Partial matching of strings with error checking\cr
#' [replaceNAs()]: Replace `NA`s in columns of a `data.table` or `data.frame`, or in a vector\cr
#' [rstring()]: Create a string statistically likely to be unique\cr
#' [seqToSQL()]: Format a numeric series into an SQL value call.\cr
#' [update()]: Refresh metadata in a `GRaster` or `GVector` object.\cr
#'
#' ## Functions that operate on **GRASS** "regions" (useful mostly to developers):
#' [region()]: Change or report the active region's extent and resolution\cr
#' [regionDim()]: Change or report the active region's resolution (also [dim()] and related functions, with no arguments)
#' [regionExt()]: Change or report the active region's extent (also [ext()] and related functions, with no arguments)\cr
#' [regionRes()]: Change or report the active region's dimensions (also [res()] and related functions, with no arguments)\cr
#'
#' ## Example data
#' [appFunsTable][appFunsTable] (see also [appFuns()]): Functions usable by the [app()] function\cr
#' [madChelsa][madChelsa]: Climate rasters for of a portion of eastern Madagascar\cr
#' [madCoast0][madCoast0], [madCoast4][madCoast4], and [madCoast][madCoast]: Borders of an eastern portion of Madagascar\cr
#' [madCover]: Land cover raster\cr
#' [madCoverCats][madCoverCats]: Table of land cover classes\cr
#' [madDypsis][madDypsis]: Specimens records of species in the genus *Dypsis*\cr
#' [madElev][madElev]: Elevation raster\cr
#' [madForest2000][madForest2000] and [madForest2014][madForest2014]: Forest cover in 2000 and 2014\cr
#' [madLANDSAT][madLANDSAT]: Surface reflectance in 2023\cr
#' [madRivers][madRivers]: Rivers vector\cr
#' [madVegIndices][madVegIndices]: Vegetation indices that can be calculated using [vegIndex()].
#' 
#' ## Esoteric tutorials
#' [Sessions, locations, and mapsets][tutorial_sessions]\cr
#' [Raster data types][tutorial_raster_data_types]\cr
#' [Regions][tutorial_regions]\cr
#' [Undocumented functions][tutorial_undocumented_functions]\cr
#'
#' @docType package
#' @author Adam B. Smith
#' @name fasterRaster
#' @keywords internal
"_PACKAGE"

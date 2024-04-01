#' "fasterRaster": Faster raster and spatial vector processing using "GRASS GIS"
#'
#' @description Processing of large-in-memory/-on disk rasters and spatial vectors in using **GRASS GIS**. Most functions in the **terra** and **sf** packages are recreated. Processing of medium-sized and smaller spatial objects will nearly always be faster using **terra** or **sf**. To use most of the functions you must have the stand-alone version of **GRASS** version 8.0 or higher (not the **OSGeoW4** installer version). Note that due to developer choices, results will not always be strictly comparable between **terra**, **sf**, and **fasterRaster**.
#'
#' ## Most useful tutorials and functions:
#' A [quick-start][tutorial_getting_started] tutorial\cr
#' A tutorial on [raster data types][tutorial_raster_data_types]\cr
#' [faster()]: Set the directory where **GRASS** is installed on your system, and set or get other package-wide options\cr
#' [fast()]: Convert a `SpatRaster`, `SpatVector`, or `sf` vector to **fasterRaster**'s raster format (`GRaster`s) or vector format (`GVector`s), or load one from a file\cr
#' [rast()], [vect()], and [st_as_sf()]: Convert `GRaster`s and `GVector`s to `SpatRaster`s, `SpatVector`s, or `sf` vectors\cr
#' [writeRaster()] and [writeVector()]: Save `GRaster`s or `GVector`s to disk\cr
#'
#' ## Properties of `GRasters`
#' [crs()]: Coordinate reference system\cr
#' [datatype()]: Data type\cr
#' [dim()]: Number of rows and columns\cr
#' [ext()], [N()], [S()], [E()], [W()], [top()], and [bottom()]: Spatial extent\cr
#' [freq()]: Frequencies of cell values in a raster\cr
#' [is.2d()] and [is.3d()]: Is an object 2- or 3-dimensional?\cr
#' [is.int()] / [is.cell()], [is.float()], [is.doub()]: `GRaster` data type (integer/float/double)\cr
#' [is.factor()]: Does a raster represent categorical data?\cr
#' [is.lonlat()]: Is an object projected (e.g., in WGS84)?\cr
#' [levels()]: Names of levels in a categorical `GRaster`\cr
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
#' [topology()]: Dimensionality (2D or 3D)\cr
#' [zext()]: Vertical extent\cr
#' [zres()]: Vertical resolution\cr
#' 
#' ## Functions that operate on or create `GRasters`
#' [Arithmetic]: Mathematical operations on `GRaster`s: `+`, `-`, `*`, `/`, `^`, `%%` (modulus), `%/%` (integer division)\cr
#' [Logical comparisons][Compare-methods]: `<`, `<=`, `==`, `!=`, `>=`, and `>`, plus \code{\link[fasterRaster]{%in%}} and \code{\link[fasterRaster]{%notin%}} (for categorical rasters only)\cr
#'
#' Single-layer functions (applied to each layer of a `GRaster`):
#' - Working with `NA`s: [is.na()] and [not.na()] \cr
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
#' The operators [$] and \code{\link[fasterRaster]{[[}} can be used to subset or remove specific layers of a `GRaster`.\cr
#' The \code{\link[fasterRaster]{[<-}} operator can be used to replace values of cells of a `GRaster`.\cr
#' The assign operators, \code{\link[fasterRaster]{$<-}}, \code{\link[fasterRaster]{[[<-}}, and \code{\link[fasterRaster]{add<-}} can be used to replace specific layers of a `GRaster`.\cr
#' [as.int()], [as.float()], [as.doub()]: Change data type (integer/float/double)\cr
#' [as.contour()]: Contour lines from a `GRaster`\cr
#' [as.lines()]: Convert a `GRaster` to a "lines" vector\cr
#' [as.points()]: Convert a `GRaster` to a "points" vector\cr
#' [as.polygons()]: Convert a `GRaster` to a "polygons" vector\cr
#' [aggregate()]: Aggregate values of `GRaster` cells into larger cells\cr
#' [buffer()]: Create a buffer around non-`NA` cells\cr
#' [app()]: Apply a user-defined function to multiple layers of a `GRaster` (with helper functions [appFuns()] and [appCheck()])\cr
#' [c()]: "Stack" two or more rasters\cr
#' [cellSize()]: Cell area\cr
#' [clump()]: Group adjacent cells with similar values\cr
#' [combineCats()]: Combine values from two or more categorical and/or integer rasters\cr
#' [cor()]: Correlation matrix between layers of a `GRaster`\cr
#' [cov()]: Covariance matrix between layers of a `GRaster`\cr
#' [crop()]: Remove parts of a `GRaster`\cr
#' [denoise()]: Remove "noise" from a `GRaster` using a principal components analysis (PCA)\cr
#' [distance()]: Distance to non-`NA` cells, or vice versa\cr
#' [extend()]: Add rows and columns to a `GRaster`\cr
#' [extract()]: Extract values from a `GRaster` at locations of a `GVector`\cr
#' [fillNAs()]: Fill `NA` cells\cr
#' [focal()]: Calculate cell values based on values of nearby cells\cr
#' [fractalRast()]: Create a fractal `GRaster`\cr
#' [fragmentation()]: Landscape fragmentation class from Riitters et al. (2020)\cr
#' [global()]: Summary statistics across cells of each `GRaster` layer\cr
#' [`hillshade()`][shade]: Create a hillshade `GRaster`\cr
#' [hist()]: Histogram of `GRaster` values\cr
#' [horizonHeight()]: Horizon height\cr
#' [interpIDW()]: Interpolate values at points to a `GRaster`\cr
#' [kernel()]: Kernel density estimator of points\cr
#' [longlat()]: Create longitude/latitude rasters\cr
#' [mask()]: Remove values in a `GRaster` based on values in another `GRaster` or vector\cr
#' [match()], \code{\link[fasterRaster]{%in%}}, and \code{\link[fasterRaster]{%notin%}}: Find which cells of a `GRaster` match or do not match certain values\cr
#' [merge()]: Combine two or more rasters with different extents and fill in `NA`s\cr
#' [noise()]: Remove coarse-scale trends from a `GRaster`, leaving just fine-scale "noise"\cr
#' [pairs()]: Plot correlations between `GRaster` layers\cr
#' [pca()]: Apply a principal components analysis (PCA) to a `GRaster`\cr
#' [pcs()]: Retrieve a principal components model from a PCA `GRaster` generated using `pca()`\cr
#' [plot()]: Display a `GRaster`\cr
#' [plotRGB()]: Display a multispectral `GRaster` using red, blue, green, and alpha channels\cr
#' [project()]: Change coordinate reference system and cell size\cr
#' [predict()]: Make predictions to a `GRaster` from a linear model or generalized linear model\cr
#' [resample()]: Change cell size\cr
#' [rnormRast()]: A random `GRaster` with values drawn from a normal distribution\cr
#' [runifRast()]: A random `GRaster` with values drawn from a uniform distribution\cr
#' [scale()]: Subtract means and divide by standard deviations\cr
#' [selectRange()]: Select values from rasters in a stack based on values in another `GRaster`\cr
#' [spatSample()]: Randomly points from a `GRaster`\cr
#' [spDepRast()]: Create a random `GRaster` with or without spatial dependence\cr
#' [sun()]: Solar radiance and irradiance\cr
#' [terrain()]: Slope, aspect, curvature, and partial slopes\cr
#' [thinLines()]: Reduce linear features on a `GRaster` so linear features are 1 cell wide\cr
#' [topoWetnessIndex()]: Topographic wetness index\cr
#' [trim()]: Remove rows and columns from a `GRaster` that are all `NA`\cr
#' [unscale()]: Multiply raster by standard deviation and add the mean (opposite of [scale()])\cr
#' [zonal()]: Statistics (mean, sum, etc.) on areas of a `GRaster` defined by sets of cells with the same values in another `GRaster`, or by geometries in a `GVector`\cr
#' [zonalGeog()]: Geographic statistics (area, perimeter, fractal dimension, etc.) for sets of cells with the same values\cr
#'
#' ## Functions operating on categorical (factor) rasters
#' [activeCat()] and [activeCats()]: Column(s) that defines category labels\cr
#' \code{\link[fasterRaster]{activeCat<-}}: Set column that defines category labels\cr
#' [addCats()]: Add new columns to a "levels" table\cr
#' \code{\link[fasterRaster]{addCats<-}}: Add new rows (levels) to a "levels" table\cr
#' [categories()]: Set "levels" table for specific layers of a categorical raster\cr
#' [catNames()]: Names of each "levels" table\cr
#' [cats()]: "Levels" table of a categorical raster\cr
#' [combineCats()]: Combine categories from two or more categorical rasters\cr
#' [complete.cases()]: Find rows of a categorical `GRaster`'s "levels" table that have no `NA`s in them\cr
#' [droplevels()]: Remove one or more levels\cr
#' [freq()]: Frequency of each category across cells of a raster\cr
#' [is.factor()]: Is a raster categorical?\cr
#' [levels()]: "Levels" table of a categorical raster\cr
#' \code{\link[fasterRaster]{levels<-}}: Set "levels" table of a categorical raster\cr
#' [match()], \code{\link[fasterRaster]{%in%}}, and \code{\link[fasterRaster]{%notin%}}: Find which cells of a `GRaster` match or do not match certain category labels\cr
#' [minmax()]: "Lowest" and "highest" category values of categorical rasters (when argument `levels = TRUE`)\cr
#' [missing.cases()]: Find rows of a categorical `GRaster`'s "levels" table that have at least one `NA` in them\cr
#' [missingCats()]: Values that have no category assigned to them\cr
#' [nlevels()]: Number of levels\cr
#' [zonalGeog()]: Geographic statistics (area, perimeter, fractal dimension, etc.) for sets of cells with the same values
#' 
#' ## Functions for analysis of remote sensing rasters
#' [compositeRGB()]: Combine red, green, and blue color bands to make a composite `GRaster`\cr
#' [plotRGB()]: Display a multispectral `GRaster` using red, blue, green, and alpha channels\cr
#' [vegIndex()]: Vegetation indices from surface reflectance\cr
#'
#' ## Properties of `GVector`s
#' [crs()]: Coordinate reference system\cr
#' [datatype()]: Data type of fields\cr
#' [dim()]: Number of geometries and columns\cr
#' [expanse()]: Area of polygons or length of lines\cr
#' [ext()], [N()], [S()], [E()], [W()], [top()], and [bottom()]: Spatial extent\cr
#' [geomtype()]: Type of vector (points, lines, polygons)\cr
#' [is.2d()] and [is.3d()]: Is an object 2- or 3-dimensional?\cr
#' [is.lonlat()]: Is an object projected (e.g., in WGS84)?\cr
#' [is.points()], [is.lines()], [is.polygons()]: Does a `GVector` represent points, lines, or polygons?\cr
#' [names()]: Names of `GVector` fields\cr
#' \code{\link[fasterRaster]{names<-}}: Assign names to columns of a `GVector`s data table\cr
#' [ncol()]: Number of fields\cr
#' [ngeom()]: Number of geometries (points, lines, polygons)\cr
#' [nrow()]: Number of rows in a vector data table\cr
#' [nsubgeom()]: Number of subgeometries (points, lines, polygons that make up single- and multipart geometries)\cr
#' [sources()]: Name of the vector in **GRASS**\cr
#' [topology()]: Dimensionality (2D or 3D)\cr
#' [zext()]: Vertical extent\cr
#'
#' ## Functions that operate on or create `GVector`s
#' The \code{\link[fasterRaster]{[}} operator can be used to subset geometries of a `GVector`.\cr
#' The [$] and \code{\link[fasterRaster]{[[}} operators can be used to get columns of a `GVector`'s data table.\cr
#' The \code{\link[fasterRaster]{$<-}} operator can be used to replace specific columns of a `GVector`'s data table.\cr
#' [addTable<-]: Add a data table to a `GVector`\cr
#' [aggregate()]: Combine `GVector` geometries\cr
#' [as.data.frame()]: Convert a `GVector`'s attribute table to a `data.frame`\cr
#' [as.data.table()]: Convert a `GVector`'s attribute table to a `data.table`\cr
#' [as.points()]: Extract vertex coordinates from a "lines" or "polygons" `GVector`\cr
#' [buffer()]: Create a polygon around/inside a `GVector`\cr
#' [cbind()]: Add columns to the data table of a `GVector`\cr
#' [clusterPoints()]: Identify clusters of points\cr
#' [complete.cases()]: Find rows of a `GVector`'s data table that have no `NA`s in them\cr
#' [connectors()]: Create lines connecting nearest features of two `GVector`s\cr
#' [convHull()]: Minimum convex hull\cr
#' [crds()]: Extract coordinates of a `GVector`\cr
#' [crop()]: Remove parts of a `GVector`\cr
#' [delaunay()]: Delaunay triangulation\cr
#' [disagg()]: Separate multipart geometries into singlepart geometries\cr
#' [distance()]: Distance between geometries in two `GVector`, or from a `GVector` to cells of a `GRaster`\cr
#' [dropTable()]: Remove the data table from a `GVector`\cr
#' [erase()] or \code{\link[fasterRaster]{-}}: Remove part of a `GVector` that overlaps with another\cr
#' [expanse()]: Area of polygons or length of lines\cr
#' [extract()]: Extract values from a `GVector` at specific points\cr
#' [grid()]: Create a grid `GVector`\cr
#' [head()]: First rows of a `GVector`'s data table\cr
#' [hexagons()]: Create a hexagonal grid\cr
#' [interpIDW()]: Interpolate values at points to a `GRaster`\cr
#' [interpSplines()]: Interpolate values at points to a `GRaster`\cr
#' [intersect()] or \code{\link[fasterRaster]{*}}: Intersection of two `GVectors`.\cr
#' [kernel()]: Kernel density estimator of points.\cr
#' [missing.cases()]: Find rows of a `GVector`'s data table that have at least `NA` in them\cr
#' [project()]: Change coordinate reference system\cr
#' [rasterize()]: Convert a `GVector` to a `GRaster`\cr
#' [rbind()]: Combine `GVectors\cr
#' [simplifyGeom()]: Remove vertices\cr
#' [smoothGeom()]: Remove "angular" aspects of features\cr
#' [st_as_sf()]: Convert a `GVector` to a `sf` vector\cr
#' [st_buffer()]: Create a polygon around/inside a `GVector`\cr
#' [tail()]: Last rows of a `GVector`'s data table\cr
#' [thinPoints()]: Reduce number of points in same raster cell\cr
#' [union()] or \code{\link[fasterRaster]{+}}: Combine two `GVector`s\cr
#' [xor()] or \code{\link[fasterRaster]{/}}: Select parts of polygons not shared by two `GVector`s\cr
#'
#' ## Functions for fixing issues with `GVector`s
#' [breakPolys()]: Break topologically clean areas\cr
#' [fillHoles()]: Fill "holes" of a `GVector`\cr
#' [fixBridges()]: Change "bridges" to "islands"\cr
#' [fixDangles()]: Change "dangles" hanging off boundaries to lines\cr
#' [fixLines()]: Break lines at intersections and lines that form closed loops\cr
#' [remove0()]: Remove all boundaries and lines with a length of 0\cr
#' [removeAngles()]: Collapse lines that diverge at an angle that is computationally equivalent to 0\cr
#' [removeBridges()]: Remove "bridges" to "islands"\cr
#' [removeDangles()]: Remove "dangling" lines\cr
#' [removeDupCentroids()]: Remove duplicated area centroids\cr
#' [removeDups()]: Remove duplicated features and area centroids\cr
#' [removeSmallPolys()]: Remove small polygons\cr
#' [snap()]: Snap lines/boundaries to each other\cr
#' 
#' ## Converting between data types
#' [as.contour()]: Convert a `GRaster` to a `GVector` representing contour lines\cr
#' [as.doub()]: Convert a `GRaster` to a double-floating point raster (**GRASS** data type `DCELL`)\cr
#' [as.data.frame()]: Convert `GVector` to a `data.frame`\cr
#' [as.data.table()]: Convert `GVector` to a `data.table`\cr
#' [as.float()]: Convert a `GRaster` to a floating-point raster (**GRASS** data type `FCELL`)\cr
#' [as.int()]: Convert a `GRaster` to an integer raster (**GRASS** data type `CELL`)\cr
#' [as.points()], [as.lines()], and [as.polygons()]: Convert a `GRaster` to a `GVector`\cr
#' [fast()]: Convert a `SpatRaster`, `SpatVector`, or `sf` vector to a `GRaster` or `GVector`, or load one from a file\cr
#' [categories()] and [levels<-]: Convert an integer raster to a categorical ("factor") raster.
#' [rast()]: Convert a `GRaster` to a `SpatRaster`\cr
#' [rasterize()]: Convert a `GVector` to a `GRaster`\cr
#' [st_as_sf()]: Convert a `GVector` to a `sf` vector\cr
#' [vect()]: Convert a `GVector` to a `SpatVector`\cr
#'
#' ## General purpose functions
#' [appendLists()]: Append values to elements of a list from another list\cr
#' [compareGeom()]: Determine if geographic metadata is same between `GRaster`s and/or `GVector`s\cr
#' [dropRows()]: Remove rows from a `data.frame` or `data.table`\cr
#' [grassInfo()]: **GRASS** version and citation\cr
#' [replaceNAs()]: Replace `NA`s in columns of a `data.table` or `data.frame`, or in a vector\cr
#' [seqToSQL()]: Format a numeric series into an SQL value call\cr
#' [update()]: Refresh metadata in a `GRaster` or `GVector` object\cr
#'
#' ## Data objects
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
#' [vegIndices][vegIndices]: Vegetation indices that can be calculated using [vegIndex()].
#' 
#' ## Esoteric, exported yet often (publicly) undocumented functions for developers, plus esoteric tutorials
#' Comparisons between `GRegion`s can be performed using the `==` and `!=` operators.\cr
#' Tutorial on **GRASS** [regions][tutorial_regions]\cr
#' Tutorial on **GRASS** ["locations" and mapsets][tutorial_locations_mapsets]\cr
#' `.copyGSpatial()`: Make a copy of the **GRASS** file pointed to by a `GRaster` or `GVector`\cr
#' `.fileExt()`: Get file extension\cr
#' `.exists()`: Does the **GRASS** file of a `GRaster` or `GVector` exist?\cr
#' `.ext()`: Extent from the [sources()] name of a `GRaster` or `GVector`\cr
#' `.geomtype()`: Geometry type ("point", "line", or "area") from the [sources()] name of a `GVector`\cr
#' `.layerIndex()`: Gets the index of `GRaster` layers from a numeric, integer, character, or logical vector\cr
#' `.locationCreate()` Make a connection to **GRASS** (i.e., start **GRASS** from within **R**) and create a location\cr
#' `.locationDelete()` Deletes all files associated with a **GRASS** "location" and mapset\cr
#' `.locationFind()`: Find a specific **GRASS** "location" that already exists\cr
#' `.locationRestore()` Reconnect **GRASS** to a previously-created **GRASS** "location"\cr
#' `.locations()`: List of all available "locations"\cr
#' `.ls()`: Lists the `sources` of all objects in the active **GRASS** "location"\cr
#' `.makeGRaster()` and `.makeGVector()`: Make `GRaster`s or `GVector`s from a vector of `sources`, which are pointers to files in **GRASS**\cr
#' `.makeSourceNames()`: Makes one or more statistically unique strings that can be used as file names to represent rasters or vectors in **GRASS**\cr
#' `.mapset()`: **GRASS** "mapset" of an object or the active session\cr
#' `.message()`: Display a warning or message if the given warning has not been displayed since **fasterRaster** was attached or if a given number or hours has passed since then\cr
#' `.minVal()` and `.maxVal()`: Values in the `@minVal` and `@maxVal` slots in a `GRaster`\cr
#' `.nlevels()`: Number of levels in a `SpatVector`, `data.frame`, `data.table`, empty string, or a list of `data.frame`s, `data.table`s, and/or empty strings.\cr
#' `.plot()`: Plot using the [sources()] name of a `GRaster` or `GVector`\cr
#' `.projection()`: Value of the `@projection` slot in a `GRaster` or `GVector`\cr
#' `.quiet()`: Returns "quiet" if `faster("verbose")` is `TRUE`\cr
#' `.rastInfo()` and `.vectInfo()`: Metadata for a **GRASS** raster or vector\cr
#' `.region()`: Change or report the active region's extent and resolution\cr
#' `.regionDim()]`: Change or report the active region's resolution (also [dim()] and related functions, with no arguments)\cr
#' `.regionExt()`: Change or report the active region's extent (also [ext()] and related functions, with no arguments)\cr
#' `.regionRes()`: Change or report the active region's dimensions (also [res()] and related functions, with no arguments)\cr
#' `.rename()`: Rename a **GRASS** raster or vector\cr
#' `.rm()`: Delete rasters or vectors in **GRASS**\cr
#' `.vAsDataTable()`: Convert the attribute table linked to a vector in **GRASS** to a `data.table`. This table is distinct from the attribute table attached to a `GVector`\cr
#' `.vAttachDatabase()`: Add a database table to the **GRASS** representation of a `GVector`\cr
#' `.vCats()`: Get a `data.table` with a single column named `cat`, which corresponds to the **GRASS** attribute table's `cat` column\cr
#' `.vDetachDatabase(): Detach the **GRASS** database from a **GRASS** vector\cr
#' `.vHasDatabase()`: Tests if **GRASS** vector has a database\cr
#' `.vIncrementCats()`: Increment category values of a `GVector`\cr
#' `.vNames()`: "**GRASS**" vector attribute table column names\cr
#' `.vRecat()`: Change **GRASS** category indices of a **GRASS** vector\cr
#' `.vValidCats()`: Are category values of a vector valid?\cr
#' `grassStarted()`: Has a connection **GRASS** been made within the current **R** session?\cr
#'
#' @docType package
#' @author Adam B. Smith
#' @name fasterRaster
#' @keywords internal
"_PACKAGE"

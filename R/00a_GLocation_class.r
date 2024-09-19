#' Classes for fasterRaster sessions, regions, rasters, and vectors
#'
#' @description The `G` suite of S4 classes contain pointers to **GRASS** objects or metadata about the current **GRASS** session. Most users will manipulate objects using these classes, but do not need to know the details.
#'
#' * The `GLocation` class stores information about the **GRASS** "project"/"location"(see `vignette("10_projects_locations_mapsets", package = "fasterRaster")`), and coordinate reference system. Contained by all the rest.
#'
#' * The `GSpatial` class contains the `GLocation` class and stores information about spatial objects (extent, topology) plus the name of the file representing it in **GRASS** (its `source`). Contained by `GRegion`, `GRaster`, and `GVector`.
#'
#' * The `GRegion` class contains the `GSpatial` class and stores information about grids (dimensions and resolution). They do have `sources`, but these are not used (they're always `NA`). Contained by `GRaster`. The `GRegion` corresponds to **GRASS** "regions", though `GRegion` objects are not actually pointers to **GRASS** "region" files (see `vignette("11_regions", package = "fasterRaster")`).
#'
#' * The `GRaster` class contains the `GRegion` class and represents rasters. It stores information on number of layers, categories, min/max values, and user-friendly names. Categorical `GRaster`s are associated with a "levels" table for representing categorical data (e.g., wetlands, forest, etc.).
#'
#' * The `GVector` class contains the `GSpatial` class and represents spatial vectors. It may or may not have an associated `data.table` (i.e., a `data.frame`), which contains metadata about each geometry in the vector.
#'
#' @slot location	Character (all classes): The **GRASS** "project"/"location" of the object. The default value is `default`. Can be obtained using the hidden function `.location()`. See `vignette("10_projects_locations_mapsets", package = "fasterRaster")`.
#'
#' @slot mapset		Character (all classes): The **GRASS** "mapset". Default value is `PERMANENT`. Typically hidden to users. Can be obtained using the hidden function `.mapset()`. See `vignette("10_projects_locations_mapsets", package = "fasterRaster")`.
#'
#' @slot workDir	Character (all classes): Directory in which **GRASS** stores files.
#'
#' @slot topology	Character (`GSpatial` objects, including `GRegion`s, `GRaster`s, and `GVector`s): Valid values are `2D` (2-dimensional--most rasters and vectors) or `3D` (3-dimensional--e.g., LIDAR data). Can be obtained using [topology()].
#'
#' @slot sources		Character (`GRaster`s and `GVector`s): Name of the object in **GRASS**. These are typically made on-the-fly and provide the pointer to the object from **R** to **GRASS**. Changing them manually will break the connection. Can be obtained using [sources()].
#'
#' @slot names		Character  (`GRaster`s only): Name of a raster or each raster layer in. Can be obtained using [names()].
#'
#' @slot crs		Character (all classes): Coordinate reference systems string (preferably in WKT2 format). Can be obtained using [crs()] or [st_crs()].
#'
#' @slot projection Character: The **GRASS** "projection" for a `GRaster` or `GVector`. Can be obtained using `.projection()`.
#'
#' @slot dimensions	Dimensions:
#' * `GRegion`s and `GRaster`s: Vector of three integers indicating number of rows, columns, and depths (for 3D objects). Can be obtained using [dim()], plus [nrow()], [ncol()], and [ndepth()].
#' * `GVectors`s: Vector of two integers indicating number of geometries and number of fields. Can be obtained using [dim()], plus [nrow()] and [ncol()].
#'
#' @slot extent		Numeric vector with four values (`GSpatial` objects, including `GRegion`s, `GRaster`s, and `GVector`s): Extent of the object listed in order from westernmost longitude, easternmost longitude, southernmost latitude, northernmost latitude. Can be obtained using [ext()].
#'
#' @slot zextent	Numeric (`GSpatial` objects, including `GRegion`s, `GRaster`s, and `GVector`s): Bottom- and top-most extents of 3D `GRaster`s and `GVector`s. Can be obtained using [zext()].
#'
#' @slot geometry	Character (`GVectors`s): Either `points`, `lines`, or `polygons`. Can be obtained using [geomtype()].
#'
#' @slot nLayers Integer (`GRaster`s): Number of layers ("stacked" rasters--different from number of depths of 3D rasters). Can be obtained using [nlyr()].
#'
#' @slot nGeometries Integer (`GVector`s): Number of features (points, lines, or polygons). Can be obtained using [nrow()].
#'
#' @slot datatypeGRASS Character (`GRaster`s): Type of data stored in a raster, as interpreted by `GRASS`. This is either `CELL` (integers), `FCELL` (floating-point values), or `DCELL` (double-values). Can be obtained using [datatype()].
#'
#' @slot resolution	Vector of two numeric values (`GRegion`s, including `GRaster`s): Size of a raster cell in the east-west direction and in the north-south direction. Can be obtained using [res()] and [res3d()].
#'
#' @slot minVal,maxVal Numeric (`GRaster`s): Minimum and maximum value across all cells. Can be obtained using [minmax()].
#'
#' @slot activeCat Integer (`GRaster`s): Column index of the category labels. Must be >0. Note that from the user's standpoint, 1 is subtracted from this number. So a value if `@activeCat` is `2`, then the user would see "1" when printed. Can be obtained using [activeCat()].
#'
#' @slot levels List of `data.table`s (`GRaster`s): Tables for categorical rasters. If a raster is not categorical, the `data.table` is `NULL`, as in `data.table(NULL)`. Can be obtained using `levels()` or `cats()`.
#'
#' @slot table `data.table` (`GVector`s): Table with metadata, one row per geometry (point, line, or plane). If no table is associated with the vector, this must be `data.table(NULL)`. The column with the category value is given in `@catName`.
#'
#' @slot catName Character (`GVector`s): Name of the column in the vector's database that contains category values (integers).
#'
#' @return An object of class `GLocation`, `GSpatial`, `GRegion`, `GRaster`, or `GVector`.
#'
#' @aliases GLocation
#' @rdname GLocation
#' @exportClass GLocation
GLocation <- methods::setClass(
	Class = "GLocation",
	slots = list(
		location = "character",			# GRASS location
		mapset = "character",			# GRASS MAPSET
		crs = "character",				# coordinate reference system
		workDir = "character" 			# working directory
	),
	prototype = prototype(
		location = NA_character_,
		mapset = NA_character_,
		crs = NA_character_,
		workDir = NA_character_
	)
)

setValidity("GLocation",
	function(object) {
		if (length(object@location) != 1L) {
			"@location can only be a single character string."
		} else if (length(object@mapset) != 1L) {
			"@mapset can only be a single character string."
		} else if (length(object@crs) != 1L) {
			"@crs can only be a single character string."
		} else if (length(object@workDir) != 1L) {
			"@workDir can only be a single character string."
		} else {
			TRUE
		}
	} # EOF
)

# GLocation <- function(
	# location = NA_character_,
	# mapset = NA_character_,
	# crs = NA_character_
# ) {
	# methods::new("GLocation", location = location, mapset = mapset, crs = crs)
# }


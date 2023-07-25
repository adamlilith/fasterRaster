#' Classes for fasterRaster sessions, regions, rasters, and vectors
#'
#' @description The `G` suite of S4 classes contain pointers to **GRASS** objects or metadata about the current **GRASS** session. Most users will manipulate objects using these classes, but do not need to know the details.
#'
#' * The `GSession` class stores information about the **GRASS** session ("[location][tutorial_sessions]"), mapset, and coordinate reference system. Contained by all the rest.
#' * The "GSpatial" class contains the `GSession` class and stores information about spatial objects (extent, topology) plus the name of the file representing it in **GRASS** (its `gname`). Contained by `GRegion`, `GRaster`, and `GVector`.
#' * The `GRegion` class contains the `GSpatial` class and stores information about grids (dimensions and resolution). They do have `gname`s, but these are not used (they"re always `NA`). Contained by `GRaster`.
#' * The `GRaster` class contains the `GRegion` class and represents rasters. It stores information on number of layers, categories, min/max values, and user-friendly names.
#' * The `GVector` class contains the `GSpatial` class and represents spatial vectors. It stores information about the **GRASS** names of data tables, names of fields (columns), and their data type.
#' * The `GMetaTable`, `GEmptyMetaTable`, and `GFullMetaTable` classes are "helper" classes intended to store metadata about data tables associated with `GVector`s. The "full" and "empty" classes are contained by `GMetaTable`, and in turn, this is contained by the `GVector` class.
#'
#' @slot location	Character (all classes): The **GRASS** ["location"][tutorial_sessions] of the object. The default value is `default`. Can be obtained using [location()].
#' @slot mapset		Character (all classes): The **GRASS** ["mapset"][tutorial_sessions]. Default value is `PERMANENT`. Typically hidden to users. Can be obtained using [mapset()].
#' @slot topology	Character (`GSpatial` objects, including `GRegion`s, `GRaster`s, and `GVector`s): Valid values are `2D` (2-dimensional--most rasters and vectors) or `3D` (3-dimensional--e.g., LIDAR data). Can be obtained using [topology()].
#' @slot gnames		Character (`GRaster`s and `GVector`s): Name of the object in **GRASS**. These are typically made on-the-fly and provide the pointer to the object from **R** to **GRASS**. Changing them manually will break the connection. Can be obtained using `.gnames()`.
#' @slot names		Character  (`GRaster`s only): Name of a raster or each raster layer in. Can be obtained using [names()].
#' @slot crs		Character (all classes): Coordinate reference systems string (preferably in WKT2 format). Can be obtained using [crs()] or [st_crs()].
#' @slot dimensions	Dimensions:
#' * `GRegion`s and `GRaster`s: Vector of three integers indicating number of rows, columns, and depths (for 3D objects). Can be obtained using [dim()], plus [nrow()], [ncol()], and [ndepth()].
#' * `GVectors`s: Vector of two integers indicating number of geometries and number of fields. Can be obtained using [dim()], plus [nrow()] and [ncol()].
#' @slot extent		Numeric vector with four values (`GSpatial` objects, including `GRegion`s, `GRaster`s, and `GVector`s): Extent of the object listed in order from westernmost longitude, easternmost longitude, southernmost latitude, northernmost latitude. Can be obtained using [ext()].
#' @slot zextent	Numeric (`GSpatial` objects, including `GRegion`s, `GRaster`s, and `GVector`s): Bottom- and top-most extents of 3D `GRaster`s and `GVector`s. Can be obtained using [zext()].
#' @slot geometry	Character (`GVectors`s): Either `points`, `lines`, or `polygons`. Can be obtained using [geomtype()].
#' @slot nLayers Integer (`GRaster`s): Number of layers ("stacked" rasters--different from number of depths of 3D rasters). Can be obtained using [nlyr()].
#' @slot nGeometries Integer (`GVector`s): Number of features (points, lines, or polygons). Can be obtained using [nrow()].
#' @slot datatypeGRASS Character (`GRaster`s): Type of data stored in a raster, as interpreted by `GRASS`. This is either `CELL` (integers), `FCELL` (floating-point values), or `DCELL` (double-values). Can be obtained using [datatype()].
#' @slot resolution	Vector of two numeric values (`GRegion`s, including `GRaster`s): Size of a raster cell in the east-west direction and in the north-south direction. Can be obtained using [res()] and [res3d()].
#' @slot nCats Integer (`GRaster`s): Number of categories. Must be >0. Can be obtained using [ncat()].
#' @slot minVal,maxVal Numeric (`GRaster`s): Minimum and maximum value across all cells. Can be obtained using [minmax()].
#' @slot dbLayer,dbName Character ("GVector"s): Names of the layer and table associated with the `GVector` in the **GRASS** SQLite database. Can be obtained using `.dbName()` and `.dblayer()`.
#' @slot fields Names of fields (`GVector`s). Can be obtained using [names()].
#' @slot numFields Number of fields (`GVector`s). Can be obtained using [ncol()].
#' @slot fieldClasses Classes of fields (`GVector`s). Can be obtained using [datatype()].
#'
#' @return An object of class `GSession`, `GSpatial`, `GRaster`, or `GVector`.
#'
#' @importFrom methods new
#' @exportClass GSession
GSession <- setClass(
	Class = "GSession",
	slots = list(
		location = "character",			# GRASS location
		mapset = "character",			# GRASS MAPSET
		crs = "character"				# coordinate reference system
	),
	prototype = prototype(
		location = NA_character_,
		mapset = NA_character_,
		crs = NA_character_
	)
)

setValidity("GSession",
	function(object) {
		if (length(object@location) != 1L) {
			"@location can only be a single character string."
		} else if (length(object@mapset) != 1L) {
			"@mapset can only be a single character string."
		} else if (length(object@crs) != 1L) {
			"@crs can only be a single character string."
		} else {
			TRUE
		}
	} # EOF
)

# GSession <- function(
	# location = NA_character_,
	# mapset = NA_character_,
	# crs = NA_character_
# ) {
	# new("GSession", location = location, mapset = mapset, crs = crs)
# }


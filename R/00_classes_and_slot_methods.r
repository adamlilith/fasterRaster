#' @title 'fasterRaster' classes for locations, rasters, and vectors
#'
#' @description S4 classes that contains **GRASS** object representations in **R**. Most users will manipulate objects using these classes, but do not need to know the details.
#'
#' @slot location	Character: The **GRASS** [location] of the object. Default value is `default`. Typically hidden to users. Can be obtained using [location()].
#' @slot mapset	Character: The **GRASS** mapset. Defaiult value is `PERMANENT`. Typically hidden to users. Can be obtained using [mapset()].
#' @slot gname		Character: Name of the object in **GRASS**. These names are made on-the-fly and provide the pointer to the object from **R** to **GRASS**.  Changing them will break the connection.
#' @slot names		Character  (`GRaster`s only): Name of a raster or each raster layer in. Can be obtained using [names()].
#' @slot crs		Character: Coordinate reference systems string (preferably in WKT2 format). Can be obtained using [crs()] or [st_crs()].
#' @slot dimensions	Vector of four integers (`GRaster`s only): Number of rows, columns, depths (for 3D rasters), and layers of a "stack" of rasters. Can be obtained using [dim()] and [dim3d()].
#' @slot extent		Numeric vector with four values: Extent of the object listed in order from westernmost longitude, easternmost longitude, southernmost latitude, northernmost latitude. Can be obtained using [ext()].
#' @slot ztop,zbottom	Numeric: Top- and bottom-most elevations of 3D `GRaster`s and `GVector`s. Can be obtained using [zExt()].
#' @slot topology	Character: Valid values are `2D` (2-dimensional--most rasters and vectors) or `3D` (3-dimensional--e.g., as in LIDAR data). Can be obtained using [topology()].
#' @slot geometry	Character (`GVectors`s only): Either `points`, `lines`, or `polygons`. Can be obtained using [geometry()].
#' @slot datatypeGRASS Character (`GRaster`s only): Type of data stored in a raster, as interpreted by `GRASS`. This is either `CELL` (integers), `FCELL` (floating-point values), or `DCELL` (double-values). Can be obtained using [datatype()].
#' @slot resolution	Vector of two numeric values (`GRaster`s only): Size of a raster cell in the east-west direction and in the north-south direction. Can be obtained using [res()] and [res3d()].
#' @slot numCategories Integer (`GRaster`s only): Number of categories. Must be >0. Can be obtained using [numCats()].
#' @slot minVal,maxVal Numeric (`GRaster`s only): Minimum and maximum value across all cells. Can be obtained using [minmax()].
#'
#' @return An object of class `GSession`, `GSpatial`, `GRaster`, or `GVector`.
#'
#' @details `GRaster` and `GVector` are the main "working" classes for **fasterRaster** functions--most user will not need to engage directly with the others. `GSpatial` is contained by `GRaster` and `GVector` and is the general spatial class. It may be extended to include **GRASS** regions later. `GSession` is the location class and contained by all the rest.
#'
#' @export

### CONTENTS ###
### location class and slot methods ###
### spatial class and slot methods ###
### raster class and slot methods ###
### vector class and slot methods ###

### location class and slot methods ###
#######################################

	setClass(
		Class = 'GSession',
		slots = list(
			location = 'character',			# GRASS location
			mapset = 'character',			# GRASS MAPSET
			crs = 'character'				# coordinate reference system
		),
		prototype = prototype(
			location = NA_character_,
			mapset = NA_character_,
			crs = NA_character_
		)
	)
	
	setValidity('GSession',
		function(object) {
			if (length(object@location) != 1L) {
				'@location can only be a single character string.'
			} else if (length(object@mapset) != 1L) {
				'@mapset can only be a single character string.'
			} else if (length(object@crs) != 1L) {
				'@crs can only be a single character string.'
			} else {
				TRUE
			}
		} # EOF
	)
	
	GSession <- function(
		location = NA_character_,
		mapset = NA_character_,
		crs = NA_character_
	) {
		new('GSession', location = location, mapset = mapset, crs = crs)
	}

	# show
	setMethod(
		f = 'show', 
		signature = 'GSession',
		definition = function(object) {
			cat('class       :', paste(class(object), collapse=', '), '\n')
			cat('location    :', object@location, '\n')
			cat('mapset      :', object@mapset, '\n')
			cat('coord ref.  :', object@crs, '\n')
		}
	)

	# print
	setMethod(
		f = 'print',
		signature = 'GSession',
		definition = function(x) show(x)
	)

### spatial class and slot methods ###
######################################

	# general class for rasters and vectors (and maybe regions?)
	setClass(
		Class = 'GSpatial',
		contains = 'GSession',
		slots = list(
			gname = 'character',			# name in GRASS
			topology = 'character',
			extent = 'numeric',				# extent (4 numerics)
			ztop = 'numeric', 				# top elevation for 3D raster/vector
			zbottom = 'numeric' 			# bottom elevation for 3D raster/vector
		),
		prototype = prototype(
			gname = NA_character_,
			topology = NA_character_,
			extent = c(NA_real_, NA_real_, NA_real_, NA_real_),
			ztop = NA_real_,
			zbottom = NA_real_
		)
	)

	setValidity('GSpatial',
		function(object) {
			if (length(object@location) != 1L) {
				'@location can only be a single character string.'
			} else if (length(object@mapset) != 1L) {
				'@mapset can only be a single character string.'
			} else if (length(object@crs) != 1L) {
				'@crs can only be a single character string.'
			} else if (!all(object@topology %in% c(NA_character_, '2D', '3D'))) {
				'@topology can only be a NA, <2D>, or <3D>.'
			} else {
				TRUE
			}
		} # EOF
	)

	GSpatial <- function(
		location = NA_character_,
		mapset = NA_character_,
		crs = NA_character_,
		gname = NA_character_,
		topology = NA_character_,
		extent = c(NA_real_, NA_real_, NA_real_, NA_real_),
		ztop = NA_real_,
		zbottom = NA_real_
	) {
		new(
			'GSpatial',
			location = location,
			mapset = mapset,
			crs = crs,
			gname = gname,
			topology = topology,
			extent = extent,
			ztop = ztop,
			zbottom = zbottom
		)
	}

	# show
	setMethod(
		f = 'show',
		signature = 'GSpatial',
		definition = function(object) {

			digs <- min(3, getOption('digits'))
			extent <- round(object@extent, digs)

			cat('class       :', paste(class(object), collapse=', '), '\n')
			cat('topology    :', object@topology, '\n')
			if (getFastOptions('details')) {
				cat('gname       :', object@gname, '\n')
				cat('location    :', object@location, '\n')
				cat('mapset      :', object@mapset, '\n')
			}
			cat('extent      :', paste(extent, collapse=', '), '(xmin, xmax, ymin, ymax)\n')
			cat('coord ref.  :', object@crs, '\n')
		}
	)

	# print
	setMethod(
		f = 'print',
		signature = 'GSpatial',
		definition = function(x) show(x)
	)

### raster class and slot methods ###
#####################################

	setClass(
		'GRaster',
		contains = 'GSpatial',
		slots = list(
			names = 'character',
			datatypeGRASS = 'character',
			dimensions = 'integer',
			resolution = 'numeric',
			numCategories = 'integer',
			minVal = 'numeric',
			maxVal = 'numeric'
		),
		prototype = prototype(
			names = NA_character_,
			datatypeGRASS = NA_character_,
			dimensions = c(NA_integer_, NA_integer_, NA_integer_),
			resolution = c(NA_real_, NA_real_, NA_real_),
			numCategories = NA_integer_,
			minVal = NA_real_,
			maxVal = NA_real_
		)
	)

	setValidity('GRaster',
		function(object) {
			if (!all(object@datatypeGRASS %in% c('CELL', 'FCELL', 'DCELL'))) {
				'@datatypeGRASS can only be <CELL>, <FCELL>, or <DCELL>.'
			} else if (length(names(object)) != nlyr(object)) {
				'Number of names does not match number of layers.'
			} else if (any(object@dimensions[1L:2L] <= 0L)) {
				'First two values in @dimensions must be positive integers.'
			} else if (!is.na(object@dimensions[3L]) && object@dimensions[3L] <= 0L) {
				'Third value in @dimensions must be NA or a positive integer.'
			} else if (object@dimensions[4L] <= 0L) {
				'Fourth value in @dimensions must be a positive integer.'
			} else if (any(object@resolution[1L:2L] <= 0)) {
				'Values in @resolution must be a positive real values.'
			} else if (!is.na(object@resolution[3L]) && object@resolution[3L] <= 0) {
				'Third value in @resolution must be NA or a positive real.'
			} else {
				TRUE
			}
		} # EOF
	)

	GRaster <- function(
		location = NA_character_,
		mapset = NA_character_,
		crs = NA_character_,
		gname = NA_character_,
		topology = NA_character_,
		extent = c(NA_real_, NA_real_, NA_real_, NA_real_),
		ztop = NA_real_,
		zbottom = NA_real_,
		names = NA_character_,
		datatypeGRASS = NA_character_,
		dimensions = c(NA_integer_, NA_integer_, NA_integer_, NA_integer_),
		resolution = c(NA_real_, NA_real_, NA_real_),
		numCategories = NA_integer_,
		minVal = NA_real_,
		maxVal = NA_real_
	) {
		new(
			'GRaster',
			location = location,
			mapset = mapset,
			crs = crs,
			gname = gname,
			topology = topology,
			extent = extent,
			ztop = ztop,
			zbottom = zbottom,
			names = names,
			datatypeGRASS = datatypeGRASS,
			dimensions = dimensions,
			resolution = resolution,
			numCategories = numCategories,
			minVal = minVal,
			maxVal = maxVal
		)
	}
	# show
	setMethod(
		f='show',
		signature='GRaster',
		definition = function(object) {

			digs <- min(5, getOption('digits'))
			ress <- round(object@resolution, digs)
			
			digs <- min(1, getOption('digits'))
			extent <- round(object@extent, digs)

			crs <- object@crs
			crs <- sf::st_crs(crs)
			crs <- crs$input

			cat('class       :', paste(class(object), collapse=', '), '\n')
			cat('topology    :', object@topology, '\n')
			cat('datatype    :', object@datatypeGRASS, '(GRASS)\n')
			if (getFastOptions('details')) {
				cat('gname       :', object@gname, '\n')
				cat('location    :', object@location, '\n')
				cat('mapset      :', object@mapset, '\n')
			}
			cat('dimensions  :', paste(object@dimensions, collapse=', '), '(nrow, ncol, depths, layers)\n')
			cat('resolution  :', paste(ress, collapse=', '), '(x, y, z)\n')
			cat('extent      :', paste(extent, collapse=', '), '(xmin, xmax, ymin, ymax)\n')
			cat('top ext.    :', object@ztop, '\n')
			cat('bottom ext. :', object@zbottom, '\n')
			cat('coord ref.  :', crs, '\n')
			cat('name(s)     :', object@names, '\n')
			cat('num. categ. :', object@numCategories, '\n')
			cat('min. value  :', object@minVal, '\n')
			cat('max. value  :', object@maxVal, '\n')
		}
	)

	# print
	setMethod(f='print', signature='GRaster',
		definition = function(x) {
			show(x)
		}
	)

### vector class and slot methods ###
#####################################

	setClass(
		'GVector',
		contains = 'GSpatial',
		slots = list(
			geometry = 'character',
			fields = 'character',
			fieldClasses = 'character'
		),
		prototype = prototype(
			geometry = NA_character_,
			fields = NA_character_,
			fieldClasses = NA_character_
		)
	)

	setValidity('GVector',
		function(object) {
			if (!(object@geometry %in% c(NA_character_, 'points', 'lines', 'polygons'))) {
				'@geometry can only be a NA, <points>, <lines>, or <polygons>.'
			} else {
				TRUE
			}
		} # EOF
	)

	GVector <- function(
		location = NA_character_,
		mapset = NA_character_,
		crs = NA_character_,
		gname = NA_character_,
		topology = NA_character_,
		extent = c(NA_real_, NA_real_, NA_real_, NA_real_),
		ztop = NA_real_,
		zbottom = NA_real_,
		geometry = NA_character_,
		fields = NA_character_,
		fieldClasses = NA_character_
	) {
		new(
			'GVector',
			location = location,
			mapset = mapset,
			crs = crs,
			gname = gname,
			topology = topology,
			extent = extent,
			ztop = ztop,
			zbottom = zbottom,
			geometry = geometry,
			fields = fields,
			fieldClasses = fieldClasses
		)
	}

	# show
	setMethod(f='show', signature='GVector',
		definition = function(object) {

			digs <- min(3, getOption('digits'))
			extent <- round(object@extent, digs)

			# fields
			fields <- object@fields
			fieldClasses <- object@fieldClasses
			nFields <- length(fields)
			if (length(fields) > 5L) {
				fields <- fields[1L:5L]
				fields[6L] <- paste0('(and ', nFields - 5L, ' more)')

				fieldClasses <- fieldClasses[1L:5L]
				fieldClasses[6L] <- '     '
			}

			# field classes
			fieldClasses[fieldClasses == 'character'] <- '<chr>'
			fieldClasses[fieldClasses == 'integer'] <- '<int>'
			fieldClasses[fieldClasses == 'numeric'] <- '<num>'

			crs <- object@crs
			crs <- sf::st_crs(crs)
			crs <- crs$input

			cat('class       :', paste(class(object), collapse=', '), '\n')
			cat('topology    :', object@topology, '\n')
			cat('geometry    :', object@geometry, '\n')
			if (getFastOptions('details')) {
				cat('gname       :', object@gname, '\n')
				cat('location    :', object@location, '\n')
				cat('mapset      :', object@mapset, '\n')
			}
			cat('extent      :', paste(extent, collapse=', '), '(xmin, xmax, ymin, ymax)\n')
			cat('vert. ext.  :', paste(object@zbottom, object@ztop, collapse=', '), '(bottom, top)\n')
			cat('coord ref.  :', crs, '\n')
			# cat('fields      :', paste(fields, collapse=', '), '\n')
			# cat('type        :', paste(fieldClasses, collapse=', '), '\n')

			cat('fields      :', fieldClasses[1L], fields[1L], '\n')
			if (nFields > 1L){
				for (i in 2L:length(fields)) {
					cat('             ', fieldClasses[i], fields[i], '\n')
				}
			}


		}
	)

	# print
	setMethod(f='print', signature='GVector',
		definition = function(x) {
			show(x)
		}
	)

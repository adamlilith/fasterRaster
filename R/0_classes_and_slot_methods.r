#' @title 'fasterRaster' classes for locations, rasters, and vectors
#'
#' @description S4 classes that contains **GRASS** object representations in **R**. Most users will manipulate objects using these classes, but do not need to know the details.
#'
#' @slot location	Character: The **GRASS** location of the object. Default value is `default`. Typically hidden to users.
#' @slot mapset	Character: The **GRASS** mapset. Defaiult value is `PERMANENT`. Typically hidden to users.
#' @slot gname		Character: Name of the object in **GRASS**. These names are made on-the-fly and provide the pointer to the object from **R** to **GRASS**.  Changing them will break the connection.
#' @slot rname		Character: Name of a raster or each raster layer in **R** (obtained typiclaly using [names()]).
#' @slot crs		Character: Coordinate reference systems string (preferably in WKT2 format).
#' @slot extent		Numeric vector with four values: Extent of the object listed in order from westernmost longitude, easternmost longitude, southernmost latitude, northernmost latitude.
#' @slot topology	Character: Valid values are `2D` (2-dimensional--most rasters and vectors) or `3D` (3-dimensional--e.g., as in LIDAR data).
#' @slot geometry	Character: For vectors, this is either `points`, `lines`, or `polygons`.
#' @slot datatypeGRASS Character: Type of data stored in a raster, as interpreted by `GRASS`. This is either `CELL` (integers), `FCELL` (floating-point values), or `DCELL` (double-values).
#' @slot dimensions	Vector of three integers: Number of rows, columns, and layers of a raster or "stack" of rasters.
#' @slot resolution	Vector of two numeric values: Size of a raster cell in the east-west direction and in the north-south direction.
#' @slot numCategories Integer: Number of categories. Only >0 for categorical rasters.
#' @slot minVal,maxVal Numeric: Minimum and maximum value across all cells of a raster.
#' @slot zbottom,ztop For 3D rasters and vector, this is the lowest and highest elevation.
#'
#' @return An object of class `GLocation`, `GSpatial`, `GRaster`, or `GVector`.
#'
#' @details `GRaster` and `GVector` are the main "working" classes--most user will not need to engage directly with the others. `GSpatial` is contained by `GRaster` and `GVector` and is the general spatial class. It may be extended to include **GRASS** regions later. `GLocation` is the location class and contained by all the rest.
#'
#' @export

### CONTENTS ###
### location class and slot methods ###
### spatial class and slot methods ###
### raster class and slot methods ###
### vector class and slot methods ###

### location class and slot methods ###
#######################################

	GLocation <- setClass(
		Class = 'GLocation',
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

	# CRS
	if (!isGeneric('crs')) crs.GLocation <- setGeneric(name='crs', def=function(x) { standardGeneric('crs') })
	setMethod(
		f='crs',
		signature='GLocation',
		definition=function(x) x@crs
	)

	# CRS
	if (!isGeneric('st_crs')) st_crs.GLocation <- setGeneric(name='st_crs', def=function(x) { standardGeneric('st_crs') })
	setMethod(
		f='st_crs',
		signature='GLocation',
		definition=function(x) {
			out <- x@crs
			out <- sf::st_crs(out)
			out
		}
	)

	# show
	setMethod(f='show', signature='GLocation',
		definition = function(object) {
			cat('class       :', paste(class(object), collapse=', '), '\n')
			cat('location    :', object@location, '\n')
			cat('mapset      :', object@mapset, '\n')
			cat('coord ref.  :', object@crs, '\n')
		}
	)

	# print
	setMethod(f='print', signature='GLocation',
		definition = function(x) {
			show(x)
		}
	)

### spatial class and slot methods ###
######################################

	# general class for rasters and vectors (and maybe regions?)
	GSpatial <- setClass(
		Class = 'GSpatial',
		contains = 'GLocation',
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

	# show
	setMethod(f='show', signature='GSpatial',
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
	setMethod(f='print', signature='GSpatial',
		definition = function(x) {
			show(x)
		}
	)

### raster class and slot methods ###
#####################################

	GRaster <- setClass(
		'GRaster',
		contains = 'GSpatial',
		slots = list(
			rname = 'character',
			datatypeGRASS = 'character',
			dimensions = 'integer',
			resolution = 'numeric',
			numCategories = 'integer',
			minVal = 'numeric',
			maxVal = 'numeric'
		),
		prototype = prototype(
			rname = NA_character_,
			datatypeGRASS = NA_character_,
			dimensions = c(NA_integer_, NA_integer_, NA_integer_),
			resolution = c(NA_real_, NA_real_, NA_real_),
			numCategories = NA_integer_,
			minVal = NA_real_,
			maxVal = NA_real_
		)
	)

	# rname (hidden)
	rname.GRaster <- setGeneric(name='rname', def=function(x) { standardGeneric('rname') })
	setMethod(
		f = 'rname',
		signature = 'GRaster',
		definition = function(x) x@rname
	)

	# show
	setMethod(
		f='show',
		signature='GRaster',
		definition = function(object) {

			digs <- min(3, getOption('digits'))
			ress <- round(object@resolution, digs)
			extent <- round(object@extent, digs)
			minVal <- round(object@minVal, digs)
			maxVal <- round(object@maxVal, digs)

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
			cat('dimensions  :', paste(object@dimensions, collapse=', '), '(nrow, ncol, depths, nlayers)\n')
			cat('resolution  :', paste(ress, collapse=', '), '(x, y, z)\n')
			cat('extent      :', paste(extent, collapse=', '), '(xmin, xmax, ymin, ymax)\n')
			cat('top ext.    :', object@ztop, '\n')
			cat('bottom ext. :', object@zbottom, '\n')
			cat('coord ref.  :', crs, '\n')
			cat('rname       :', object@rname, '\n')
			cat('num. categ. :', object@numCategories, '\n')
			cat('min. value  :', minVal, '\n')
			cat('max. value  :', maxVal, '\n')
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

	GVector <- setClass(
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

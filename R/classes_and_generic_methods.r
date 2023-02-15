#' @title GRASS class
#'
#' @description An S4 class that contains 'GRASS' object representations in 'R'
#'
#' @slot name		Character: Name of the object.
#' @slot location	Character: The `GRASS` [location] of the object.
#' @slot extent		Numeric vector with 4 values: Extent of the object listed in order from westernmost longitude, easternmost longitude, southernmost latitude, northernmost latitude
#' @slot CRS		Character. Coordinate reference systems string (preferably in WKT format).
#'
#' @details This class is contained by all other `GRASS` classes so is as general as possible.
#' @export GRASS

GRASS <- setClass(
	Class = 'GRASS',
	slots = list(
		name = 'character',				# name in R and GRASS
		location = 'character',			# GRASS location
		MAPSET = 'character',			# GRASS MAPSET
		extent = 'numeric',				# extent (4 numerics)
		crs = 'character'				# coordinate reference system
	),
	prototype = prototype(
		name = NA_character_,
		location = NA_character_,
		extent = NA_real_,
		crs = NA_character_
	)
)

test <- new('GRASS', name='trial', location='examples', extent=c(10, 20, 0, 15), crs='la la la la')

setMethod(f='names', signature='GRASS', definition=function(x) x@name)
setMethod(f='ext', signature='GRASS', definition=function(x) c(xmin=x@extent[1L], xmax=x@extent[2L], ymin=x@extent[3L], ymax=x@extent[4L]))
setMethod(f='crs', signature='GRASS', definition=function(x) x@crs)

setMethod(f='show', signature='GRASS',
	definition = function(object) {
		cat('name        :', object@name, '\n')
		cat('class       : GRASS\n')
		cat('location    :', object@location, '\n')
		cat('MAPSET      :', object@MAPSET, '\n')
		cat('extent      :', paste(object@extent, collapse=', '), '(xmin, xmax, ymin, ymax)\n')
		cat('coord ref.  :', object@crs, '\n')
	}
)

setMethod(f='print', signature='GRASS',
	definition = function(x) {
		cat('name        :', x@name, '\n')
		cat('class       : GRASS\n')
		cat('location    :', x@location, '\n')
		cat('MAPSET      :', x@MAPSET, '\n')
		cat('extent      :', paste(x@extent, collapse=', '), '(xmin, xmax, ymin, ymax)\n')
		cat('coord ref.  :', x@crs, '\n')
	}
)

setGeneric(name='location', def=function(x) { standardGeneric('location') })
setMethod(
	f='location',
	signature='GRASS',
	definition=function(x) x@location
)

setGeneric(name='MAPSET', def=function(x) { standardGeneric('MAPSET') })
setMethod(
	f='MAPSET',
	signature='GRASS',
	definition=function(x) x@MAPSET
)




### GRASSRaster
setClass(
	'GRASSRaster',
	contains = 'GRASS',
	slots = list(
		topology = 'character',
		datatypeGRASS = 'character',
		dimensions = c(x = 'numeric', y = 'numeric'),
		resolution = c(x = 'numeric', y = 'numeric'),
		range = c(min = 'numeric', max = 'numeric')
	)
)

### GRASSVector
setClass(
	Class = 'GRASSRaster',
	contains = 'GRASS',
	slots = list(
		topology = 'character',
		MORE = 'ANY'
	)
)

### GRASSRegion
setClass(
	Class = 'GRASSRaster',
	contains = 'GRASS',
	slots = list(
		dimensions_x = 'numeric',
		dimensions_y = 'numeric',
		resolution_ew = 'numeric',
		resolution_ns = 'numeric'
	)
)

setMethod('initialize', 'GRASSRaster', 
	function(.Object, ...) {
		.Object <- callNextMethod()
		if (
			(.Object@dimensions[1L] < 1 || .Object@dimensions[1L] %% 1 != 0) |
			(.Object@dimensions[2L] < 1 || .Object@dimensions[2L] %% 1 != 0)) {
				stop('Dimensions must be positive integers.')
			}
		.Object
	}
)

setValidity(
	'GRASSRaster',
	function(object) {
		if (
			(.Object@dimensions[1L] < 1 || .Object@dimensions[1L] %% 1 != 0) |
			(.Object@dimensions[2L] < 1 || .Object@dimensions[2L] %% 1 != 0)) {
				stop('Dimensions must be positive integers.')
			} else {
				TRUE
			}
	}
)




r <- new(
	'GRASSRaster',
	name = 'myNewRaster',
	datatypeGRASS = 'FCELL',
	dimensions = c(100, 200),
	resolution = c(10, 11.5),
	extent = c(10, 20, 0, 15),
	CRS = enmSdmX::getCRS('madAlbers'),
	range = c(12, 23.3)
)

setMethod(
	'show',
	'GRASSRaster',
	function(x) {
	
		# dims <- 
	
		cat('class:        GRASSRaster\n'),
		cat('name:             ', x@name, '\n'),
		cat('GRASS datatype:   ', datatypeGRASS, '\n'),
		cat('dimensions:       ', paste(x@dimensions, collapse=', '), ' (nrow, ncol)\n'),
		cat('resolution:       ', paste(x@resolution, collapse=', '), ' (x, y)\n'),
		cat('extent:           ', paste(x@extent, collapse=', '), ' (xmin, xmax, ymin, ymax)\n'),
		cat('coord. ref.:      ', x@CRS, '\n'),
		cat('minimum value:    ', x@minValue, '\n'),
		cat('maximum value:    ', x@maxValue, '\n')
	
	}
)

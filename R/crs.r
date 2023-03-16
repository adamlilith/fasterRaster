#' Coordinate reference system of a `GRaster` or `GVector`
#'
#' Get the coordinate reference system (CRS) of a `GRaster`, `GVector`, `GSpatial`, or `GSession` object.
#'
#' @param x An object that inherits from a `GSession` (i.e., a `GRaster` or `GVector`) or missing, in which case the coordinate reference system of the currently active **GRASS** [location] is reported.
#'
#' @return Character.
#'
#' @example man/examples/example_GRaster.r
#'
#' @export
# if (!isGeneric('crs')) setGeneric(name='crs', def=function(x, ...) standardGeneric('crs'))
setMethod(
	f = 'crs',
	signature = 'GSession',
	definition = function(x) x@crs
)
setMethod(
	f = 'crs',
	signature = 'missing',
	definition = function(x) {
		workDir <- getFastOptions('workDir')
		location <- getFastOptions('location')
		file <- file.path(workDir, location, 'crs.rds')
		if (file.exists(file)) {
			out <- readRDS(file)
		} else {
			out <- NA_character_
		}
		out
	}
)

#' @name st_crs
#' @title Coordinate reference system of a 'GRaster' or 'GVector'
#' @rdname crs
#' @export
# if (!isGeneric('st_crs')) setGeneric(name = 'st_crs', def = function(x, ...) standardGeneric('st_crs'), signature = 'x')
# setGeneric(name = 'st_crs', def = function(x, ...) standardGeneric('st_crs'), signature = 'x')
setMethod(
	f = 'st_crs',
	signature = 'GSession',
	definition = function(x) {
		out <- x@crs
		out <- sf::st_crs(out)
		out
	}
)
setMethod(
	f = 'st_crs',
	signature = 'missing',
	definition = function(x) crs()
)

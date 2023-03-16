#' GRASS "location" and "mapset" of an object or the active session
#'
#' @description **GRASS** [location]s are sets of one or more rasters and/or vectors with the same coordinate reference systems, and may or may not represent the same actual location on Earth. **GRASS** "mapsets" are like subfolders of locations, and are collections of rasters and/or vectors typically related to the same general project.  Typical users will not need to make changes to the default location (called "location") or mapset (called "PERMANENT").
#'
#' @param x A Either:\cr
#' Missing: Reports location or mapset of currently active session\cr
#  A `GSession` object or an object that contains the `GSession` class (i.e., a `GSpatial` object: a `GRaster` or `GVector`).
#'
#' @return A character string.
#'
#' @seealso **GRASS** [locations and mapsets][https://grass.osgeo.org/grass82/manuals/grass_database.html]
#'
#' @example man/examples/example_sessions.r
#'
#' @export
# if (!isGeneric('location')) setGeneric(name = 'location', def = function(x) standardGeneric('location'))
setMethod(
	f = 'location',
	signature = 'GSession',
	definition = function(x) x@location
)
setMethod(
	f = 'location',
	signature = 'missing',
	definition = function(x) getFastOptions('location')
)

#' @name mapset
#' @title "Mapset" of a 'G-' object
#' @rdname location
#' @export
# if (!isGeneric('mapset')) setGeneric(name = 'mapset', def = function(x) standardGeneric('mapset'))
setMethod(
	f = 'mapset',
	signature = 'GSession',
	definition = function(x) x@mapset
)

setMethod(
	f = 'mapset',
	signature = 'missing',
	definition = function(x) getFastOptions('mapset')
)

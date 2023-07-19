#' @title GRASS "location" of an object or the active session
#'
#' @description **GRASS** ["locations"][tutorial_sessions] are sets of one or more rasters and/or vectors with the same coordinate reference systems, and may or may not represent the same actual location on Earth. **GRASS** "mapsets" are like sub-folders of locations, and are collections of rasters and/or vectors typically related to the same general project. Typical users will not need to make changes to the default location (called "location") or mapset (called "PERMANENT").
#'
#' @param x A Either:
#' * Missing: Reports location of currently active [session][tutorial_sessions].
#  * A `GSession` object or an object that contains the `GSession` class (i.e., a `GSpatial` object: a `GRaster` or `GVector`).
#'
#' @return A character string.
#'
#' @seealso **GRASS** [locations and mapsets](https://grass.osgeo.org/grass82/manuals/grass_database.html)
#'
#' @example man/examples/ex_sessions.r
#'
#' @aliases location
#' @rdname location
#' @exportMethod location
methods::setMethod(
	f = 'location',
	signature = 'GSession',
	definition = function(x) unname(x@location)
)

#' @rdname location
#' @aliases location
#' @exportMethod location
methods::setMethod(
	f = 'location',
	signature = 'missing',
	definition = function(x) unname(getFastOptions('location'))
)

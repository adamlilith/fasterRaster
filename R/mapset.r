#' @title GRASS "mapset" of an object or the active session
#'
#' @description **GRASS** "[locations][tutorial_sessions]" are sets of one or more rasters and/or vectors with the same coordinate reference systems, and may or may not represent the same actual location on Earth. **GRASS** "mapsets" are like subfolders of locations, and are collections of rasters and/or vectors typically related to the same general project. Typical users will not need to make changes to the default location (called "location") or mapset (called "PERMANENT").
#'
#' @param x A Either:
#' * Missing: Reports mapset of currently active **GRASS** [session][tutorial_sessions].
#  * A `GSession` object or an object that contains the `GSession` class (i.e., a `GSpatial` object: a `GRaster` or `GVector`): Reports the CRS of the object.
#'
#' @return A character string.
#'
#' @seealso **GRASS** [locations and mapsets](https://grass.osgeo.org/grass82/manuals/grass_database.html)
#'
#' @example man/examples/ex_sessions.r
#'
#' @aliases mapset
#' @rdname mapset
#' @exportMethod mapset
methods::setMethod(
	f = "mapset",
	signature = "GSession",
	definition = function(x) unname(x@mapset)
)

#' @rdname mapset
#' @aliases mapset
#' @exportMethod mapset
methods::setMethod(
	f = "mapset",
	signature = "missing",
	definition = function(x) unname(getFastOptions("mapset"))
)

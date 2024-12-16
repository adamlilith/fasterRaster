#' @title GRASS "location" of an object or the active session
#'
#' @description **GRASS** "projects" or "locations" are sets of one or more rasters and/or vectors with the same coordinate reference systems, and may or may not represent the same actual location on Earth. **GRASS** "mapsets" are like sub-folders of locations, and are collections of rasters and/or vectors typically related to the same general project. Typical users will not need to make changes to the default location (called "location") or mapset (called "PERMANENT"). See `vignette("projects_mapsets", package = "fasterRaster")`.
#'
#' @param x Either:
#' * Missing: Reports location of currently active project/location.
#'  * A `GLocation` object or an object that contains the `GLocation` class (i.e., a `GSpatial` object: a `GRaster` or `GVector`).
#'
#' @return A character string.
#'
#' @seealso [.mapset()]
#'
#' @aliases .location
#' @rdname location
#' @keywords internal
methods::setMethod(
	f = ".location",
	signature = "GLocation",
	definition = function(x) unname(x@location)
)

#' @aliases .location
#' @rdname location
#' @keywords internal
methods::setMethod(
	f = ".location",
	signature = "missing",
	definition = function(x) .fasterRaster$activeLocation
)

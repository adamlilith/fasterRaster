#' @title GRASS "mapset" of an object or the active session
#'
#' @description **GRASS** "locations" are sets of one or more rasters and/or vectors with the same coordinate reference systems, and may or may not represent the same actual location on Earth. **GRASS** "mapsets" are like subfolders of locations, and are collections of rasters and/or vectors typically related to the same general project. This function returns the mapset of an object or the current mapset. This said, **fasterRaster** always uses the "PERMANENT" mapset, so there is very little reason to use this function as-is. See `vignette("10_projects_locations_mapsets", package = "fasterRaster")`.
#'
#' @param x A Either:
#' * Missing: Reports mapset of currently active **GRASS** "projects"/"locations".
#'  * A `GLocation` object or an object that contains the `GLocation` class (i.e., a `GSpatial` object: a `GRaster` or `GVector`): Reports the CRS of the object.
#'
#' @return A character string.
#'
#' @seealso **GRASS** [locations and mapsets](https://grass.osgeo.org/grass82/manuals/grass_database.html)
#'
#' @example man/examples/ex_location_mapset.r
#'
#' @aliases .mapset
#' @rdname mapset
#' @keywords internal
methods::setMethod(
	f = ".mapset",
	signature = "GLocation",
	definition = function(x) unname(x@mapset)
)

#' @aliases .mapset
#' @rdname mapset
#' @keywords internal
methods::setMethod(
	f = ".mapset",
	signature = "missing",
	definition = function(x) {
	
	rgrass::execGRASS("g.mapset", flags = c(.quiet(), 'p'), intern = TRUE)
	
	} # EOF
)

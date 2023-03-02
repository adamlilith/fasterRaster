#' "Location" of a 'G-' object
#'
#' **GRASS** "locations" are sets of one or more rasters and/or vectors with the same coordinate reference systems, and may or may not represent the same actual location on Earth. **GRASS** "mapsets" are like subfolders of locations, and are collections of rasters and/or vectors typically related to the same general project.  Typical users will not need to make changes to the default location (called "location") or mapset (called "PERMANENT").
#'
#' @param x A `GLocation` object or one that contains it (i.e., a `GSpatial` object, or a `GRaster` or `GVector`).
#'
#' @return A character string.
#'
#' @seealso **GRASS** [https://grass.osgeo.org/grass82/manuals/grass_database.html](locations and mapsets)
#'
#' @example man/examples/example_locationMapset.r
#'
#' @export
fastLocation.GLocation <- setGeneric(name='fastLocation', def=function(x) { standardGeneric('fastLocation') })
setMethod(
	f='fastLocation',
	signature='GLocation',
	definition=function(x) x@location
)

#' @name fastMapset
#' @title "Mapset" of a 'G-' object
#' @rdname fastLocation
#' @export
fastMapset.GLocation <- setGeneric(name='fastMapset', def=function(x) { standardGeneric('fastMapset') })
setMethod(
	f='fastMapset',
	signature='GLocation',
	definition=function(x) x@mapset
)

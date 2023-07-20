#' Convert a GVector to a SpatVector
#'
#' @description The **fasterRaster** version of the `vect()` function converts a `GVector` to a `SpatVector` (from the **terra** package).
#'
#' @param x A `GVector`.
#' @param ... Additional arguments to send to [writeRaster()] (typically unneeded, though `bigTiff` may be of use if the raster is large).
#'
#' @return A `SpatVector` (**terra** package).
#' 
#' @seealso [terra::vect()], [sf::st_as_sf()]
#' 
#' @example man/examples/ex_GVector.r
#'
#' @aliases vect
#' @rdname vect
#' @exportMethod vect
setMethod(
	'vect',
	signature(x = 'GVector'),
	function(x, ...) {

	filename <- paste0(forwardSlash(tempfile()), '.gpkg')
	out <- writeVector(x, filename=filename, ...)
	out
	} # EOF
)

#' @aliases st_as_sf
#' @rdname vect
#' @exportMethod st_as_sf
setMethod(
	'st_as_sf',
	signature(x = 'GVector'),
	function(x) {
	out <- vect(x)
	sf::st_as_sf(out)
	} # EOF
)

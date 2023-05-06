#' Convert a GVector to a SpatVector
#'
#' @description The **fasterRaster** version of the `vect()` function converts a `GVector` to a `SpatVector` (from the **terra** package).
#'
#' @param x A `GVector`.
#' @param ... Additional arguments to send to [writeRaster()] (typically unneeded, though `bigTiff` may be of use if the raster is large).
#'
#' @return A `SpatVector` (**terra** package).
#'
#' @aliases vect
#' @rdname vect
#' @export
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

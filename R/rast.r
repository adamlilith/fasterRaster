#' Convert a GRaster to a SpatRaster
#'
#' @description The **fasterRaster** version of the `rast()` function converts a `GRaster` to a `SpatRaster` (from the **terra** package). If you wish to save this raster, please consult [writeRaster4()] and [writeRaster8()].
#'
#' @param x A `GRaster`.
#' @param ... Additional arguments to send to [writeRaster()] (typically unneeded, though `bigTiff` may be of use if the raster is large).
#'
#' @return A `SpatRaster` (**terra** package).
#'
#' @aliases rast
#' @rdname rast
#' @export
#' @exportMethod rast
setMethod(
	'rast',
	signature(x = 'GRaster'),
	function(x, ...) {

	filename <- paste0(forwardSlash(tempfile()), '.tif')
	out <- writeRaster(x, filename=filename, ...)
	out
	} # EOF
)

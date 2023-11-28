#' Convert a GRaster to a SpatRaster
#'
#' @description The **fasterRaster** version of the `rast()` function converts a `GRaster` to a `SpatRaster` (from the **terra** package).
#'
#' @param x A `GRaster`.
#' @param ... Additional arguments to send to [writeRaster()] (typically unneeded, though `bigTiff` may be of use if the raster is large).
#'
#' @return A `SpatRaster` (**terra** package).
#' 
#' @seealso [terra::rast()]
#'
#' @example man/examples/ex_GRaster.r
#'
#' @aliases rast
#' @rdname rast
#' @exportMethod rast
setMethod(
	"rast",
	signature(x = "GRaster"),
	function(x, ...) {

	filename <- tempfile(fileext=".tif")
	out <- writeRaster(x, filename=filename, ...)
	out
	} # EOF
)

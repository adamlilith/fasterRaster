#' Convert a GRaster to a SpatRaster
#'
#' @description The **fasterRaster** version of the `rast()` function converts a `GRaster` to a `SpatRaster` (from the **terra** package).
#'
#' @param x A `GRaster`.
#' @param mm Logical: If `TRUE`, call [terra::setMinMax()] on the raster to ensure it has metadata on the minimum and maximum values. For large rasters, this can take a long time, so the default value of `mm` is `FALSE`.
#' @param ... Additional arguments to send to [writeRaster()]. These are typically unneeded, though `bigTiff` may be of use if the raster is large, and supplying `datatype` can speed conversion of large rasters. See [writeRaster()].
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
	function(x, mm = FALSE, ...) {

	filename <- tempfile(fileext = ".tif")
	writeRaster(x, filename = filename, format = "GeoTIFF", ...)
	out <- terra::rast(filename)
	if (mm) out <- terra::setMinMax(out)
	names(out) <- names(x)
	out

	} # EOF
)

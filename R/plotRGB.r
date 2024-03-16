#' Create red-green-blue plot from a raster with RGB layers
#'
#' @description This function takes as its main argument a `GRaster` with at least three layers typically representing red, green, and blue components (plus possibly an "alpha", or transparency layer). As with [plot()], this function is somewhat of a hack in that it downsamples the layers to a coarser resolution using [aggregate()], saves the raster to disk, then uses [terra::plotRGB()] to do the actual plotting.
#'
#' @param x A `GRaster`. Values must be in the range from 0 to 255.
#'
#' @param r,g,b Either a numeric integer or the [names()] of layers representing red, green, and blue components.
#'
#' @param a Either `NULL` (default), or a numeric integer or the [names()] of a layer representing transparency.
#'
#' @param simplify Logical: If `TRUE` (default), then downsample the `GRaster` before plotting. This can save time for very dense rasters.
#'
#' @param maxcell Positive numeric integer: Maximum number of cells to display. When simplifying, [aggregate()] will be applied so that it has approximately this number of cells. The default is 500000.
#'
#' @param ... Arguments to pass to [terra::plotRGB()].
#'
#' @returns Nothing (makes a plot).
#'
#' @seealso [terra::plotRGB()], [plot()], [compositeRGB()]
#'
#' @example man/examples/ex_plot.r
#'
#' @aliases plotRGB
#' @rdname plotRGB
#' @exportMethod plotRGB
methods::setMethod(
	f = "plotRGB",
	signature = c(x = "GRaster"),
	function(x, r = 1, g = 2, b = 3, a = NULL, simplify = TRUE, maxcell = 500000, ...) {
	
	.locationRestore(x)
	.region(x)
	
	# simplify
	nc <- ncell(x)
	if (simplify & nc > maxcell) {

		rows <- nrow(x)
		cols <- ncol(x)
		rescale <- ceiling(nc / (2 * maxcell))
		if (rescale != 1L) x <- aggregate(x, fact = rescale)

	}
	
	tf <- tempfile(fileext = ".tif")
	x <- writeRaster(x, tf, overwrite = TRUE)
	
	terra::plotRGB(x, r = r, g = g, b = b, a = a, ...)
	
	} # EOF
)

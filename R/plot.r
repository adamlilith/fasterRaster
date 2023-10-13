#' Display a raster or vector
#'
#' @description `plot()` displays a `GRaster` or `GVector`.
#'
#' This function is essentially a hack, as it it not possible to dependably call the appropriate **GRASS** modules and display a raster or vector without potential confusion on the user side. Instead, this function 1) simplifies the focal `GRaster` or `GVector` (if needed); 2) writes it to disk as a `SpatRaster` or `SpatVector`; and 3) plots the object.
#'
#' @param x A `GRaster` or `GVector`.
#'
#' @param y Missing--leave as empty.
#'
#' @param maxcell Positive integer (rasters only): Maximum number of cells to display. When simplifying, [aggregate()] will be applied so that it has approximately this number of cells. The default is 500000.
#'
#' @param simplify Logical (vectors only): If `TRUE` (default), then simplify the `GVector` before plotting. This can save time for very large vectors. However, details in the vector may appear inaccurate.
#'
#' @param maxGeoms Positive integer (vectors only): Maximum number of features before simplification is used (`simplify` must also be `TRUE`).
#' 
#' @param ... Other arguments to send to [terra::plot()].
#'
#' @returns Nothing (displays a raster or vector).
#'
#' @seealso [terra::plot()]
#'
#' @example man/examples/ex_plot.r
#'
#' @aliases plot
#' @rdname plot
#' @exportMethod plot
methods::setMethod(
	f = "plot",
	signature = c(x = "GRaster", y = "missing"),
	function(x, y, maxcell = 500000, ...) {

	# simplify
	nc <- ncell(x)
	if (nc > maxcell) {

		rows <- nrow(x)
		cols <- ncol(x)
		rescale <- ceiling(nc / (2 * maxcell))
		if (rescale != 1L) x <- aggregate(x, fact = rescale)

	}
	
	tf <- tempfile(fileext = ".tif")
	y <- writeRaster(x, tf, overwrite = TRUE)
	terra::plot(y, ...)
	
	} # EOF
)

#' @aliases plot
#' @rdname plot
#' @exportMethod plot
methods::setMethod(
	f = "plot",
	signature = c(x = "GVector", y = "missing"),
	function(x, y, simplify = FALSE, maxGeoms = 10000, ...) {
	
	# simplify
	if (simplify & nrow(x) > maxGeoms) x <- simplifyGeom(x)
	
	tf <- tempfile(fileext = ".gpkg")
	y <- writeVector(x, filename = tf, overwrite = TRUE)
	terra::plot(y, ...)
	
	} # EOF
)

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
#' @param simplify Logical: If `TRUE` (default), then simplify the `GRaster` or `GVector` before plotting. This can save time for very dense rasters or large vectors, but especially for vectors, how the vector appears may be different from how it actually is.
#'
#' @param maxcell Positive integer (rasters only): Maximum number of cells to display. When simplifying, [aggregate()] will be applied so that it has approximately this number of cells. The default is 500000.
#'
#' @param maxGeoms Positive integer (vectors only): Maximum number of features before simplification is used (`simplify` must also be `TRUE`).
#' 
#' @param ... Other arguments to send to [terra::plot()].
#'
#' @returns Nothing (displays a raster or vector).
#'
#' @seealso [terra::plot()]
#'
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @aliases plot
#' @rdname plot
#' @exportMethod plot
methods::setMethod(
	f = "plot",
	signature = c(x = "GRaster", y = "missing"),
	function(x, y, simplify = TRUE, maxcell = 500000, ...) {
	
	# simplify
	nc <- ncell(x)
	if (simplify & nc > maxcell) {
		
		rows <- nrow(x)
		cols <- ncol(x)
		
		scale <- ceiling(nc / (2 * maxcell))
		x <- aggregate(x, fact=scale)
		
	}
	
	tf <- tempfile(fileext = ".tif")
	x <- writeRaster(x, tf, overwrite=TRUE)
	terra::plot(x, ...)
	
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
	x <- writeVector(x, tf, overwrite=TRUE)
	terra::plot(x, ...)
	
	} # EOF
)

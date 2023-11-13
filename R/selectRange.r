#' Select values from rasters in a stack based on values in another raster
#'
#' @description `selectRange()` selects values from `GRaster`s in a "stack" based on the values in another "selection" raster. For example, if the stack has three layers (call them A, B, and C), the "selection" raster could have values of 1, 2, or 3 in each cell. The raster that is returned will have values from A wherever the selection raster is 1, B from where it is 2, and C from where it is 3.
#'
#' @param x A `GRaster`, typically with more than one layer.
#'
#' @param y A `GRaster` with integer values. The raster will be rounded if it does not. The values are typically between 1 and the number of layers in `x`, but wherever they are outside this range, the returned raster will have `NA` values.
#'
#' @returns A `GRaster`.
#'
#' @example man/examples/ex_selectRange.r
#'
#' @aliases selectRange
#' @rdname selectRange
#' @exportMethod selectRange
methods::setMethod(
	f = "selectRange",
	signature = c(x = "GRaster"),
	function(x, y) {
	
	.restore(x)
	region(x)

	if (nlyr(y) > 1L) {
		warning("The ", sQuote("y"), " raster has more than one layer. On the first will be used.")
		y <- y[[1L]]
	}
	if (datatype(y, "GRASS") != "CELL") y <- round(y)

	src <- .makeSourceName("selectRange", "raster")
	xGnames <- sources(x)
	yGname <- sources(y)
	nLayers <- nlyr(x)

	ex <- paste0(src, " = ")
	for (i in seq_len(nLayers)) {
		ex <- paste0(ex, " if(", yGname, " == ", i, ", ", xGnames[i], ",")
	}
	ex <- paste0(ex, " null()", paste(rep(")", nLayers), collapse = ""))

	args <- list(
		cmd = "r.mapcalc",
		expression = ex,
		flags = c(.quiet(), "overwrite"),
		intern = TRUE
	)
	do.call(rgrass::execGRASS, args = args)

	.makeGRaster(src, "selectRange")

	} # EOF
)

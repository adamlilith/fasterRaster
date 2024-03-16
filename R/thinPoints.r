#' Reduce number of points in same raster cell
#'
#' @description This function thins a "points" `GVector` so that it has no more than `n` points per grid cell in a raster.
#'
#' @param x A "points" `GVector`.
#'
#' @param y A `GRaster`.
#'
#' @param n Integer or numeric integer: Maximum number of points to remain in a cell. The default is 1.
#'
#' @returns A "points" `GVector`.
#'
#' @example man/examples/ex_thinPoints.r
#'
#' @aliases thinPoints
#' @rdname thinPoints
#' @exportMethod thinPoints
methods::setMethod(
	f = "thinPoints",
	signature = c(x = "GVector", y = "GRaster"),
	function(x, y, n = 1) {
	
	.locationRestore(x)
	compareGeom(x, y)
	.region(y)

	src <- .makeSourceName("v_decimate", "vector")
	rgrass::execGRASS(
		cmd = "v.decimate",
		input = sources(x),
		output = src,
		cell_limit = n,
		flags = c(.quiet(), "overwrite", "g", "t")
	)

	if (nrow(x) > 0L) {
	
		table <- as.data.table(x)
		xCats <- .vCats(x)
		outCats <- .vCats(src)
		keeps <- which(outCats %in% xCats)
		table <- table[keeps]
	
	} else {
		table <- NULL
	}

	.makeGVector(src, table = table)
	
	} # EOF
)
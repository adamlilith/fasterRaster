#' Create a hexagonal grid
#'
#' @description This function creates a `GVector` of "wall-to-wall" hexagons. The input can be a `GVector` or `GRaster`, which provides the extent of the output.
#'
#' @param x A `GRaster` or `GVector`.
#'
#' @param n Integer: Number of hexagons in the x-dimension.
#'
#' @returns A `GVector`.
#'
#' @example man/examples/ex_grid_gridPoints_hexagons.r
#'
#' @seealso [grid()], [gridPoints()], module `v.mkgrid` in **GRASS**
#'
#' @aliases hexagons
#' @rdname hexagons
#' @exportMethod hexagons
methods::setMethod(
	f = "hexagons",
	signature = c(x = "GRaster"),
	function(x, n = 10) .hexagons(x, n = n)
)

#' @aliases hexagons
#' @rdname hexagons
#' @exportMethod hexagons
methods::setMethod(
	f = "hexagons",
	signature = c(x = "GVector"),
	function(x, n = 10) .hexagons(x, n = n)
)

#' @noRd
.hexagons <- function(x, n) {

	.restore(x)
	region(x)

	src <- .makeSourceName("v_mkgrid", "vector")
	rgrass::execGRASS(
		cmd = "v.mkgrid",
		map = src,
		position = "region",
		grid = c(0, n), # 1st number does not matter
		flags = c("quiet", "overwrite", "h")
	)

	.makeGVector(src)

}

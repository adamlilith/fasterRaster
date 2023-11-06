#' Create a hexagonal grid
#'
#' @description This function creates a `GVector` of "wall-to-wall" hexagons. The input can be a `GVector` or `GRaster`, which provides the extent of the output.
#'
#' @param x A `GRaster` or `GVector`.
#'
#' @param n Integer: Number of hexagons in the x-dimension.
#'
#' @param angle Numeric: Degrees by which to rotate grid (from north, clockwise).
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
	function(x, n = 10, angle = 0) .hexagons(x, n = n, angle = angle)
)

#' @aliases hexagons
#' @rdname hexagons
#' @exportMethod hexagons
methods::setMethod(
	f = "hexagons",
	signature = c(x = "GVector"),
	function(x, n = 10, angle = 0) .hexagons(x, n = n, angle = angle)
)

#' @noRd
.hexagons <- function(x, n, angle) {

	.restore(x)
	region(x)

	angle <- 360 - angle

	src <- .makeSourceName("v_mkgrid", "vector")
	rgrass::execGRASS(
		cmd = "v.mkgrid",
		map = src,
		position = "region",
		angle = angle,
		grid = c(0, n), # 1st number does not matter
		flags = c("quiet", "overwrite", "h")
	)

	.makeGVector(src)

}

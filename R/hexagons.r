#' Create a hexagonal grid
#'
#' @description This function creates a `GVector` of "wall-to-wall" hexagons. The input can be a `GVector` or `GRaster`, which provides the extent of the output.
#'
#' @param x A `GRaster` or `GVector`.
#'
#' @param ny Integer or numeric integer: Number of rows of hexagons that span the extent of object `x`.
#'
#' @param expand One or two numeric values: Expand the region by this proportion in both directions (a single value) or in the x- and y-dimensions separately. Expanding the region can be helpful to ensure the entire area of interest is covered by polygons, which can otherwise leave gaps at the edges. The number of rows and columns will be increased, but the number of hexagons that span `x` will still be `ny`.
#'
#' @param angle Numeric: Degrees by which to rotate grid (from north, clockwise).
#'
#' @returns A `GVector`.
#'
#' @example man/examples/ex_grid_hexagons.r
#'
#' @seealso [grid()], module `v.mkgrid` in **GRASS**
#'
#' @aliases hexagons
#' @rdname hexagons
#' @exportMethod hexagons
methods::setMethod(
	f = "hexagons",
	signature = c(x = "GRaster"),
	function(x, ny = 10, expand = 0, angle = 0) .hexagons(x, ny = ny, expand = expand, angle = angle)
)

#' @aliases hexagons
#' @rdname hexagons
#' @exportMethod hexagons
methods::setMethod(
	f = "hexagons",
	signature = c(x = "GVector"),
	function(x, ny = 10, expand = 0, angle = 0) .hexagons(x, ny = ny, expand = expand, angle = angle)
)

#' @noRd
.hexagons <- function(x, ny, expand, angle) {

	.locationRestore(x)
	.region(x)

	extent <- ext(vector = TRUE)
	ysize <- (extent[4L] - extent[3L]) / ny

	# expand region?
	xp <- expand[1L]
	xp <- omnibus::compareFloat(xp, 0, "!=")

	if (length(expand) == 1L) {
		yp <- xp
	} else {
		yp <- expand[2L]
		yp <- omnibus::compareFloat(yp, 0, "!=")
	}

	if (xp | yp) {
	
		if (length(expand) == 1L) expand[2L] <- expand[1L]

		extent <- ext(vector = TRUE)
		xdim <- extent[2L] - extent[1L]
		ydim <- extent[4L] - extent[3L]
		
		xdelta <- 0.5 * expand[1L] * xdim
		ydelta <- 0.5 * expand[2L] * ydim

		extent <- c(
			extent[1L] - xdelta,
			extent[2L] + xdelta,
			extent[3L] - ydelta,
			extent[4L] + ydelta
		)

		.regionExt(extent, respect = "dimensions")

	}

	src <- .makeSourceName("v_mkgrid", "vector")
	args <- list(
		cmd = "v.mkgrid",
		map = src,
		position = "region",
		# grid = c(ycells, 0), # order is reversed
		box = c(ysize, ysize),
		flags = c(.quiet(), "overwrite", "h")
	)

	angle <- 360 - angle
	if (!(angle %in% c(0, 360))) args$angle <- angle

	do.call(rgrass::execGRASS, args = args)
	.makeGVector(src)

}

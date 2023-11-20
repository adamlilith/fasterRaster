#' Create a grid GVector
#'
#' @description This function creates a `GVector` of "wall-to-wall" "cells" (like a lattice). The input can be a `GVector` or `GRaster`, which provides the extent of the output.
#'
#' @param x A `GRaster` or `GVector`.
#'
#' @param nx,ny Integer or numeric:
#' * If `use` is `"number"`, then these values represent the number of rows and columns in the grid.
#'  * If `use` is `size`, then these values represent the size of the cells in the x- and y-dimensions.
# In either case, either `nx` or `ny` can be left `NULL` if the other value is defined. In this case, then the size of the cells in the opposing direction will be the same as the size in the direction that is defined. For example, if `nx` is 10 and `use` is `number`, then the size of the cells in the y-dimension will be the same as the size in the x-dimension. The extent of the output will be expanded in the eastward and southward directions to accommodate an integer number of cells in the desired direction.
#'
#' @param use Character: How to generate the grid. If this is `number` (default), then `nx` and `ny` are taken to be the number of grid cells. If `size`, then `nx` and `ny` are taken to be the size of the grid cells.
#'
#' @param angle Numeric: Degrees by which to rotate grid (from north, clockwise).
#'
#' @returns A `GVector`.
#'
#' @example man/examples/ex_grid_hexagons.r
#'
#' @seealso [hexagons()], module `v.mkgrid` in **GRASS**
#'
#' @aliases grid
#' @rdname grid
#' @exportMethod grid
methods::setMethod(
	f = "grid",
	signature = c(x = "GRaster"),
	function(x, nx = NULL, ny = NULL, use = "number", angle = 0) .grid(x, nx = nx, ny = ny, use = use, angle = angle)
)

#' @aliases grid
#' @rdname grid
#' @exportMethod grid
methods::setMethod(
	f = "grid",
	signature = c(x = "GVector"),
	function(x, nx = NULL, ny = NULL, use = "number", angle = 0) .grid(x, nx = nx, ny = ny, use = use, angle = angle)
)

#' @noRd
.grid <- function(x, nx, ny, use, angle) {

	.restore(x)
	region(x)

	use <- omnibus::pmatchSafe(use, c("number", "size"), nmax = 1L)

	# calculate cell number and re-calibrate GRASS region
	if (is.null(nx) & is.null(ny)) {
		stop("At least one of ", sQuote("nx"), " or ", sQuote("ny"), " must be defined.")
	} else if (!is.null(nx) & is.null(ny)) {

		extent <- ext(x, vector = TRUE)
		if (use == "number") {
		
			dim <- extent[2L] - extent[1L]
			xsize <- ysize <- dim / nx

		} else if (use == "size") {
			xsize <- ysize <- nx
		}
		
		ycellsFract <- (extent[4L] - extent[3L]) / ysize
		ycells <- ceiling(ycellsFract)
		delta <- 0.5 * (ycells - ycellsFract)
		newNorth <- extent[4L] + delta * ysize
		newSouth <- extent[3L] - delta * ysize

		regionExt(c(extent[1L], extent[2L], newSouth, newNorth), respect = "dimensions")

	} else if (is.null(nx) & !is.null(ny)) {

		extent <- ext(x, vector = TRUE)
		if (use == "number") {
		
			dim <- extent[4L] - extent[3L]
			xsize <- ysize <- dim / ny

		} else if (use == "size") {
			xsize <- ysize <- ny
		}

		xcellsFract <- (extent[2L] - extent[1L]) / xsize
		xcells <- ceiling(xcellsFract)
		delta <- 0.5 * (xcells - xcellsFract)
		newWest <- extent[1L] - delta * xsize
		newEast <- extent[2L] + delta * xsize
	
		regionExt(c(newWest, newEast, extent[3L], extent[4L]), respect = "dimensions")

	} else {

		if (use == "number") {

			extent <- ext(x, vector = TRUE)
			xsize <- (extent[2L] - extent[1L]) / nx
			ysize <- (extent[4L] - extent[3L]) / ny

		} else if (use == "size") {
		
			xsize <- nx
			ysize <- ny

			xcellsFract <- (extent[2L] - extent[1L]) / xsize
			ycellsFract <- (extent[4L] - extent[3L]) / ysize

			xcells <- ceiling(xcellsFract)
			ycells <- ceiling(ycellsFract)

			xdelta <- 0.5 * (xcells - xcellsFract)
			ydelta <- 0.5 * (ycells - ycellsFract)

			newWest <- extent[1L] - xdelta * xsize
			newEast <- extent[2L] + xdelta * xsize
		
			newNorth <- extent[4L] + ydelta * ysize
			newSouth <- extent[3L] - ydelta * ysize

			regionExt(c(newWest, newEast, newSouth, newNorth), respect = "dimensions")
		
		}

	}

	src <- .makeSourceName("v_mkgrid", "vector")
	args <- list(
		cmd = "v.mkgrid",
		map = src,
		position = "region",
		box = c(xsize, ysize),
		flags = c(.quiet(), "overwrite")
	)

	angle <- 360 - angle
	if (!(angle %in% c(0, 360))) args$angle <- angle

	do.call(rgrass::execGRASS, args = args)
	.makeGVector(src)

}

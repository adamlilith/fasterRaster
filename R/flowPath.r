#' Path of water flow across a landscape
#'
#' @description This function finds the least-cost pathway from a set of starting points to the lowest cells accessible from them while, in each step, traversing "down" slope gradients. It is intended to depict the path a drop of water would take when flowing across a landscape. For a single starting point, the defaults settings will produce a raster with cells with values of 1 along the path. All other cells will be set to `NA`.
#'
#' @param x A `GRaster` with a single layer, typically representing elevation.
#'
#' @param y A "points" `GVector`. The `GVector` must have <= 1024 points.
#'
#' @param return Character: Indicates the type of values "burned" into the cells of the output raster. Case is ignored and partial matching is used, but only one option can be selected.
#' * "`ID`" (default): Cells in each path are labeled with the index of the starting point. A cell in the flow path of the first point will have a value of 1, a cell in the flow path of the second point will have a value of 2, and so on.
#' * "`sequence`": The output raster's cells will start with 1 at the source point(s), then accumulate so that the next cell in the flow path is 2, the one after that 3, and so on.
#' * "`copy`": The cells in the flow path will have the elevation raster's values in the cells along the flow path(s).
#' * "`accumulation`": Cells in the flow path will accumulate the elevation raster's cell values. For example, if the starting cell has an elevation of 700 and the next cell in the drainage path has a value of 600 and the one after that 500, then the first cell in the path will have a value of 700, the next 1300 (= 700 + 600), and the third 1800 (= 700 + 600 + 500).
#' * A numeric value: All cells in flow paths will be assigned this value.
#'
#' @returns A `GRaster`.
#'
#' @example man/examples/ex_flowPath.r
#'
#' @seealso [flow()], [streams()], the **GRASS** module `r.drain` (see `grassHelp("r.drain")`)
#'
#' @aliases flowPath
#' @rdname flowPath
#' @exportMethod flowPath
methods::setMethod(
	f = "flowPath",
	signature = c(x = "GRaster"),
	function(x, y, return = "ID") {
	
	if (nlyr(x) > 1L) stop("This function can only use a single-layered GRaster as input.")
	if (geomtype(y) != "points") stop("The GVector must represent points.")
	if (ngeom(y) > 1024L) stop("The GVector can only have up to 1024 points in it.")

	if (!is.numeric(return)) {
		returns <- c("ID", "sequence", "copy", "accumulation")
		return <- omnibus::pmatchSafe(return, returns, nmax = 1L)
	}

	.locationRestore(x)
	.region(x)

	ySrc <- .copyGSpatial(y)
	ySrc <- .vRecat(ySrc, gtype = "point")

	src <- .makeSourceName("flowPath_r_drain", type = "raster")
	args <- list(
		cmd = "r.drain",
		input = sources(x),
		output = src,
		start_points = ySrc,
		flags = c(.quiet(), "overwrite")
	)

	if (return == "copy") {
		args$flags <- c(args$flags, "c")
	} else if (return == "accumulation") {
		args$flags <- c(args$flags, "a")
	} else if (return == "sequence") {
		args$flags <- c(args$flags, "n")
	}

	do.call(rgrass::execGRASS, args = args)

	# force output to a numeric value
	if (is.numeric(return)) {

		srcIn <- src
		src <- .makeSourceName("flowPath_r_mapcalc", "raster")
		ex <- paste0(src , " = ", return, " + (", srcIn, " * 0)")
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

	}

	.makeGRaster(src, names = "flowPath")

	} # EOF
)

#' Identify watershed basins and direction and accumulation of flow
#'
#' @description The `flow()` function uses a raster representing elevation to compute rasters representing:
#' * Flow accumulation raster;
#' * Direction of flow;
#' * Watershed basins;
#' * Flooded areas;
#' * Topographic convergence (log of flow accumulation divided by local slope).
#'
#' More details about the computations can be found at the help page for the `[r.terraflow](https://grass.osgeo.org/grass84/manuals/r.terraflow.html)` module for **GRASS**.
#'
#' @param x A `GRaster` with a single layer, typically representing elevation.
#'
#' @param direction Character: Either `"single"` or `"multi"`. This indicates whether a single-direction flow or multi-direction flow model is used. The default is `"multi"`. Partial matching is used and case is ignored.
#'
#' @param flowThreshold Numeric (default is `Inf`): For the multi-direction flow model, this indicates the amount of accumulated flow above which the single-direction flow rule is used to locate the egress of water from a cell.
#'
#' @param return Character vector: Indicates what rasters to return. Partial matching is used and case is ignored. Options include:
#' * `"accumulation"` (default): Flow accumulation raster.
#' * `"basins"`: Watershed basins
#' * `"direction"`: Flow direction
#' * `"flooded"`: Flooded areas
#' * `"TCI"`: Topographic convergence index
#' * `"*"`: All of the above
#'
#' @seealso [flowPath()], the `[r.terraflow](https://grass.osgeo.org/grass84/manuals/r.terraflow.html)` module for **GRASS**
#'
#' @param scratchDir Character: Directory in which to store temporary files. The **GRASS** module `r.terraflow` makes a lot of temporary files. The default is given by [tempdir()].
#'
#' @returns A `GRaster`.
#'
#' @example man/examples/ex_flow.r
#'
#' @aliases flow
#' @rdname flow
#' @exportMethod flow
methods::setMethod(
	f = "flow",
	signature = c(x = "GRaster"),
	function(
		x,
		direction = "multi",
		return = "accumulation",
		flowThreshold = Inf,
		scratchDir = tempdir()
	) {

	returns <- c("accumulation", "basins", "direction", "flooded", "TCI")
	if (any(return == "*")) return <- returns
	return <- omnibus::pmatchSafe(return, returns)
	return <- unique(return)

	direction <- omnibus::pmatchSafe(direction, c("single", "multi"), nmax = 1L)

	if (nlyr(x) > 1L) stop("This function can only use a single-layered GRaster as input.")

	.locationRestore(x)
	.region(x)

	args <- list(
		cmd = "r.terraflow",
		elevation = sources(x),
		memory = faster("memory"),
		directory = scratchDir,
		flags = c(.quiet(), "overwrite")
	)

	if (direction == "single") args$flags <- c(args$flags, "s")
	if (direction == "multi" & !is.infinite(flowThreshold)) args$d8cut <- flowThreshold

	if (any(return == "accumulation")) args$accumulation <- .makeSourceName("r_flow_accumulation", "raster")
	if (any(return == "direction")) args$direction <- .makeSourceName("r_flow_direction", "raster")
	if (any(return == "basins")) args$swatershed <- .makeSourceName("r_flow_swatershed", "raster")
	if (any(return == "flooded")) args$filled <- .makeSourceName("r_flow_filled", "raster")
	if (any(return == "TCI")) args$tci <- .makeSourceName("r_flow_tci", "raster")

	do.call(rgrass::execGRASS, args = args)

	names <- srcs <- character()
	if (any(return == "accumulation")) {
		srcs <- args$accumulation
		names <- "accumulation"
	}
	if (any(return == "direction")) {
		srcs <- c(srcs, args$direction)
		names <- c(names, "direction")
	}
	if (any(return == "basins")) {
		srcs <- c(srcs, args$swatershed)
		names <- c(names, "basins")
	}
	if (any(return == "flooded")) {
		srcs <- c(srcs, args$filled)
		names <- c(names, "flooded")
	}
	if (any(return == "TCI")) {
		srcs <- c(srcs, args$tci)
		names <- c(names, "TCI")
	}
	
	order <- match(return, names)
	srcs <- srcs[order]
	names <- names[order]

	.makeGRaster(srcs, names)

	} # EOF
)

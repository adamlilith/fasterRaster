#' Horizon height
#'
#' @description `horizonHeight()` uses a raster representing elevation to calculate the height of the horizon in a particular direction from each cell on a raster. Height is expressed in radians or degrees from the horizontal.
#'
#' @param x A `GRaster`.
#'
#' @param units Character: Units of the height. Either `radians` (default) or `degrees`. Partial matching is used.
#'
#' @param step Numeric integer between 0 and 360, inclusive: Angle step size (in degrees) for calculating horizon height. The direction in which horizon height is calculated is incremented from 0 to 360, with the last value excluded.
#'
#' @param northIs0 Logical: If `TRUE` (default), horizon height calculated in the 0-degree direction will be facing north,  and proceed clockwise So, under "north orientation", 0 is north, 90 east, 180 south, and 270 west. If `FALSE`, angles are in "east orientation", and proceed counterclockwise from east. So, east is 0, north 90, west 180, and south 270. North orientation is the default for this function in **R**, but east orientation is the default in the **GRASS** module `r.horizon`. **Note:** The [sun()] function requires aspect to be in east orientation.
#'
#' @param bufferZone Numeric >= 0 (default is 0): A buffer of the specified width will be generated around the raster before calculation of horizon angle. If the coordinate system is in longitude/latitude (e.g., WGS84 or NAD83), then this is specified in degrees. Otherwise units are map units (usually meters).
#'
#' @param maxDist Either `NULL` (default) or numeric >= 0: Maximum distance to consider when finding horizon height in meters. If `NULL`, the maximum distance is the full extent of the raster. Smaller values can decrease run time but also reduce accuracy.
#'
#' @param distance Numeric between 0.5 and 1.5, inclusive (default is 1): This determines the step size when searching for the horizon from a given point. The default value of 1 goes cell-by-cell (i.e., search distance step size is one cell width).
#'
#' @returns A `GRaster` with one or more layers. The layers will be named `height_`*xyz*, where *xyz* is degrees from north or from east, depending on whether north or east orientation is used.
#'
#' @seealso The **GRASS** module `r.horizon`.
#'
#' @example man/examples/ex_horizonHeight.r
#' 
#' @aliases horizonHeight
#' @rdname horizonHeight
#' @exportMethod horizonHeight
methods::setMethod(
	f = "horizonHeight",
	signature = c(x = "GRaster"),
	definition = function(
		x,
		units = "radians",
		step = 90,
		northIs0 = TRUE,
		bufferZone = 0,
		distance = 1,
		maxDist = NULL
	) {
	
	units <- omnibus::pmatchSafe(units, c("radians", "degrees"))
	
	if (any(!omnibus::is.wholeNumber(step))) {
		step <- round(step)
		warning("Non-integer value used for argument `step`.\n  Value will be rounded (GRASS will do this anyway).")
	}
	
	if (step < 0 | step >= 360) stop("Argument `step` must be in the range [0, 359].")
	
	src <- .makeSourceName("horizonHeight", "raster")
	args <- list(
		cmd = "r.horizon",
		elevation = sources(x),
		output = src,
		step = step,
		start = 0,
		end = 360,
		bufferzone = bufferZone,
		distance = distance,
		flags = c(.quiet(), "overwrite")
	)
	
	# The "c" flag does not seem to work as described in `r.horizon` when using `start`, `end`, and `step` arguments in the call to `r.horizon`. So, we'll calculate horizon height assuming east orientation and if needed, sort the rasters so they match north orientation.
	# if (northIs0) args$flags <- c(args$flags, "c")
	
	if (units == "degrees") args$flags <- c(args$flags, "d")
	if (!is.null(maxDist)) args$maxdistance <- maxDist

	.locationRestore(x)
	.region(x)

	do.call(rgrass::execGRASS, args = args)

	directions <- seq(0, 359.9999999, by = step)
	if (northIs0) directions <- reorient(directions)

	niceDirections <- sprintf("%03.0f", directions)
	srcs <- paste0(src, "_", niceDirections)
	
	if (northIs0) niceDirections <- sort(niceDirections)

	.makeGRaster(srcs, names = paste0("horizonHeight_", niceDirections, "deg"))

	} # EOF
)

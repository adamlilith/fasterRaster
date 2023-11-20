#' Horizon height
#'
#' @description `horizonHeight()` uses a raster representing elevation to calculate the height of the horizon in a particular direction from each cell on a raster. Height is expressed in radians or degrees from the horizontal.
#'
#' @param x A `GRaster`.
#'
#' @param units Character: Units of the height. Either `radians` (default) or `degrees`. Partial matching is used.
#'
#' @param directions Numeric vector with positive integers between 0 and 359, inclusive: Direction(s) in which to calculate horizon height for each cell. By default, these are given in degrees clockwise from 0, so 0 is north, 90 east, 180 south, and 270 west. However, if you set `northIs0 = FALSE`, then the directions are given degrees counterclockwise from east, so east is 0, north 90, west 180, and south 270. Regardless, the default is to calculate horizon angle in all four directions. One raster is created per direction. Note that the output will be labeled according to the angle of the directions (e.g., `horizonHeight_090` will be horizon height facing east if `northIs0 = TRUE` (default), but horizon height facing north if `northIs0 = FALSE`. Note that **GRASS** automatically rounds these values down to the nearest integer, so this function does the same but also produces a warning.
#'
#' @param northIs0 Logical: If `TRUE` (default), argument `directions` specifies horizon height in "north orientation," or clockwise from 0, so 0 is north, 90 east, 180 south, and 270 west. If `FALSE`, angles are in "east orientation", or counterclockwise from east, so east is 0, north 90, west 180, and south 270. North orientation is the default for this function in **R**, but east orientation is the default in the **GRASS** module `r.horizon`. **Note:** The [sun()] function requires aspect to be in east orientation.
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
		directions = c(0, 90, 180, 270),
		northIs0 = TRUE,
		bufferZone = 0,
		distance = 1,
		maxDist = NULL
	) {
	
	units <- omnibus::pmatchSafe(units, c("radians", "degrees"))
	if (any(directions %% 1 != 0)) {
		directions <- floor(directions)
		warning("Non-integer value(s) used for argument ", sQuote("directions"), ".\n  Value(s) rounded down (GRASS will do this anyway).")
	}
	
	if (any(directions < 0 | directions >= 360)) stop("Values in argument ", sQuote("directions"), " must be in the range [0, 359].")
	
	flags <- c(.quiet(), "overwrite")
	if (northIs0) flags <- c(flags, "c")
	if (units == "degrees") flags <- c(flags, "d")
	
	args <- list(
		cmd = "r.horizon",
		elevation = sources(x),
		output = NA_character_,
		direction = NA_real_,
		bufferzone = bufferZone,
		distance = distance,
		flags = flags
	)
	
	if (!is.null(maxDist)) args$maxdistance <- maxDist
	
	.restore(x)
	region(x)
	
	src <- .makeSourceName("horizon", "rast")
	for (i in seq_along(directions)) {
	
		direction <- directions[i]
		niceDirection <- sprintf("%03.0f", direction)
		args$direction <- direction
		args$output <- src
	
		do.call(rgrass::execGRASS, args=args)
		this <- .makeGRaster(paste0(src, "_", niceDirection), paste0("horizonHeight_", niceDirection, "deg"))
	
		if (i == 1L) {
			out <- this
		} else {
			out <- c(out, this)
		}
		
	}
	out

	} # EOF
)

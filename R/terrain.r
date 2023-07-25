#' Slope, aspect, curvature, and partial slopes
#'
#' `terrain()` calculates topographic indices, including slope, aspect, curvature, and partial slopes (slopes in the east-west or north-south directions).
#'
#' @param x A `GRaster` (typically representing elevation).
#' @param v Name of the topographic metric(s) to calculate. Valid values include one or more of:
#'
#' * `"slope"`: Slope. Units are given by argument `units`.
#' * `"aspect"`: Aspect. When argument `northIs0` is `TRUE` (default), then aspect is given in degrees from north going clockwise (0 = north, 90 = east, 180 = south, 270 = west).
#' * `"profileCurve"`: Profile curvature.
#' * `"tanCurve"`: Tangential curvature.
#' * `"dx"`: Slope in east-west direction.
#' * `"dy"`: Slope in north-south direction.
#' * `"dxx"`: Second partial derivative in east-west direction.
#' * `"dyy"`: Second partial derivative in north-south direction.
#' * `"dxy"`: Second partial derivative along east-west and north-south direction.
#' * `"*"`: All of the above.
#'
#' @param units Character: "Units" in which to calculate slope: either `"degrees"` for degrees (default) or `"percent"`.
#'
#' @param northIs0 Logical: If `TRUE` (default), aspect will be reported in "north orientation," such that 0 is north, and degrees run clockwise (90 is east, 180 south, 270 west). If `FALSE`, then aspect will be reported in "east orientation," such that 0 is east, and degrees run counterclockwise (90 is north, 180 west, 270 south). The latter is the default in **GRASS**, but the former is the default in [terra::terrain()] function, so is used here as the default. **Note:** The [sun()] function requires aspect to be in east orientation.
#'
#' @returns A `GRaster` with one or more layers.
#' 
#' @seealso [terra::terrain()], module `r.slope.aspect` in **GRASS**
#'
#' @example man/examples/ex_terrain.r
#'
#' @aliases terrain
#' @rdname terrain
#' @exportMethod terrain
methods::setMethod(
	f = "terrain",
	signature = c(x = "GRaster"),
	definition = function(
		x,
		v = "slope",
		units = "degrees",
		northIs0 = TRUE
	) {

	metrics <- c("slope", "aspect", "profileCurve", "tanCurve", "dx", "dy", "dxx", "dyy", "dxy")
	if (length(v) == 1L && v == "*") v <- metrics
	v <- pmatchSafe(v, metrics)
	
	.restore(x)
	region(x)
	
	args <- list(
		cmd = "r.slope.aspect",
		elevation = .gnames(x),
		nprocs = getFastOptions("cores"),
		memory = getFastOptions("memory"),
		flags = c("quiet", "overwrite", "e")
	)
	
	if ("slope" %in% v) {
		args$slope <- .makeGName("slope", "rast")
		units <- pmatchSafe(units, c("degrees", "percent"))
		args$format <- units
	}

	if ("aspect" %in% v) {
		args$aspect <- .makeGName("aspect", "rast")
		units <- pmatchSafe(units, c("degrees", "percent"))
		args$format <- units
		if (northIs0) args$flags <- c(args$flags, "n")
	}

	if ("profileCurve" %in% v) args$pcurvature = .makeGName("profileCurve", "rast")
	if ("tanCurve" %in% v) args$tcurvature = .makeGName("tanCurve", "rast")
	if ("dx" %in% v) args$dx = .makeGName("dx", "rast")
	if ("dy" %in% v) args$dy = .makeGName("dy", "rast")
	if ("dxx" %in% v) args$dxx = .makeGName("dxx", "rast")
	if ("dyy" %in% v) args$dyy = .makeGName("dyy", "rast")
	if ("dxy" %in% v) args$dxy = .makeGName("dxy", "rast")
	
	do.call(rgrass::execGRASS, args)
	
	if ("slope" %in% v) {
		out <- .makeGRaster(args$slope, "slope")
	}
	
	if ("aspect" %in% v) {
		this <- .makeGRaster(args$aspect, "aspect")
		if (exists("out", inherits=FALSE)) {
			out <- c(out, this)
		} else {
			out <- this
		}
	}

	if ("profileCurve" %in% v) {
		this <- .makeGRaster(args$pcurvature, "profileCurve")
		if (exists("out", inherits=FALSE)) {
			out <- c(out, this)
		} else {
			out <- this
		}
	}

	if ("tanCurve" %in% v) {
		this <- .makeGRaster(args$tcurvature, "tanCurve")
		if (exists("out", inherits=FALSE)) {
			out <- c(out, this)
		} else {
			out <- this
		}
	}

	if ("dx" %in% v) {
		this <- .makeGRaster(args$dx, "dx")
		if (exists("out", inherits=FALSE)) {
			out <- c(out, this)
		} else {
			out <- this
		}
	}

	if ("dy" %in% v) {
		this <- .makeGRaster(args$dy, "dy")
		if (exists("out", inherits=FALSE)) {
			out <- c(out, this)
		} else {
			out <- this
		}
	}

	if ("dxx" %in% v) {
		this <- .makeGRaster(args$dxx, "dxx")
		if (exists("out", inherits=FALSE)) {
			out <- c(out, this)
		} else {
			out <- this
		}
	}

	if ("dyy" %in% v) {
		this <- .makeGRaster(args$dyy, "dyy")
		if (exists("out", inherits=FALSE)) {
			out <- c(out, this)
		} else {
			out <- this
		}
	}

	if ("dxy" %in% v) {
		this <- .makeGRaster(args$dxy, "dxy")
		if (exists("out", inherits=FALSE)) {
			out <- c(out, this)
		} else {
			out <- this
		}
	}
	
	out

	} # EOF
)



#' Simplify the geometry of a vector
#'
#' @description `simplifyGeom()` reduces the number of vertices used to represent a vector (i.e., to save memory or disk space). There are several methods available.
#'
#' @param x A `GVector`.
#'
#' @param method Character: Method used to reduce the number of vertices. Partial matching is used, and case does not matter:
#'
#' * `"VR"`: Vertex reduction (default, simplest): If two points p1 and p2 on the same line are closer than the threshold, remove p2. The `tolerance` argument represents this threshold distance.
#' * `"DP"`: Douglas-Peucker (AKA Ramer-Douglas-Peucker) algorithm: Simply stated, for points p1, p2, and p3 on a line, this method constructs a line segment between p1 and p3. If p2 is closer than the threshold to the line segment, it is removed. In this example, the `tolerance` argument refers to the maximum distance between p2 and the line segment.
#' * `"DPR"`: Douglas-Peucker algorithm with reduction: As the Douglas-Pueker method, but each geometry is thinned so that in the end it has only a given proportion of the starting number of points. The `prop` argument refers to this proportion of remaining points.
#' * `"RW`: Reumann-Witkam algorithm: For points p1, p2, p3, and p4 on a line, constructs two line segments parallel to the line segment defined by p1 and p4. These are placed `tolerance` distance one either side of the p1-p4 line segment. If the line segment p1-p2 or p3-p4 falls entirely within the bounds of the two outer parallel segments, p2 and p3 are removed, leaving just p1 and p4.
#'
#' @param tolerance Numeric >= 0: Threshold distance in map units (degrees for unprojected, usually meters for projected). If `NULL`, then 2% of the minimum of the x-, y-, and z-extent will be used.
#'
#' @param prop Positive value between 0 and 1: Proportion of points that will be retained for each geometry when the Douglas-Peucker algorithm with reduction is applied (ignored otherwise). Default is 0.5 (retain 50% of vertices).
#'
#' @seealso [smoothGeom()], [geometry cleaning][breakPolys], [terra::simplifyGeom()]
#'
#' @returns A `GVector`.
#'
#' @example man/examples/ex_simplify_smooth_clean_GVector.r
#'
#' @aliases simplifyGeom
#' @rdname simplifyGeom
#' @exportMethod simplifyGeom
methods::setMethod(
	f = "simplifyGeom",
	signature = c(x = "GVector"),
	function(x, tolerance = NULL, method = "VR", prop = 0.5) {

	# automatic distance
	if (is.null(tolerance)) {
		extent <- ext(x, vector=TRUE)
		xext <- extent[2L] - extent[1L]
		yext <- extent[4L] - extent[3L]
		zext <- diff(zext)
		tolerance <- 0.02 * min(xext, yext, zext, na.rm=TRUE)
	} else {
		if (tolerance < 0) stop("Argument ", sQuote("tolerance"), " must be > 0.")
	}


	.locationRestore(x)
	
	method <- tolower(method)
	method <- omnibus::pmatchSafe(method, c("vr", "dp", "dpr", "rw"), useFirst = TRUE)
	method <- if (method == "vr") {
		"reduction"
	} else if (method == "dp") {
		"douglas"
	} else if (method == "dpr") {
		"douglas_reduction"
	} else if (method == "rw") {
		"reumann"
	}
	
	if (method == "reumann" && (prop < 0 | prop > 1)) stop("Argument ", sQuote("prop"), " must be in the range [0, 1].")
	
	src <- .makeSourceName("generalized", "vect")
	args <- list(
		cmd = "v.generalize",
		input = sources(x),
		output = src,
		method = method,
		threshold = tolerance,
		flags = c(.quiet(), "overwrite"),
		intern = TRUE
	)

	if (method == "reumann") args$reduction <- 100 * prop
	
	do.call(rgrass::execGRASS, args=args)
	.makeGVector(src)

	} # EOF
)

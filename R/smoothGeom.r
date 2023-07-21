#" Smooth the geometry of a vector
#"
#" @description `smoothGeom()` makes line segments of a vector appear less angular.
#"
#" @param x A `GVector`.
#"
#" @param method Character: Method used to smooth line segments. Partial matching is used, and case does not matter:
#"
#" * `"Hermite"`: Hermite interpolation (default): Guarantees that the output vector always passes through the original points. This method adds points (possibly many) by constructing cubic splines with points approximately `dist` apart. The number of points can be reduced by specifying a smaller value of `angle`, which specifies the minimum angle between two successive line segments.
#" * `"Chaiken"`: Chaiken"s algorithm: Guarantees that the new vector always touches the midpoint of each original line segment. The points on the new line are at least `dist` apart.
#"
#" @param dist Numeric > 0 of `NULL` (default): Minimum distance (see `method`). Units are in map units. If `NULL`, then 2% of the minimum of the x-, y-, and z-extent will be used.
#"
#" @param angle Numeric > 0: Maximum angle for the Hermite algorithm. Default is 3.
#"
#" @seealso [simplifyGeom()], [terra::simplifyGeom()], [cleanGeom()]
#"
#" @returns A `GVector`.
#"
#" @example man/examples/ex_simplify_smooth_clean_GVector.r
#"
#" @aliases smoothGeom
#" @rdname smoothGeom
#" @exportMethod smoothGeom
methods::setMethod(
	f = "smoothGeom",
	signature = c(x = "GVector"),
	function(x, method = "Hermite", dist = NULL, angle = 3) {

	# automatic distance
	if (is.null(dist)) {
		extent <- ext(x, vector=TRUE)
		xext <- extent[2L] - extent[1L]
		yext <- extent[4L] - extent[3L]
		zext <- diff(zext)
		dist <- 0.02 * min(xext, yext, zext, na.rm=TRUE)
	} else {
		if (dist < 0) stop("Argument ", sQuote("dist"), " must be > 0.")
	}

	.restore(x)
	
	method <- tolower(method)
	method <- pmatchSafe(method, c("hermite", "chaiken"))
	
	if (method == "hermite" && angle < 0) stop("Argument ", sQuote("angle"), " must be >0.")
	
	gn <- .makeGName("generalized", "vect")
	args <- list(
		cmd = "v.generalize",
		input = .gnames(x),
		output = gn,
		method = method,
		threshold = dist,
		flags = c("quiet", "overwrite"),
		intern = TRUE
	)

	if (method == "hermite") args$angle_thresh <- angle
	
	do.call(rgrass::execGRASS, args=args)
	.makeGVector(gn)

	} # EOF
)

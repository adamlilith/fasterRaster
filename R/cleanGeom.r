#' Fix undesirable geometries of a vector
#'
#' @description `cleanGeom()` fixes geometries of a vector. These can include, for example, "dangling" lines, removing topologically incorrect features, or lines of zero length.
#'
#' @param x A `GVector`.
#'
#' @param method Character: Method used to clean line segments. Partial matching is used, and case does not matter:
#'
#' * `duplicated`: Remove duplicated features and area centroids. 
#' * `break`: Break lines at intersections and lines that form closed loops (e.g., 0, 1, 0).
#' * `removeDangles`: Remove "dangling" lines if shorter than `tolerance` distance. If `tolerance` is <0, all dangles will be removed. Units of `tolerance` are in map units, or in degrees for unprojected CRSs. If `tolerance` <0, all dangles are removed, and the function will retain only closed loops and lines connecting loops. Dangles will be removed from longest to shortest.
#' * `changeDangles`: Change "dangles" hanging off boundaries to lines if shorter than `tolerance` distance. If `tolerance` is <0, all dangles will be changed to lines.  Units of `tolerance` are in map units, or in degrees for unprojected CRSs. If `tolerance` <0, all dangles are removed, and the function will retain only closed loops and lines connecting loops. Dangles will be removed from longest to shortest.
#' * `removeBridges`: Remove "bridges" to "islands" (which are topologically incorrect) within geometries. Argument `tolerance` is ignored.
#' * `changeBridges`: Change "bridges" to "islands" (which are topologically incorrect) within geometries to lines. Argument `tolerance` is ignored.
#' * `snap`: Snap lines to vertex if they are less than `tolerance` apart. Subsequent removal of dangles may be needed. Units of `tolerance` are map units, or degrees for unprojected CRSs.
#' * `dupAreaCentroids`: Remove duplicated area centroids. In **GRASS**, closed polygons have their attributes mapped to a (hidden) centroid of the polygon. The `tolerance` argument is ignored.
#' * `topoClean`: Break topologically clean areas. This is similar to `break`, except that it does not break loops. Topologically clean vectors may occur if the vector was imported from a format that does not enforce topology, such as a shapefile. Duplicate geometries are automatically removed after breaking. Argument `tolerance` is ignored.
#' * `smallAreas`: Remove polygons smaller than `minArea`. Units of `tolerance` are in square meters (regardless of the CRS).
#' * `remove0`: Remove all boundaries and lines with a length of 0. Argument `tolerance` is ignored.
#' * `smallAngles`: Collapse lines that diverge at an angle that is computationally equivalent to 0. This tool often needs to be followed with the `break` and `duplicated` methods.
#'
#' @param tolerance Numeric or `NULL` (default): Minimum distance in map units (degrees for unprojected, usually meters for projected) or minimum area (for `smallAreas` in meters-squared, regardless of projection). If `NULL`, then 2% of the minimum of the x-, y-, and z-extent will be used, or this same value but assumed to be in meters-squared (for `smallAreas`).
#'
#' @param minArea Numeric >= 0: Minimum area of a polygon to be retained when using the `smallAreas` tool.
#'
#' @seealso [simplifyGeom()], [terra::simplifyGeom()], [smoothGeom()]
#'
#' @returns A `GVector`.
#'
#' @example man/examples/ex_simplify_smooth_clean_GVector.r
#'
#' @aliases cleanGeom
#' @rdname cleanGeom
#' @exportMethod cleanGeom
methods::setMethod(
	f = "cleanGeom",
	signature = c(x = "GVector"),
	function(x, method = "duplicated", tolerance = NULL, minArea = 0) {

	# automatic distance
	if (is.null(tolerance) & !(method %in% c("duplicated"))) {
		extent <- ext(x, vector = TRUE)
		xext <- extent[2L] - extent[1L]
		yext <- extent[4L] - extent[3L]
  		zext <- diff(zext(x))
		tolerance <- 0.01 * min(xext, yext, zext, na.rm=TRUE)
	} else {
		tolerance <- 0
	}

	.locationRestore(x)
	
	methods <- c("duplicated", "break", "removeDangles", "changeDangles", "removeBridges", "changeBridges", "snap", "dupAreaCentroids", "topoClean", "smallAreas", "remove0", "smallAngles")
	method <- omnibus::pmatchSafe(method, methods)
	
	method <- if (method == "duplicated") {
		"rmdupl"
	} else if (method == "break") {
		"break"
	} else if (method == "removeDangles") {
		"rmdangle"
	} else if (method == "changeDangles") {
		"chdangle"
	} else if (method == "removeBridges") {
		"rmbridge"
	} else if (method == "changeBridges") {
		"chbridge"
	} else if (method == "snap") {
		"snap"
	} else if (method == "dupAreaCentroids") {
		"rmdac"
	} else if (method == "topoClean") {
		"bpol"
	} else if (method == "smallAreas") {
		"rmarea"
	} else if (method == "remove0") {
		"rmline"
	} else if (method == "smallAngles") {
		"rmsa"
	}
	
	src <- .makeSourceName("v_clean", "vector")
	rgrass::execGRASS(
		cmd = "v.clean",
		input = sources(x),
		output = src,
		tool = method,
		threshold = tolerance,
		flags = c("c", .quiet(), "overwrite")
	)
	.makeGVector(src)

	} # EOF
)

#' Fix issues with geometries of a vector
#'
#' @description These functions are intended to help fix geometric issues with a `GVector`. Note that the functionality of the `snap()` and `removeAreas()` functions can also be implemented when using [fast()] to create a `GVector`.
#'
#' * `breakPolys()`: Break topologically clean areas. This is similar to `fixLines()`, except that it does not break loops. Topologically clean vectors may occur if the vector was imported from a format that does not enforce topology, such as a shapefile. Duplicate geometries are automatically removed after breaking.
#' * `fixBridges()`: Change "bridges" to "islands" (which are topologically incorrect) within geometries to lines.
#' * `fixDangles()`: Change "dangles" hanging off boundaries to lines if shorter than `tolerance` distance. If `tolerance` is <0, all dangles will be changed to lines.  Units of `tolerance` are in map units, or in degrees for unprojected CRSs. If `tolerance` <0, all dangles are removed, and the function will retain only closed loops and lines connecting loops. Dangles will be removed from longest to shortest.
#' * `fixLines()`: Break lines at intersections and lines that form closed loops.
#' * `remove0()`: Remove all boundaries and lines with a length of 0.
#' * `removeAngles()`: Collapse lines that diverge at an angle that is computationally equivalent to 0. This tool often needs to be followed with the `break()` and `removeDups()` methods.
#' * `removeBridges()`: Remove "bridges" to "islands" (which are topologically incorrect) within geometries.
#' * `removeDangles()`: Remove "dangling" lines if shorter than `tolerance` distance. If `tolerance` is <0, all dangles will be removed. Units of `tolerance` are in map units, or in degrees for unprojected CRSs. If `tolerance` <0, all dangles are removed, and the function will retain only closed loops and lines connecting loops. Dangles will be removed from longest to shortest.
#' * `removeDupCentroids()`: Remove duplicated area centroids. In **GRASS**, closed polygons have their attributes mapped to a (hidden) centroid of the polygon.
#' * `removeDups()`: Remove duplicated features and area centroids. 
#' * `removeSmallPolys()`: Remove polygons smaller than `tolerance`. Units of `tolerance` are in square meters (regardless of the CRS).
#' * `snap()`: Snap lines/boundaries to each other if they are less than `tolerance` apart. Subsequent removal of dangles may be needed. Units of `tolerance` are map units, or degrees for unprojected CRSs.
#'
#' @param x A `GVector`.
#'
#' @param tolerance Numeric or `NULL` (default): Minimum distance in map units (degrees for unprojected, usually meters for projected) or minimum area (in meters-squared, regardless of projection).
#'
#' @seealso [terra::topology()], [fillHoles()], [terra::removeDupNodes()], *Details* section in [fast()], [simplifyGeom()], [smoothGeom()]
#'
#' @returns A `GVector`.
#'
#' @example man/examples/ex_simplify_smooth_clean_GVector.r
#'
#' @aliases breakPolys
#' @rdname breakPolys
#' @exportMethod breakPolys
methods::setMethod(
	f = "breakPolys",
	signature = c(x = "GVector"),
	function(x) {
	
	if (!is.polygons(x)) stop("This tool can only be applied to polygon GVectors.")
	.cleanGeom(x, method = "bpol", tolerance = NULL, topo = topology(x))

	} # EOF
)

#' @aliases fixBridges
#' @rdname breakPolys
#' @exportMethod fixBridges
methods::setMethod(
	f = "fixBridges",
	signature = c(x = "GVector"),
	function(x) .cleanGeom(x, method = "chbridge", tolerance = NULL, topo = topology(x))
)

#' @aliases fixDangles
#' @rdname breakPolys
#' @exportMethod fixDangles
methods::setMethod(
	f = "fixDangles",
	signature = c(x = "GVector"),
	function(x, tolerance = -1) .cleanGeom(x, method = "chdangle", tolerance = tolerance, topo = topology(x))
)

#' @aliases fixLines
#' @rdname breakPolys
#' @exportMethod fixLines
methods::setMethod(
	f = "fixLines",
	signature = c(x = "GVector"),
	function(x) {
	
	if (is.points(x)) stop("This tool can only be applied to line or polygon GVectors.")
	.cleanGeom(x, method = "break", tolerance = NULL, topo = topology(x))

	} #EOF
)

#' @aliases remove0
#' @rdname breakPolys
#' @exportMethod remove0
methods::setMethod(
	f = "remove0",
	signature = c(x = "GVector"),
	function(x) {
	
	if (is.points(x)) stop("This tool can only be applied to line or polygon GVectors.")
	.cleanGeom(x, method = "rmline", tolerance = NULL, topo = topology(x))

	} # EOF
)

#' @aliases removeAngles
#' @rdname breakPolys
#' @exportMethod removeAngles
methods::setMethod(
	f = "removeAngles",
	signature = c(x = "GVector"),
	function(x) .cleanGeom(x, method = "rmsa", tolerance = NULL, topo = topology(x))
)

#' @aliases removeBridges
#' @rdname breakPolys
#' @exportMethod removeBridges
methods::setMethod(
	f = "removeBridges",
	signature = c(x = "GVector"),
	function(x) .cleanGeom(x, method = "rmbridge", tolerance = NULL, topo = topology(x))
)

#' @aliases removeDangles
#' @rdname breakPolys
#' @exportMethod removeDangles
methods::setMethod(
	f = "removeDangles",
	signature = c(x = "GVector"),
	function(x, tolerance = -1) .cleanGeom(x, method = "rmdangle", tolerance = tolerance, topo = topology(x))
)

#' @aliases removeDupCentroids
#' @rdname breakPolys
#' @exportMethod removeDupCentroids
methods::setMethod(
	f = "removeDupCentroids",
	signature = c(x = "GVector"),
	function(x) {
	
	if (!is.polygons(x)) stop("This tool can only be applied to polygon GVectors.")
	.cleanGeom(x, method = "rmdac", tolerance = NULL, topo = topology(x))
	
	} # EOF
)

#' @aliases removeDups
#' @rdname breakPolys
#' @exportMethod removeDups
methods::setMethod(
	f = "removeDups",
	signature = c(x = "GVector"),
	function(x) .cleanGeom(x, method = "rmdupl", tolerance = NULL, topo = topology(x))
)

#' @aliases removeSmallPolys
#' @rdname breakPolys
#' @exportMethod removeSmallPolys
methods::setMethod(
	f = "removeSmallPolys",
	signature = c(x = "GVector"),
	function(x , tolerance) {
	
	if (!is.polygons(x)) stop("This tool can only be applied to polygon GVectors.")
	.cleanGeom(x, method = "rmarea", tolerance = tolerance, topo = topology(x))

	} # EOF
)

#' @aliases snap
#' @rdname breakPolys
#' @exportMethod snap
methods::setMethod(
	f = "snap",
	signature = c(x = "GVector"),
	function(x , tolerance) .cleanGeom(x, method = "snap", tolerance = tolerance, topo = topology(x))
)

#' Implement vector cleaning using `sources()` name of a vector 
#'
#' @param x `GVector` or [sources()] name of a `GVector`.
#' @param method Character.
#' @param tolerance `NULL` or numeric.
#' @param topo "2D" or "3D"
#'
#' @noRd
.cleanGeom <- function(x, method, tolerance, topo) {

	if (inherits(x, "GVector")) {
		.locationRestore(x)
		src <- sources(x)
		table <- x@table
	} else {
		src <- x
		table <- NULL
	}

	src <- .copyGSpatial(src, type = "vector", topo = topo, reshapeRegion = FALSE)
	
	src <- .makeSourceName("v_clean", "vector")
	args <- list(
		cmd = "v.clean",
		input = sources(x),
		output = src,
		tool = method,
		flags = c("c", .quiet(), "overwrite")
	)

	if (!is.null(tolerance)) args$threshold <- tolerance
	do.call(rgrass::execGRASS, args = args)

	out <- .makeGVector(src)
	if (!is.null(table)) {
	
		if (ngeom(out) == nrow(table)) out@table <- table
		methods::validObject(out)
	
	}
	out

}

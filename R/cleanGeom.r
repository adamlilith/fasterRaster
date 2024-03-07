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
#' @seealso [terra::topology()], [fillHoles()], [simplifyGeom()], [smoothGeom()]
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
	.cleanGeom(src = sources(x), method = "bpol", tolerance = NULL)

	} # EOF
)

#' @aliases fixBridges
#' @rdname breakPolys
#' @exportMethod fixBridges
methods::setMethod(
	f = "fixBridges",
	signature = c(x = "GVector"),
	function(x) .cleanGeom(src = sources(x), method = "chbridge", tolerance = NULL)
)

#' @aliases fixDangles
#' @rdname breakPolys
#' @exportMethod fixDangles
methods::setMethod(
	f = "fixDangles",
	signature = c(x = "GVector"),
	function(x, tolerance = -1) .cleanGeom(src = sources(x), method = "chdangle", tolerance = tolerance)
)

#' @aliases fixLines
#' @rdname breakPolys
#' @exportMethod fixLines
methods::setMethod(
	f = "fixLines",
	signature = c(x = "GVector"),
	function(x) {
	
	if (is.points(x)) stop("This tool can only be applied to line or polygon GVectors.")
	.cleanGeom(src = sources(x), method = "break", tolerance = NULL)

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
	.cleanGeom(src = sources(x), method = "rmline", tolerance = NULL)

	} # EOF
)

#' @aliases removeAngles
#' @rdname breakPolys
#' @exportMethod removeAngles
methods::setMethod(
	f = "removeAngles",
	signature = c(x = "GVector"),
	function(x) .cleanGeom(src = sources(x), method = "rmsa", tolerance = NULL)
)

#' @aliases removeBridges
#' @rdname breakPolys
#' @exportMethod removeBridges
methods::setMethod(
	f = "removeBridges",
	signature = c(x = "GVector"),
	function(x) .cleanGeom(src = sources(x), method = "rmbridge", tolerance = NULL)
)

#' @aliases removeDangles
#' @rdname breakPolys
#' @exportMethod removeDangles
methods::setMethod(
	f = "removeDangles",
	signature = c(x = "GVector"),
	function(x, tolerance = -1) .cleanGeom(src = sources(x), method = "rmdangle", tolerance = tolerance)
)

#' @aliases removeDupCentroids
#' @rdname breakPolys
#' @exportMethod removeDupCentroids
methods::setMethod(
	f = "removeDupCentroids",
	signature = c(x = "GVector"),
	function(x) {
	
	if (!is.polygons(x)) stop("This tool can only be applied to polygon GVectors.")
	.cleanGeom(src = sources(x), method = "rmdac", tolerance = NULL)
	
	} # EOF
)

#' @aliases removeDups
#' @rdname breakPolys
#' @exportMethod removeDups
methods::setMethod(
	f = "removeDups",
	signature = c(x = "GVector"),
	function(x) .cleanGeom(src = sources(x), method = "rmdupl", tolerance = NULL)
)

#' @aliases removeSmallPolys
#' @rdname breakPolys
#' @exportMethod removeSmallPolys
methods::setMethod(
	f = "removeSmallPolys",
	signature = c(x = "GVector"),
	function(x , tolerance) {
	
	if (!is.polygons(x)) stop("This tool can only be applied to polygon GVectors.")
	.cleanGeom(src = sources(x), method = "rmarea", tolerance = tolerance)

	} # EOF
)

#' @aliases snap
#' @rdname breakPolys
#' @exportMethod snap
methods::setMethod(
	f = "snap",
	signature = c(x = "GVector"),
	function(x , tolerance) .cleanGeom(src = sources(x), method = "snap", tolerance = tolerance)
)

#' Implement vector cleaning using `sources()` name of a vector 
#'
#' @param src [sources()] name of a `GVector`.
#' @param method Character.
#' @param tolerance `NULL` or numeric.
#'
#' @noRd
.cleanGeom <- function(src, method, tolerance) {

	.locationRestore(x)
	
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

}

#' Geometry of a GVector (points, lines, or polygons)
#'
#' @description `geomtype()` reports whether a `GVector` represents points, lines, or polygons. The "`is.*`" functions test whether the `GVector` represents points, lines, or polygons.
#'
#' @param x A `GVector`.
#'
#' @return `geomtype()` returns either "points", "lines", or "polygons". The "`is.*`" functions return `TRUE` or `FALSE`.
#' 
#' @seealso [terra::geomtype()]
#'
#' @example man/examples/ex_GVector.r
#'
#' @aliases geomtype
#' @rdname geomtype
#' @exportMethod geomtype
methods::setMethod(
	f = 'geomtype',
	signature = 'GVector',
	definition = function(x) x@geometry
)

#' @aliases is.points
#' @rdname geomtype
#' @exportMethod is.points
methods::setMethod(
	f = 'is.points',
	signature = 'GVector',
	definition = function(x) {
		gt <- geomtype(x)
		gt == 'points'
	} # EOF
)

#' @aliases is.lines
#' @rdname geomtype
#' @exportMethod is.lines
methods::setMethod(
	f = 'is.lines',
	signature = 'GVector',
	definition = function(x) {
		gt <- geomtype(x)
		gt == 'lines'
	} # EOF
)

#' @aliases is.polygons
#' @rdname geomtype
#' @export is.polygons
methods::setMethod(
	f = 'is.polygons',
	signature = 'GVector',
	definition = function(x) {
		gt <- geomtype(x)
		gt == 'polygons'
	} # EOF
)

#' Geometry of a GVector (points, lines, or polygons)
#'
#' @description `geomtype()` reports whether a `GVector` represents points, lines, or polygons. The "`is.*`" functions test whether the `GVector` represents points, lines, or polygons.
#'
#' @param x A `GVector`.
#' 
#' @param grass Logical: If `FALSE` (default), return **terra**-like geometry types ("points", "lines", or "polygons"). If `TRUE`, return **GRASS**-like geometry types ("point", "line", "area"--note that these are a subset of the available types and may not be the "true" **GRASS** type).
#'
#' @return `geomtype()` returns either "points", "lines", or "polygons" if the `grass` arguments is `FALSE`, or "point", "line", "area" if `grass` is `TRUE`. The "`is.*`" functions return `TRUE` or `FALSE`.
#' 
#' @seealso [terra::geomtype()]
#'
#' @example man/examples/ex_GVector.r
#'
#' @aliases geomtype
#' @rdname geomtype
#' @exportMethod geomtype
methods::setMethod(
	f = "geomtype",
	signature = "GVector",
	definition = function(x, grass = FALSE) {

  	out <- x@geometry
	if (grass) {
		if (out == "points") {
			out <- "point"
		} else if (out == "lines") {
			out <- "line"
		} else if (out == "polygons") {
			out <- "area"
		}
	}
	out

  } # EOF
)

#' @aliases is.points
#' @rdname geomtype
#' @exportMethod is.points
methods::setMethod(
	f = "is.points",
	signature = "GVector",
	definition = function(x) {
		gt <- geomtype(x)
		gt == "points"
	} # EOF
)

#' @aliases is.lines
#' @rdname geomtype
#' @exportMethod is.lines
methods::setMethod(
	f = "is.lines",
	signature = "GVector",
	definition = function(x) {
		gt <- geomtype(x)
		gt == "lines"
	} # EOF
)

#' @aliases is.polygons
#' @rdname geomtype
#' @export is.polygons
methods::setMethod(
	f = "is.polygons",
	signature = "GVector",
	definition = function(x) {
		gt <- geomtype(x)
		gt == "polygons"
	} # EOF
)

#' Get geometry type from the sources() name of a vector
#'
#' @param x A `GVector` or the [sources()] name of one.
#'
#' @returns Character ("point", "line", or "area"; i.e., in **GRASS** nomenclature).
#'
#' @keywords internal
.geomtype <- function(x) {

	if (inherits(x, "GVector")) x <- sources(x)
	info <- .vectInfo(x)
	out <- info$geometry

	if (out == "points") {
		out <- "point"
	} else if (out == "lines") {
		out <- "line"
	} else if (out == "polygons") {
		out <- "area"
	}
	out

}

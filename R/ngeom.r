#' Number of geometries in a vector
#'
#' @description `ngeom()` returns the number of geometries (points, lines, or polygons) in a `GVector`.
#'
#' @param x A `GVector`.
#'
#' @returns An integer.
#'
#' @example man/examples/ex_GVector.r
#'
#' @seealso [nrow()]
#'
#' @aliases ngeom
#' @rdname ngeom
#' @exportMethod ngeom
methods::setMethod(
	f = "ngeom",
	signature = c(x = "GVector"),
	function(x) x@nGeometries
)

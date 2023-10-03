#' Coerce as multipart GVector to a singlepart GVector
#'
#' @description `GVectors` can contain a mix of "singlepart" and "multipart" features. A singlepart feature is a single point, set of connected line segments, or a polygon. A multipart feature is a set of lines, sets of connected line segments, or set of polygons that are treated as a single feature. This function converts all multipart features to singlepart features. If the `GVector has an attribute table, it will be removed from the output.
#' 
#' @param x A `GVector`.
#' 
#' @returns A `GVector`.
#' 
#' @seealso [aggregate()]
#' 
#' @example man/examples/ex_GVector.r
#' 
#' @aliases disagg
#' @rdname disagg
#' @exportMethod disagg
methods::setMethod(
	f = "disagg",
	signature = c(x = "GVector"),
	function(x) {

	.restore(x)

	# copying vector to obviate cases where multiple GVectors point to the same GRASS vector 
	src <- .copyGVector(x)

	.vRemoveTable(src)

	n <- nsubgeom(x)
	catTable <- data.table(cat = 1L:n)
	.vRecat(catTable, src)
	.makeGVector(src, table = NULL)

	} # EOF
)

#' Intersection of two GVectors
#'
#' @description The `intersect()` function selects the area of overlap between two `GVector`s of the same type (points, lines or polygons). You can also use the `*` operator (e.g., `vect1 * vect2`).
#'
#' @param x,y `GVector`s.
#'
#' @returns A `GVector`.
#'
#' @seealso [c()], [aggregate()], [crop()], [union()], [xor()]
#' 
#' @example man/examples/ex_union_intersect_xor_not.r
#'
#' @aliases intersect
#' @rdname intersect
#' @exportMethod intersect
methods::setMethod(
	f = "intersect",
	signature = c(x = "GVector", y = "GVector"),
	function(x, y) {

	compareGeom(x, y, geometry = TRUE)
	if (geomtype(x) != "polygons") stop("Only polygon GVectors can be intersected.")
	.restore(x)
		
	src <- .makeSourceName("v_overlay", "vector")
	rgrass::execGRASS(
		cmd = "v.overlay",
		ainput = sources(x),
		binput = sources(y),
		output = src,
		operator = "and",
		snap = -1,
		flags = c(.quiet(), "overwrite")
	)

	.makeGVector(src)
	
	} # EOF
)

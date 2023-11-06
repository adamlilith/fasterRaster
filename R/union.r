#' Combine two GVectors
#'
#' @description The `union()` function combines two "polgons" `GVector`s. The output will have at least as many geometries as both `GVector`s, plus more if polygons of one divide polygons of the other, and vice versa.
#'
#' @param x,y `GVector`s representing polygons.
#'
#' @returns A `GVector`.
#'
#' @seealso [c()], [aggregate()], [crop()], [intersect()], [xor()], [not()]
#' 
#' @example man/examples/ex_union_intersect_xor_not.r
#'
#' @aliases union
#' @rdname union
#' @exportMethod union
methods::setMethod(
	f = "union",
	signature = c(x = "GVector", y = "GVector"),
	function(x, y) {

	compareGeom(x, y, geometry = TRUE)
	if (geomtype(x) != "polygons") stop("Only polygon GVectors can be unioned.")

	.restore(x)
	
	src <- .makeSourceName("v_overlay", "vector")
	rgrass::execGRASS(
		cmd = "v.overlay",
		ainput = sources(x),
		binput = sources(y),
		output = src,
		operator = "or",
		snap = -1,
		flags = c("quiet", "overwrite")
	)

	.makeGVector(src)
	
	} # EOF
)

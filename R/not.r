#' Select parts of a polygon GVEctor not shared by another pologon GVector
#'
#' @description The `not()` function removes from the `x` "polygons" `GVector` parts that overlap with the `y` "polygons" `GVector`.
#'
#' @param x,y `GVector`s.
#'
#' @returns A `GVector`.
#'
#' @seealso [c()], [aggregate()], [crop()], [intersect()], [union()], [xor()]
#' 
#' @example man/examples/ex_union_intersect_xor_not.r
#'
#' @aliases xor
#' @rdname xor
#' @exportMethod xor
methods::setMethod(
	f = "not",
	signature = c(x = "GVector", y = "GVector"),
	function(x, y) {

	compareGeom(x, y, geometry = TRUE)
	if (geomtype(x) != "polygons") stop("Only polygon GVectors can be notted.")
	.restore(x)
		
	src <- .makeSourceName("v_overlay", "vector")
	rgrass::execGRASS(
		cmd = "v.overlay",
		ainput = sources(x),
		binput = sources(y),
		output = src,
		operator = "not",
		snap = -1,
		flags = c("quiet", "overwrite")
	)

	.makeGVector(src)
	
	} # EOF
)

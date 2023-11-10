#' Select parts of a polygon GVector erase shared by another polygon GVector
#'
#' @description The `erase()` function removes from the `x` "polygons" `GVector` parts that overlap with the `y` "polygons" `GVector`. You can also use the `-` operator (e.g., `vect1 - vect2`).
#'
#' @param x,y `GVector`s.
#'
#' @returns A `GVector`.
#'
#' @seealso [c()], [aggregate()], [crop()], [intersect()], [union()], [xor()]
#' 
#' @example man/examples/ex_union_intersect_xor_not.r
#'
#' @aliases erase
#' @rdname erase
#' @exportMethod erase
methods::setMethod(
	f = "erase",
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
		operator = "erase",
		snap = -1,
		flags = c("quiet", "overwrite")
	)

	.makeGVector(src)
	
	} # EOF
)

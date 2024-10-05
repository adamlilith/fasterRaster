#' Select parts of polygons not shared between two GVectors
#'
#' @description The `xor()` function selects the area that does *not* overlap between two "polygon" `GVector`s. You can also use the `/` operator, as in `vect1 / vect2`.
#'
#' @param x,y `GVector`s.
#'
#' @returns A `GVector`.
#'
#' @seealso [crop()], [intersect()], [union()], [erase()]
#' 
#' @example man/examples/ex_union_intersect_xor_erase.r
#'
#' @aliases xor
#' @rdname xor
#' @exportMethod xor
methods::setMethod(
	f = "xor",
	signature = c(x = "GVector", y = "GVector"),
	function(x, y) {

	compareGeom(x, y, geometry = TRUE)
	if (geomtype(x) != "polygons") stop("Only polygon GVectors can be xored.")
	.locationRestore(x)
		
	src <- .makeSourceName("xor_v_overlay", "vector")
	rgrass::execGRASS(
		cmd = "v.overlay",
		ainput = sources(x),
		binput = sources(y),
		output = src,
		operator = "xor",
		snap = -1,
		flags = c(.quiet(), "overwrite")
	)
	.makeGVector(src)
	
	} # EOF
)

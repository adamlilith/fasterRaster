#' Delaunay triangulation for points
#'
#' @description This function creates a Delaunay triangulation from a "points" `GVector`.
#'
#' @param x A `GVector` "points" object.
#'
#' @returns A `GVector`.
#'
#' @seealso [terra::delaunay()], module `v.delaunay` in **GRASS**
#'
#' @example man/examples/ex_delaunay_voronoi.r
#'
#' @aliases delaunay
#' @rdname delaunay
#' @exportMethod delaunay
methods::setMethod(
	f = "delaunay",
	signature = c(x = "GVector"),
	definition = function(x) {

	if (geomtype(x) != "points") stop("The vector must represent spatial points.")
	
	src <- .makeSourceName("v_delaunay", "vect")
	args <- list(
		cmd = "v.delaunay",
		input = sources(x),
		output = src,
		flags = c(.quiet(), "overwrite")
	)

	do.call(rgrass::execGRASS, args = args)
	.makeGVector(src)
	
	} # EOF
)

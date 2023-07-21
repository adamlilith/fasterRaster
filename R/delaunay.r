#" Delaunay triangulation for points
#"
#" @description This function creates a Delaunay triangulation from a set of spatial points.
#"
#" @param x A `GVector` "points" object.
#"
#" @returns A `GVector`.
#"
#" @seealso [terra::delaunay()], module `v.delaunay` in **GRASS**
#"
#" @example man/examples/ex_pointOperations.r
#"
#" @aliases delaunay
#" @rdname delaunay
#" @exportMethod delaunay
methods::setMethod(
	f = "delaunay",
	signature = c(x = "GVector"),
	definition = function(x) {

	if (geomtype(x) != "points") stop("The vector must represent spatial points.")
	
	gn <- .makeGName("delaunay", "vect")
	args <- list(
		cmd = "v.delaunay",
		input = .gnames(x),
		output = gn,
		flags = c("quiet", "overwrite"),
		intern = TRUE
	)

	do.call(rgrass::execGRASS, args=args)
	.makeGVector(gn)
	
	} # EOF
)

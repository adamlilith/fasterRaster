#' Create a randomly-positioned tesselation
#'
#' @description This function partitions a region into Voronoi polygons that completely overlap it. Each polygon has a random center. The function is essentially a wrapper for [spatSample()] and [voronoi()].
#'
#' @param x A `GRaster` or `GVector` used to constrain the location of random points used to create the tesselation.
#'
#' @param size Numeric integer or integer: Number of polygons.
#' 
#' @param seed Numeric integer, integer, or `NULL` (default): Value used as a random seed. If `NULL`, a random seed will be generated by **GRASS**.
#'
#' @returns A `GVector`.
#'
#' @example man/examples/ex_delaunay_voronoi.r
#' 
#' @aliases rvoronoi
#' @rdname rvoronoi
#' @exportMethod rvoronoi
methods::setMethod(
	f = "rvoronoi",
	signature = c(x = "GRaster"),
	function(x, size = 100, seed = NULL) .rvoronoi(x = x, size = size, seed = seed)
)

#' @aliases rvoronoi
#' @rdname rvoronoi
#' @exportMethod rvoronoi
methods::setMethod(
	f = "rvoronoi",
	signature = c(x = "GVector"),
	function(x, size = 100, seed = NULL) .rvoronoi(x = x, size = size, seed = seed)
)

#' @noRd
.rvoronoi <- function(x, size, seed) {
	
	rp <- spatSample(x = x, size = size, as.points = TRUE, values = FALSE, seed = seed)

	out <- voronoi(rp, buffer = 0)
	out

}

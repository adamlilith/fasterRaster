#' @title Topology (2- or 3-dimensions) of a GRaster or GVector
#' @aliases topology
#' @description `GRaster`s and `GVector`s can have 2-dimensional or 3-dimensional coordinates. This function returns the dimensions of the object.
#'
#' @param x A `GSpatial` object (i.e., a `GRaster` or `GVector`).
#'
#' @returns Either "2D" or "3D".
#'
#' @seealso [is.2d()], [is.3d()]
#'
#' @example man/examples/ex_GRaster.r
#'
#' @exportMethod topology
methods::setMethod(
	f = "topology",
	signature = "GSpatial",
	definition = function(x) x@topology
)

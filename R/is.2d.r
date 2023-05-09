#' Test if a GRaster or GVector is 2- or 3-dimensional
#'
#' @description Tests whether a a GRaster or GVector is 2- or 3-dimensional.
#'
#' @param An object that inherits from the `GSpatial` class (i.e., a `GRaster` or `GVector`).
#'
#' @returns Logical.
#'
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @aliases is.2d
#' @rdname is.2d
#' @exportMethod is.2d
methods::setMethod(
	f = 'is.2d',
	signature = 'GSpatial',
	definition = function(x) topology(x) == '2D'
)

#' @aliases is.3d
#' @rdname is.2d
#' @exportMethod is.3d
methods::setMethod(
	f = 'is.3d',
	signature = 'GSpatial',
	definition = function(x) topology(x) == '3D'
)

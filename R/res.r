#' Spatial resolution of a 'GRaster'
#'
#' @description Spatial resolution of a `GRaster`. `res()` reports 2-dimensional (x and y) resolution. `res3d()` reports resolution for 3-dimensional rasters.
#'
#' @param x A `GRaster`.
#'
#' @return A numeric vector. For both `res()` and `res3d()`, the first value is the length of cells in the x-direction and the second the length of cells in the y-direction. For `res3d()` the third value is height of a voxel (the z-direction).
#'
#' @example man/examples/ex_GRaster.r
#'
#' @aliases res
#' @rdname res
#' @export
#' @exportMethod res3d
setMethod(
	f = 'res',
	signature = 'GRaster',
	definition = function(x) x@resolution[1L:2L]
)

#' @rdname res
#' @aliases res3d
#' @exportMethod res3d
setMethod(
	f = 'res3d',
	signature = 'GRaster',
	definition = function(x) x@resolution
)

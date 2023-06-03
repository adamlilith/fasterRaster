#' Spatial resolution
#'
#' @description Spatial resolution of a `GRaster`:
#' 
#' `res()`: 2-dimensional resolution (x and y) .\cr
#' `res3d()`: 3-dimensinal resolution (z, y, and z).\cr
#' `zres()`: Resolution of the third dimension (z).
#'
#' @param x A `GRaster`.
#'
#' @return A numeric vector. For both `res()` and `res3d()`, the first value is the length of cells in the x-direction and the second the length of cells in the y-direction. For `res3d()` the third value is height of a voxel (the z-direction). For `zres()` the value is the resolution in the z-direction.
#'
#' @seealso [terra::res()]
#'
#' @example man/examples/ex_GRaster.r
#'
#' @aliases res
#' @rdname res
#' @exportMethod res
setMethod(
	f = 'res',
	signature = 'GRegion',
	definition = function(x) x@resolution[1L:2L]
)

#' @aliases res3d
#' @rdname res
#' @exportMethod res3d
setMethod(
	f = 'res3d',
	signature = 'GRegion',
	definition = function(x) c(xres=x@resolution[1L], yres=x@resolution[2L], zres=x@resolution[3L])
)

#' @aliases zres
#' @rdname res
#' @exportMethod zres
setMethod(
	f = 'zres',
	signature = 'GRegion',
	definition = function(x) x@resolution[3L]
)

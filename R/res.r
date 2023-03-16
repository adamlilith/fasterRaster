#' Spatial resolution of a 'GRaster'
#'
#' Spatial resolution of a `GRaster`. `res()` reports 2-dimensional (x and y) resolution. `res3d()` reports resolution for 3-dimensional rasters.
#'
#' @param x A `GRaster`.
#'
#' @return A numeric vector. For both `res()` and `res3d()`, the first value is the length of cells in the x-direction and the second the length of cells in the y-direction. For `res3d()` the third value is length of a voxel in the z-direction.
#'
#' @example man/examples/example_GRaster.r
#'
#' @export

# if (!isGeneric('res')) setGeneric(name='res', def=function(x) standardGeneric('res'))
# if (!isGeneric('res3d')) setGeneric(name='res3d', def=function(x) standardGeneric('res3d'))
setMethod(
	f = 'res',
	signature = 'GRaster',
	definition = function(x) x@resolution[1L:2L]
)

setMethod(
	f = 'res3d',
	signature = 'GRaster',
	definition = function(x) x@resolution
)

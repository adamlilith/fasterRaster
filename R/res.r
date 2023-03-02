#' Spatial resolution of a 'GRaster'
#'
#' Spatial resolution of a `GRaster`.
#'
#' @param x A `GRaster`.
#'
#' @return A numeric vector. The first value is the length of cells in the x-direction and the second the length of cells in the y-direction.
#'
#' @example man/examples/example_GRaster.r
#'
#' @export
if (!isGeneric('res')) res.GRaster <- setGeneric(name='res', def=function(x) { standardGeneric('res') })
setMethod(
	f='res',
	signature='GRaster',
	definition=function(x) x@resolution
)

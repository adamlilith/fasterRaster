#' Number of layers of a `GRaster` raster
#'
#' Number of layers of a `GRaster` raster object.
#'
#' @param x A `GRaster`.
#'
#' @return A numeric value.
#'
#' @example man/examples/example_GRaster.r
#'
#' @export
if (!isGeneric('nlyr')) nlyr.GRaster <- setGeneric(name='nlyr', def=function(x) { standardGeneric('nlyr') })
setMethod(
	f = 'nlyr',
	signature = 'GRaster',
	definition = function(x) x@dimensions[3L]
)

#' Minimum and maximum values across all non-NA cells of a 'GRaster'
#'
#' Minimum and maximum values across all non-NA cells of a 'GRaster'.
#'
#' @param x A `GRaster`.
#'
#' @return A numeric matrix.
#' 
#' @example man/examples/example_GRaster.r
#'
#' @export
if (!isGeneric('minmax')) minmax.GRaster <- setGeneric(name='minmax', def=function(x) { standardGeneric('minmax') })
setMethod(
	f = 'minmax',
	signature = 'GRaster',
	definition = function(x) matrix(c(x@minVal, x@maxVal), nrow=2, byrow=TRUE, dimnames=list(c('min', 'max'), x@rname))
)

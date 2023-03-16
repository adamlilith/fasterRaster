#' Number of categories in a 'GRaster'
#'
#' Reports number of categories in a categorical `GRaster`.
#'
#' @param x A `GRaster`.
#'
#' @return A numeric vector with one value per layer in the `GRaster`.
#' 
#' @example man/examples/example_GRaster.r
#'
#' @export
# if (!isGeneric('ncats')) setGeneric(name='ncats', def=function(x) standardGeneric('ncats'))
setMethod(
	f = 'ncats',
	signature = 'GRaster',
	definition = function(x) x@numCategories
)

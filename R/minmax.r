#' Minimum and maximum values across all non-NA cells of a GRaster
#'
#' Minimum and maximum values across all non-NA cells of a `GRaster`.
#'
#' @param x A `GRaster`.
#'
#' @return A numeric matrix.
#' 
#' @seealso [terra::minmax()]
#' 
#' @example man/examples/ex_GRaster.r
#'
#' @aliases minmax
#' @rdname minmax
#' @exportMethod minmax
setMethod(
	f = 'minmax',
	signature = 'GRaster',
	definition = function(x) matrix(c(x@minVal, x@maxVal), nrow=2, byrow=TRUE, dimnames=list(c('min', 'max'), names(x)))
)

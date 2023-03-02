#' Vertical extent of a 'GVector' or 'GRaster'
#'
#' `GRasters` and `GVectors` can have 2-dimensional or 3-dimensional coordinate systems. If 2-dimensional, the vertical extent is assumed to be 0. If 3-dimensional, this function reports the lowest and highest coordinates.
#'
#' @param x A `GRaster` or `GVector`.
#'
#' @return A numeric matrix.
#'
#' @example man/examples/example_GRaster.r
#'
#' @export

if (!isGeneric('zExt')) zExt.GRaster <- setGeneric(name='zExt', def=function(x) { standardGeneric('zExt') })
if (!isGeneric('zExt')) zExt.GVector <- setGeneric(name='zExt', def=function(x) { standardGeneric('zExt') })

.zExt <- function(x) {
	matrix(c(x@bottom, x@top), nrow=2, byrow=TRUE, dimnames=list(c('bottom', 'top'), x@rname))
}

setMethod(f='zExt', signature='GRaster',
	definition = function(x) .zExt(x)
)

setMethod(f='zExt', signature='GVector',
	definition = function(x) .zExt(x)
)

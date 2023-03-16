#' Spatial extent of a 'GRaster' or 'GVector'
#'
#' Spatial extent of a 'GRaster' or 'GVector'.
#'
#' @param x A `GRaster` or `GVector`.
#' @param vector Logical: if `FALSE` (default), return a `SpatExtent` object. If `TRUE`, return a vector.
#'
#' @return A numeric vector.
#'
#' @examplesIf man/examples/example_slots.r
#'
#' @export

if (!isGeneric('ext')) setGeneric(name='ext', def=function(x, vector) standardGeneric('ext'))
# setGeneric(name='ext', def=function(x, vector) standardGeneric('ext'))
setMethod(
	f = 'ext',
	signature = 'GSpatial',
	definition = function(x, vector = FALSE) {

	x <- c(xmin=x@extent[1L], xmax=x@extent[2L], ymin=x@extent[3L], ymax=x@extent[4L])
	if (!vector) x <- terra::ext(x)
	x

})

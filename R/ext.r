#' @title Spatial extent of a GRaster or GVector
#'
#' @description `ext()` provides the 2-dimensional spatial extent of a `GRaster` or `GVector` (i.e., westernmost/easternmost and southernmost/northernmost coordinates of area represented). `zext()` provides the vertical extent (i.e., topmost and bottommost elevation of the volume represented). The vertical extent is only defined if the `GRaster` or `GVector` is 3-dimensional.
#'
#' @param x,obj An object that inherits from `GSpatial` (i.e., a `GRaster` or `GVector`).
#' @param vector Logical: If `FALSE` (default), return a `SpatExtent` object. If `TRUE`, return the extent as a named vactor.
#' @param ... Other arguments (generally unused).
#'
#' @return `ext()` returns a `SpatExtent` object (**terra** package) or a numeric vector.
#' `zext()` returns a numeric vector for `GRegion`s and `GRaster`s, and a numeric matrix for `GVector`s.
#' `st_bbox()` returns a `bbox` object (**sf** package).
#'
#' @seealso [terra::ext()], [sf::st_bbox()]
#' 
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @aliases ext
#' @rdname ext
#' @exportMethod ext
methods::setMethod(
	f = 'ext',
	signature = 'GSpatial',
	definition = function(x, vector = FALSE) {

	out <- c(xmin=x@extent[1L], xmax=x@extent[2L], ymin=x@extent[3L], ymax=x@extent[4L])
	if (!vector) out <- terra::ext(out)
	out

})

#' @aliases zext
#' @rdname ext
#' @exportMethod zext
methods::setMethod(
	f = 'zext',
	signature = 'GSpatial',
	definition = function(x) {
	x@zextent
})

st_bbox <- function(obj, ...) UseMethod('st_bbox', obj)

#' @rdname ext
#' @exportMethod st_bbox
setMethod('st_bbox', definition = function(obj, ...) st_bbox(obj, ...))

#' @rdname ext
#' @export
st_bbox <- function(obj, vector = FALSE, ...) {
	if (inherits(obj, 'GSpatial')) {
		out <- obj@extent
		out <- c(out[1L], out[3L], out[2L], out[4L])
		names(out) <- c('xmin', 'ymin', 'xmax', 'ymax')
		if (!vector) out <- sf::st_bbox(out, crs = st_crs(obj))
	} else {
		out <- sf::st_bbox(obj, ...)
		if (vector) out <- as.vector(out)
	}
	out
}

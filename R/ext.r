#' @title Spatial extent of a GRaster or GVector
#'
#' @description `ext()` provides the 2-dimensional spatial extent of a `GRaster` or `GVector` (i.e., westernmost/easternmost and southernmost/northernmost coordinates of area represented). `zext()` provides the vertical extent (i.e., topmost and bottommost elevation of the volume represented). The vertical extent is only defined if the `GRaster` or `GVector` is 3-dimensional.
#'
#' @param x,obj An object that inherits from `GSpatial` (i.e., a `GRaster` or `GVector`).
#' @param ... Other arguments (generally unused).
#'
#' @return `ext()` returns a `SpatExtent` object (**terra** package).
#' `zext()` returns a numeric vector for `GRaster`s and a numeric matrix for `GVector`s.
#' `st_bbox()` returns a `bbox` object (**sf** package).
#'
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @aliases ext
#' @rdname ext
#' @exportMethod ext
methods::setMethod(
	f = 'ext',
	signature = 'GSpatial',
	definition = function(x) {

	x <- c(xmin=x@extent[1L], xmax=x@extent[2L], ymin=x@extent[3L], ymax=x@extent[4L])
	x <- terra::ext(x)
	x

})

#' @aliases zext
#' @rdname ext
#' @exportMethod zext
methods::setMethod(
	f = 'zext',
	signature = 'GRaster',
	definition = function(x) {
	c(bottom = x@zextent[1L], top = x@zextent[2L])
})

#' @aliases zext
#' @rdname ext
#' @exportMethod zext
methods::setMethod(
	f = 'zext',
	signature = 'GVector',
	definition = function(x) {
	matrix(c(x@ztop, x@zbottom), byrow=FALSE, nrow=2, dimnames=list(c('top', 'bottom'), names(x)))
})

st_bbox <- function(obj, ...) UseMethod('st_bbox', obj)

#' @rdname ext
#' @exportMethod st_bbox
setMethod('st_bbox', definition = function(obj, ...) st_bbox(obj, ...))

#' @rdname ext
#' @export
st_bbox <- function(obj, ...) {
	if (inherits(obj, 'GSpatial')) {
		out <- obj@extent
		names(out) <- c('xmin', 'ymin', 'xmax', 'ymax')
		out <- sf::st_bbox(out, crs = st_crs(obj))
	} else {
		out <- sf::st_bbox(obj, ...)
	}
	out
}

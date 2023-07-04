#' Spatial extent or origin of a GRaster or GVector
#'
#' @description The extent or origin of `GSpatial` object (`GRegions`, `GRaster`s, and `GVector`s):
#' 
#' `ext()`: 2-dimensional spatial extent (i.e., westernmost/easternmost and southernmost/northernmost coordinates of area represented).\cr\cr
#' `zext()`: Vertical extent (i.e., topmost and bottommost elevation of the volume represented). The vertical extent is not `NA` only if the object is 3-dimensional.\cr\cr
#' `west()`, `east()`, `north()`, `south()`: Coordinates of one side of horiztonal extent.\cr\cr
#' `top()` and `bottom()`: Coordinates of top and bottom of vertical extent.\cr\cr
#' `origin()`: Coordinates of the northwest corner of the extent of the object.\cr\cr
#' 
#' @param x,obj An object that inherits from `GSpatial` (i.e., a `GRaster` or `GVector`) or missing. If missing, then the horitontal or vertical extent of the current [region][tutorial_regions] is returned.
#' @param vector Logical: If `FALSE` (default), return a `SpatExtent` object. If `TRUE`, return the extent as a named vactor.
#' @param char Logical: If `FALSE` (default), return a numeric value. If `TRUE`, return as a character.
#' @param ... Other arguments (generally unused).
#'
#' @returns The returned values depend on the function:
#' * `ext()`: A `SpatExtent` object (**terra** package) or a numeric vector.
#' * `zext()`: A numeric vector.
#' * `west()`, `east()`, `north()`, `south()`, `top()`, and `bottom()`: A numeric value or character.
#' * `st_bbox()` returns a `bbox` object (**sf** package).
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

	out <- c(x@extent[1L], x@extent[2L], x@extent[3L], x@extent[4L])
	names(out) <- c('xmin', 'xmax', 'ymin', 'ymax')
	if (!vector) out <- terra::ext(out)
	out

	} # EOF
)

#' @aliases zext
#' @rdname ext
#' @exportMethod zext
methods::setMethod(
	f = 'zext',
	signature = 'GSpatial',
	definition = function(x) {
	x@zextent
})

#' @aliases west
#' @rdname ext
#' @exportMethod west
methods::setMethod(
	f = 'west',
	signature = 'missing',
	definition = function(x, char = FALSE) {
	out <- unname(ext(region(), vector = TRUE)['xmin'])
	if (char) out <- as.character(out)
	out
})

#' @aliases west
#' @rdname ext
#' @exportMethod west
methods::setMethod(
	f = 'west',
	signature = 'GSpatial',
	definition = function(x, char = FALSE) {
	out <- unname(ext(x, vector = TRUE)['xmin'])
	if (char) out <- as.character(out)
	out
})

#' @aliases east
#' @rdname ext
#' @exportMethod east
methods::setMethod(
	f = 'east',
	signature = 'missing',
	definition = function(x, char = FALSE) {
	out <- unname(ext(region(), vector = TRUE)['xmax'])
	if (char) out <- as.character(out)
	out
})

#' @aliases east
#' @rdname ext
#' @exportMethod east
methods::setMethod(
	f = 'east',
	signature = 'GSpatial',
	definition = function(x, char = FALSE) {
	out <- unname(ext(x, vector = TRUE)['xmax'])
	if (char) out <- as.character(out)
	out
})

#' @aliases north
#' @rdname ext
#' @exportMethod north
methods::setMethod(
	f = 'north',
	signature = 'missing',
	definition = function(x, char = FALSE) {
	out <- unname(ext(region(), vector = TRUE)['ymax'])
	if (char) out <- as.character(out)
	out
})

#' @aliases north
#' @rdname ext
#' @exportMethod north
methods::setMethod(
	f = 'north',
	signature = 'GSpatial',
	definition = function(x, char = FALSE) {
	out <- unname(ext(x, vector = TRUE)['ymax'])
	if (char) out <- as.character(out)
	out
})

#' @aliases south
#' @rdname ext
#' @exportMethod south
methods::setMethod(
	f = 'south',
	signature = 'missing',
	definition = function(x, char = FALSE) {
	out <- unname(ext(region(), vector = TRUE)['ymin'])
	if (char) out <- as.character(out)
	out
})

#' @aliases south
#' @rdname ext
#' @exportMethod south
methods::setMethod(
	f = 'south',
	signature = 'GSpatial',
	definition = function(x, char = FALSE) {
	out <- unname(ext(x, vector = TRUE)['ymin'])
	if (char) out <- as.character(out)
	out
})

#' @aliases top
#' @rdname ext
#' @exportMethod top
methods::setMethod(
	f = 'top',
	signature = c(x = 'missing'),
	definition = function(x, char = FALSE) {
	out <- unname(zext(region())['top'])
	if (char) out <- as.character(out)
	out
})

#' @aliases top
#' @rdname ext
#' @exportMethod top
methods::setMethod(
	f = 'top',
	signature = 'GSpatial',
	definition = function(x, char = FALSE) {
	out <- unname(zext(x)['top'])
	if (char) out <- as.character(out)
	out
})

#' @aliases bottom
#' @rdname ext
#' @exportMethod bottom
methods::setMethod(
	f = 'bottom',
	signature = c(x = 'GSpatial'),
	definition = function(x, char = FALSE) {
	out <- unname(zext(region())['bottom'])
	if (char) out <- as.character(out)
	out
})

#' @aliases bottom
#' @rdname ext
#' @exportMethod bottom
methods::setMethod(
	f = 'bottom',
	signature = 'GSpatial',
	definition = function(x, char = FALSE) {
	out <- unname(zext(x)['bottom'])
	if (char) out <- as.character(out)
	out
})

#' @aliases origin
#' @rdname ext
#' @exportMethod origin
setMethod(
	f = 'origin',
	signature = c(x = 'GSpatial'),
	definition = function(x) ext(x, vector=TRUE)[c('xmin', 'ymin')]
)

st_bbox <- function(obj, ...) UseMethod('st_bbox', obj)

#' @rdname ext
#' @exportMethod st_bbox
setMethod('st_bbox', definition = function(obj, ...) st_bbox(obj, ...))

#' @rdname ext
#' @export
st_bbox <- function(obj, ...) {
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

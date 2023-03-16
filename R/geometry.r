#' Geometry `GVector` (points, lines, or polygons)
#'
#' `geometry()` reports whether a `GVector` represents points, lines, or polygons.
#'
#' @param x A `GVector`.
#'
#' @return One of: "points", "lines", or "polygons".
#'
#' @example man/examples/example_GVector.r
#'
#' @export
# if (!isGeneric('geometry')) setGeneric(name='geometry', def=function(x) standardGeneric('geometry'))
setMethod(
	f = 'geometry',
	signature = 'GVector',
	definition = function(x) x@geometry
)

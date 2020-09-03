#' Convert degrees between cases with 0 = north and 0 = east
#' 
#' This function converts between cases where 0 degrees (direction) is intended to point east, with degrees running counterclockwise so 90 is north, 180 is west, and 270 south (this the the default for GRASS 7 modules) and cases where 0 degrees is intended to point north, with degrees running clockwise, so 90 is east, 180 south, and 270 west.
#' @param x Numeric. Degrees. The conversion is symmetric, so \code{x} can represent degrees in the system where 0 is north or 0 is east.
#' @return Numeric.
#' @examples
#' degreeConvert(0)
#' degreeConvert(90)
#' @export

degreeConvert <- function(x) {
	(360 - x) %% 360
}

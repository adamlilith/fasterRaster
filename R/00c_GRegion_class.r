#' @title Classes for "fasterRaster" locations, rasters, and vectors
#'
#' @aliases GRegion
#' @rdname GLocation
#' @exportClass GRegion
GRegion <- methods::setClass(
	Class = "GRegion",
	contains = "GSpatial",
	slots = list(
		dimensions = "integer",         # 3 integers
		resolution = "numeric"          # 3 numerics
	),
	prototype = prototype(
		dimensions = c(NA_integer_, NA_integer_, NA_integer_),
		resolution = c(NA_real_, NA_real_, NA_real_)
	)
)

methods::setValidity("GRegion",
	function(object) {
		if (length(object@dimensions) != 3L) {
			"@dimensions must be three elements long."
		} else if (any(object@dimensions[1L:2L] <= 0L)) {
			"First two values in @dimensions must be positive integers."
		} else if (!is.na(object@dimensions[3L]) && object@dimensions[3L] <= 0L) {
			"Third value in @dimensions must be NA_integer_ or a positive integer."
		} else if (length(object@resolution) != 3) {
			"@resolution must be three values long."
		} else if (any(object@resolution[1L:2L] <= 0)) {
			"First two values in @resolution must be positive real values."
		} else if (!is.na(object@resolution[3L]) && object@resolution[3L] <= 0) {
			"Third value in @resolution must be NA_real_ or a positive real value."
		} else {
			TRUE
		}

	} # EOF
)

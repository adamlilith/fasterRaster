#' @name GRegion
#' @rdname GSession
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

setValidity("GRegion",
	function(object) {
		if (any(object@dimensions[1L:2L] <= 0L)) {
			"First two values in @dimensions must be positive integers."
		} else if (!is.na(object@dimensions[3L]) && object@dimensions[3L] <= 0L) {
			"Third value in @dimensions must be NA or a positive integer."
		} else if (any(object@resolution[1L:2L] <= 0)) {
			"First two values in @resolution must be positive real values."
		} else if (!is.na(object@resolution[3L]) && object@resolution[3L] <= 0) {
			"Third value in @resolution must be NA or a positive real value."
		} else {
			TRUE
		}

	} # EOF
)

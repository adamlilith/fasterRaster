#' Class of a fasterRaster object
#'
#' This function is a workaround the CRAN complaint that using [class()] is not warranted in code. It is a replacement for that function when the object is a `GSession`, `GSpatial`, `GRaster`, or `GVector` object.
#'
#' @param x A `GSession` object or one that contains it.
#'
#' @return Character.
#'
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @aliases fastClass
#' @rdname fastClass
#' @exportMethod fastClass
methods::setMethod(
	f = 'fastClass',
	signature = c(x = 'GSession'),
	definition = function(x) {
	
	# have to test for lowest-level classes first because they contain higher-level ones
	if (inherits(x, 'GVector')) {
		'GVector'
	} else if (inherits(x, 'GRaster')) {
		'GRaster'
	} else if (inherits(x, 'GSpatial')) { # contains GRaster and GVector
		'GSpatial'
	} else if (inherits(x, 'GSession')) { # contains GSpatial
		'GSession'
	}
	} # EOF
)

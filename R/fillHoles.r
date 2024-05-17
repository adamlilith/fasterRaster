#' Fill holes in a GVector
#'
#' @description `fillHoles()` fills holes in a `GVector`.
#'
#' @param x A `GVector`.
#'
#' @param fail Logical: If `TRUE` (default), and **GRASS 8.4** or higher is not installed, cause an error. If `FALSE`, a warning will be displayed and a `NULL` value will be returned.
#'
#' @returns A `GVector`.
#'
#' @example man/examples/ex_GVector.r
#'
#' @aliases fillHoles
#' @rdname fillHoles
#' @exportMethod fillHoles
methods::setMethod(
	f = "fillHoles",
	signature(x = "GVector"),
	function(x, fail = TRUE) {
	
	if (grassInfo("versionNumber") < 8.4) {
	
		if (fail) {
			msg <- "This function requires GRASS 8.4 or above."
			stop(msg)
		} else {
			msg <- "This function requires GRASS 8.4 or above. A NULL value has been returned."
			warning(msg)
			out <- NULL
		}

	} else {

		.locationRestore(x)
		src <- .makeSourceName("v_fill_holes", "vector")
		rgrass::execGRASS(
			cmd = "v.fill.holes",
			input = sources(x),
			output = src,
			flags = c(.quiet(), "overwrite")
		)

		out <- .makeGVector(src, table = x)

	}

	out

	} # EOF
)
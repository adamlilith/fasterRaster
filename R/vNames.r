#' Names of columns of a GRASS vector's attribute table
#'
#' @description This function returns the column names of a **GRASS** vector's attribute table.
#'
#' @param x A `GVector` or the name of a vector in **GRASS**.
#' 
#' @returns Character vector.
#'
#' @example man/examples/ex_vFunctions.r
#' 
#' @aliases .vNames
#' @rdname vNames
#' @keywords internal
.vNames <- function(x) {

	if (inherits(x, "GVector")) {
		.locationRestore(x)
		src <- sources(x)
	} else {
		src <- x
	}

	if (.vHasDatabase(src)) {

		out <- rgrass::execGRASS(
			cmd = "db.columns",
			table = src,
			flags = .quiet(),
			intern = TRUE
		)
		
	} else {
		out <- NULL
	}
	out

}
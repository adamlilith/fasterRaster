#' "GRASS" vector attribute table column names
#'
#' @description This function returns the column names of a **GRASS** vector's attribute table.
#'
#' @param x A `GVector` or the name of a vector in **GRASS**.
#' 
#' @returns Character vector.
#' 
#' @aliases .vNames
#' @rdname vNames
#' @noRd
.vNames <- function(x) {

	if (inherits(x, "GVector")) {
		.locationRestore(x)
		src <- sources(x)
	} else {
		src <- x
	}

	args <- list(
		cmd = "db.columns",
		table = src,
		flags = .quiet(),
		intern = TRUE
	)

	out <- do.call(rgrass::execGRASS, args = args)
	out

}
#' Delete column(s) from a GRASS vector's attribute table
#'
#' @description Removes one or more columns from a **GRASS** vector's attribute table.
#'
#' @param x A `GVector` or the name of a vector in **GRASS**.
#' 
#' @param col Name or indices of columns to drop.
#' 
#' @returns Invisibly returns a `GVector` or the **GRASS** name of a vector.
#' 
#' @aliases .vDropColumn
#' @rdname vDropColumn
.vDropColumn <- function(x, col) {

	if (inherits(x, "GVector")) {
		.restore(x)
		src <- sources(x)
	} else {
		src <- x
	}

	if (inherits(col, c("numeric", "integer"))) {
		colNames <- .vNames(x)
		col <- colNames[col]
	}

	col <- paste(col, collapse = ",")

	args <- list(
		cmd = "v.db.dropcolumn",
		map = src,
		columns = col,
		flags = c("quiet"),
		intern = TRUE
	)

	do.call(rgrass::execGRASS, args = args)
	invisible(x)

}
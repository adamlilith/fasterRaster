#' Tests if vector has a database
#'
#' @description **GRASS** vectors typically have a database with the first column named `cat` and holding integer values indexing each feature. This table is mostly independent from the **fasterRaster** attribute table, which is stored as a `data.table` in the `@table` slot of a `GRaster`. This function tests to see if the vector does indeed have a database.
#'
#' This function is typically used by developers.
#'
#' @param x A `GVector` or the [sources()] name of a vector in **GRASS**.
#'
#' @returns Logical.
#'
#' @aliases .vHasTable
#' @rdname vHasTable
#' @noRd
.vHasTable <- function(x) {

	if (inherits(x, "GVector")) {
		.restore(x)
		src <- sources(x)
	} else {
		src <- x
	}

	info <- rgrass::execGRASS(
		"v.db.connect",
		map = src,
		flags = c(.quiet(), "p"),
		intern = TRUE
	)

	length(info) > 0L

}

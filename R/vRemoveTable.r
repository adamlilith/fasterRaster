#' Remove an attribute table from a vector in GRASS
#'
#' @description This function removes an attribute table from a vector in **GRASS*.  It does *not* remove the attribute table associated with a `GVector`, which is a an **R** object that points to a vector in **GRASS**. It removes the entire table, even the `cat` column, which is necessary for identifying individual features.
#'
#' **Note** that the function removes the table in **GRASS**, so all you need to do is `dbRemove(vector_name)`. You need not call `new_vector` <- dbRemove(vector_name)`.
#'
#' This function is typically used by developers. 
#'
#' @param x A `GVector` or the name of a vector in **GRASS**.
#'
#' @returns Invisibly returns a `GVector` or the name of a vector in **GRASS**.
#'
#' @aliases .vRemoveTable
#' @rdname dbRemoveTable
#' @noRd
.vRemoveTable <- function(x) {

	if (inherits(x, "GVector")) {
		.restore(x)
		src <- sources(x)
	} else {
		src <- x
	}

	if (.vHasTable(src)) {

		rgrass::execGRASS(
			cmd = "v.db.droptable",
			map = src,
			flags = c(.quiet(), "f")
		)

	}
	invisible(x)
		
}

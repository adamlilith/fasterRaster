#' Remove an attribute table from a vector in GRASS
#'
#' @description This function removes an attribute table from a vector in **GRASS*.  It does *not* remove the attribute table associated with a `GVector`, which is a an **R** object that points to a vector in **GRASS**. It removes the entire table, even the `cat` column, which is necessary for identifying individual features.
#'
#' **Note** that the function removes the table in **GRASS**, so all you need to do is `dbRemove(vector_name)`. You need not call `new_vector` <- dbRemove(vector_name)`.
#'
#' This function is typically used by developers. 
#'
#' @param x A `GVector`.
#'
#' @returns A `GVector` (invisibly). This may have an attribute table associated with it, but the table is in **R**. The table in **GRASS** will have been deleted.
#'
#' @aliases .dbRemove
#' @rdname dbRemove
#' @noRd
.dbRemove <- function(x) {

	.restore(x)
	args <- list(
		cmd = "v.db.droptable",
		map = sources(x),
		flags = c("quiet", "f"),
		intern = TRUE
	)
	do.call(rgrass::execGRASS, args = args)

	invisible(x)
	
}

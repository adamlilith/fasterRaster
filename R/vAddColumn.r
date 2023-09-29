#' Add a column to the attribute table of a GRASS vector
#'
#' @description This function adds a column to the attribute table of a vector in **GRASS**. The typical user is will not interact with this table directly. Rather, they should interact with the table in a `GVector`'s `@table` slot.
#' @param x A `GVector`.
#' @param name Character: Column name(s).
#' @param type Column data type(s): `INTEGER` or `integer`, `DOUBLE PRECISION` or `numeric`, `VARCHAR` or `character`. If `VARCHAR` or `character`, then defining `nchar` will be important. Case is ignored and partial matching is used.
#' @param nchar Positive integer: Maximum length of character strings to be stored in a `VARCHAR` column.
#' 
#' @returns A `GVector` (invisibly).
#'
#' @aliases .vAddColumn
#' @rdname vAddColumn
#' @noRd
.vAddColumn <- function(x, name, type, nchar = 100) {

	type <- pmatchSafe(type, c("integer", "numeric", "DOUBLE PRECISION", "character", "VARCHAR"))
	type[type == "integer"] <- "INTEGER"
	type[type == "numeric"] <- "DOUBLE PRECISION"
	type[type == "character"] <- "VARCHAR"

	type[type == "VARCHAR"] <- paste0(type[type == "VARCHAR"], "(", nchar, ")")
	column <- paste(paste(name, type), collapse = ",")

	args <- list(
		cmd = "v.db.addcolumn",
		map = sources(x),
		layer = "1",
		columns = column,
		flags = "quiet",
		intern = F
	)

	suppressMessages(info <- do.call(rgrass::execGRASS, args = args))
	invisible(x)

}

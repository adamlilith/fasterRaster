#' Convert a GRASS vector's attribute table to a data.table
#'
#' @description **GRASS** vectors can be linked to an attribute table, which can be exported from **GRASS** to **R** using this function. **This function is mostly of use to developers.**
#'
#' Values in the `cat` column are not necessarily unique--if a value appears more than once, the set of features they index are (in other software) called "multipart" features. The table can have more columns with metadata for each feature.
#'
#' This function is typically used by developers.
#'
#' @param x A `GVector` or the name of a vector in **GRASS**.
#'
#' @returns A `data.table` or `NULL` if the vector is not attached to a database.
#'
#' @aliases .vAsDataTable
#' @rdname vAsDataTable
#' @keywords internal
.vAsDataTable <- function(x) {

	if (inherits(x, "GVector")) {
		.locationRestore(x)
		src <- sources(x)
	} else {
		src <- x
	}

	hasTable <- .vHasDatabase(src)
	if (!hasTable) {
		out <- NULL
	} else {

		tf <- tempfile(fileext = ".csv")
		rgrass::execGRASS(
			"v.db.select",
			map = src,
			separator = "comma",
			null_value = "NA",
			file = tf,
			flags = "overwrite",
			intern = TRUE
		)
		out <- data.table::fread(tf)
	
	}

	out
}

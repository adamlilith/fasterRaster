#' Convert the attribute table of a vector to a data.table
#'
#' @description **GRASS** vectors typically have a database with the first column named `cat` and holding integer values indexing each feature. This table is mostly independent from the **fasterRaster** attribute table, which is stored as a `data.table` in the `@table` slot of a `GRaster`. This function exports the **GRASS** database to **R**.
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
#' @rdname dbToDataTable
#' @noRd
.vAsDataTable <- function(x) {

	if (inherits(x, "GVector")) {
		.restore(x)
		src <- sources(x)
	} else {
		src <- x
	}

	hasTable <- .vHasTable(src)
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

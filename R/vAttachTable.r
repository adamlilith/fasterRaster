#' Add a database table to the GRASS representation of a GVector
#'
#' @description `.vAttachTable()` adds a table to a **GRASS** vector. The existing table should be removed first with [.dbRemove()]. This table is meant to be "invisible" to most users--they should use interact with attribute tables using the `GVector` slot @table. Some functions require tables (e.g., [extract()] and [spatSample()]).
#'
#' @param x A `GVector`.
#' 
#' @param table Either `NULL` (default) or a `data.frame` or `data.table`. If `NULL`, then a bare minimal table will be created with a column named `cat`, holding sequential integer values, and an integer column with a random string name. If provided, the first column must be named `cat` and have integer values. The column names and data type of each column are used to create the table. The values in the table are not actually copied to the attribute table.
#' 
#' @returns A `GVector` (invisibly).
#' 
#' @aliases .vAttachTable
#' @rdname vAttachTable
#' @noRd
.vAttachTable <- function(x, table = NULL) {

	if (is.null(table)) {
		table <- data.frame(
			TEMPTEMP_count = 1:ngeom(x)
		)
		names(table)[1L] <- rstring(1L)
	}

	columns <- names(table)
	classes <- sapply(table, "class")
	for (i in seq_len(ncol(table))) {

		if (classes[i] == "integer") {
			columns[i] <- paste0(columns[i], " INTEGER")
		} else if (classes[i] == "numeric") {
			columns[i] <- paste0(columns[i], " DOUBLE PRECISION")
		} else {
			nc <- nchar(table[ , i])
			nc <- max(nc)
			columns[i] <- paste0(columns[i], " VARCHAR(", nc, ")")
		}

	}

	src <- sources(x)
	args <- list(
		cmd = "v.db.addtable",
		map = src,
		columns = columns,
		flags = "quiet",
		intern = TRUE
	)

	info <- suppressMessages(do.call(rgrass::execGRASS, args = args))
	invisible(x)

}

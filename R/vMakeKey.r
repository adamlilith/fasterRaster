#' Makes a "key" column in the attribute table of a GRASS vector
#'
#' @description This function creates a column in the **GRASS** attribute table of a `GVector` named "`key`" and populates it with unique string values. It then adds these values to the `@key` slot in the `GVector`. If the `key` column already exists, it updates the values in the attribute table and the `GVector`.
#'
#' @param x A `GVector`.
#'
#' @returns A `GVector`
#' 
#' @aliases .vMakeKey
#' @rdname vMakeKey
#' @noRd
.vMakeKey <- function(x) {

	.restore(x)

	cols <- .vNames(x)
	if (all(cols != "key")) {
		.vAddColumn(x, "key", type = "VARCHAR", nchar = 12L)
	}

	nGeoms <- ngeom(x, "GRASS")
	keys <- rstring(nGeoms, 12)

	# write file for SQL
	src <- sources(x)
	sql <- matrix(NA_character_, nrow = nGeoms, ncol = 1)
	for (i in seq_len(nGeoms)) {

		sql[i] <- paste0("UPDATE ", src, " SET key = '", keys[i], "' WHERE cat = ", i, ";")

	}
	tf <- tempfile(fileext = ".sql")
	write(sql, tf)

	args <- list(
		cmd = "db.execute",
		input = tf,
		flags = "quiet"
	)

	do.call(rgrass::execGRASS, args = args)

	# x@keys <- keys
	# validObject(x)
	invisible(x)

}

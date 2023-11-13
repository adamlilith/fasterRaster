#' Import a vector attribute "cat" table into GRASS and attach to a 
#' 
#' @description This function assigns new category values to geometries of a `GVector`. It can be run in three modes:
#' * Retain eExisting categories will be retained but renumbered starting with 1. Both `cats` and `start` must be `NULL`.
#' * Re-assign categories based on values in `cats`. Argument `start` is ignored.
#' * Re-assign categories, starting with the value given in `start`. Argument `cats` must be `NULL`.
#'
#' @param src The [sources()] name of the vector in **GRASS** to which to attach the table.
#' 
#' @param cats Either `NULL` (default) or a numeric vector with integers or an integer vector: New category values. If `NULL` and `start` is `NULL`, then new category numbers will be assigned so that they start at 1 and correspond to existing category numbers.
#'
#' @param start Either `NULL` (default) or an integer or numeric integer: Category number to start at.
#'
#' @returns The [sources()] name of the vector.
#' 
#' @aliases .vMakeCatTable
#' @rdname .vMakeCatTable
#' @noRd
.vRecat <- function(x, cats = NULL, start = NULL) {

	if (inherits(x, "GVector")) {
		.restore(x)
		src <- sources(x)
	} else {
		src <- x
	}

	if (is.null(cats) & is.null(start)) {

		cats <- .vCats(src)
		cats <- omnibus::renumSeq(cats)
		cats <- data.frame(newcat = cats)
	
	} else if (!is.null(cats)) {
	
		cats <- data.frame(newcat = as.integer(cats))
	
	} else {
	
		cats <- .vCats(src)
		cats <- cats + (start - min(cats))
	
	}

	tf <- tempfile(fileext = ".csv")
	tft <- paste0(tf, "t")
	utils::write.csv(cats, tf, row.names = FALSE)
	keyTableType <- '"Integer"'
	write(keyTableType, tft)

	# import table
	srcTable <- .makeSourceName("db_in_ogr_table", "table")
	rgrass::execGRASS(
		cmd = "db.in.ogr",
		input = tf,
		output = srcTable,
		flags = c(.quiet(), "overwrite")
	)

	# detach existing table
	.vDetachDatabase(src)

	# attach table
	rgrass::execGRASS(
		cmd = "v.db.connect",
		map = src,
		table = srcTable,
		key = "newcat",
		flags = c(.quiet(), "o")
	)

	# reclass
	srcIn <- src
	src <- .makeSourceName("v_reclass", "vector")
	rgrass::execGRASS(
		cmd = "v.reclass",
		input = srcIn,
		output = src,
		layer = srcTable,
		column = "newcat",
		type = "centroid",
		flags = c(.quiet(), "overwrite")
	)

	invisible(src)

}

#' Import a vector attribute "cat" table into GRASS and attach to a 
#' 
#' @description This function assigns new category values to geometries of a `GVector`. It first saves a `data.frame` containing a single column named "cat" with integer values to a temporary file. It then imports the table into **GRASS*. It ends by attaching the table to a *GRASS** vector.
#'
#' @param catTable A `data.frame` with one column named "cat" with integer values, one per geometry in a `GVector`.
#' 
#' @param src The [sources()] name of the vector in **GRASS** to which to attach the table.
#' 
#' @param removeTable Logical: If `TRUE` (default), remove existing attribute table before attaching new one with categories.
#' 
#' @returns The [sources()] name of the table imported into **GRASS**.
#' 
#' @aliases .vMakeCatTable
#' @rdname .vMakeCatTable
#' @noRd
.vRecat <- function(catTable, src, removeTable = TRUE) {

	if (inherits(src, "GVector")) src <- sources(x)

	# save cat table
	tf <- tempfile(fileext = ".csv")
	tft <- paste0(tf, "t")
	utils::write.csv(catTable, tf, row.names = FALSE)
	keyTableType <- '"Integer"'
	write(keyTableType, tft)

	# import into GRASS
	attsSrc <- .makeSourceName("db_in_ogr", "table")
	args <- list(
		cmd = "db.in.ogr",
		input = tf,
		output = attsSrc,
		key = "cat",
		flags = c("quiet", "overwrite")
	)
	do.call(rgrass::execGRASS, args = args)
	
	# attach to vector
	if (removeTable) .vRemoveTable(src)
	args <- list(
		cmd = "v.db.connect",
		map = src,
		table = attsSrc,
		key = "cat",
		flags = c("quiet", "overwrite")
	)
	do.call(rgrass::execGRASS, args = args)

	invisible(attsSrc)

}

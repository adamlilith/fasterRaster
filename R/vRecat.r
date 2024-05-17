#' Re-make vector "category" (cat) values
#' 
#' @description Each geometry in a **GRASS** vector has a "category" number (abbreviated "cat" in output and modules). Geometries can have the same or different numbers, but for functions to work as intended, they often need to have sequential category values, starting at 1, with no skips between integers. This function reconstitutes the category values of a vector in **GRASS** so they being with 1 and have no skips. **This function is mostly of use to developers.**
#'
#' @param x A `GVector` or the [sources()] name of a vector in **GRASS**.
#' 
#' @param gtype Character: Type of vector features in **GRASS** format (i.e., either `point`, `line`, or `area`). See [geomtype()].
#'
#' @returns The [sources()] name of a vector.
#'
#' @seealso [.vIncrementCat()], [.vCats()]
#' 
#' @aliases .vRecat
#' @rdname vRecat
#' @noRd
.vRecat <- function(x, gtype) {

	if (inherits(x, "GVector")) {
		.locationRestore(x)
		src <- sources(x)
	} else {
		src <- x
	}

	cats <- .vCats(src)

	# srcIn <- src
	# src <- .makeSourceName("v_category", "vector")

	# rgrass::execGRASS(
	# 	cmd = "v.category",
	# 	input = srcIn,
	# 	output = src,
	# 	option = "del",
	# 	cat = -1,
	# 	type = gtype,
	# 	flags = c(.quiet(), "overwrite", "t")
	# )

	# rgrass::execGRASS(
	# 	cmd = "v.category",
	# 	input = srcIn,
	# 	output = src,
	# 	option = "add",
	# 	type = gtype,
	# 	flags = c(.quiet(), "overwrite", "t")
	# )

	srcIn <- src
	src <- .makeSourceName("v_category", "vector")

	reCats <- as.character(cats)
	.vAttachDatabase(srcIn, table = data.frame(reCat = reCats))

	rgrass::execGRASS(
		cmd = "v.reclass",
		input = srcIn,
		output = src,
		column = "reCat",
		# type = gtype,
		flags = c(.quiet(), "overwrite")
	)

	# # # if (is.null(cats) & is.null(start)) {

	# # # 	cats <- .vCats(src, db = TRUE)
	# # # 	cats <- omnibus::renumSeq(cats)
	# # # 	cats <- data.frame(newcat = cats)
	
	# # # } else if (!is.null(cats)) {
	
	# # # 	cats <- data.frame(newcat = as.integer(cats))
	
	# # # } else {
	
	# # # 	cats <- .vCats(src)
	# # # 	cats <- cats + (start - min(cats))
	
	# # # }

	# # # tf <- tempfile(fileext = ".csv")
	# # # tft <- paste0(tf, "t")
	# # # utils::write.csv(cats, tf, row.names = FALSE)
	# # # keyTableType <- '"Integer"'
	# # # write(keyTableType, tft)

	# # # # import table
	# # # srcTable <- .makeSourceName("db_in_ogr_table", "table")
	# # # rgrass::execGRASS(
	# # # 	cmd = "db.in.ogr",
	# # # 	input = tf,
	# # # 	output = srcTable,
	# # # 	flags = c(.quiet(), "overwrite")
	# # # )

	# # # # detach existing table
	# # # .vDetachDatabase(src)

	# # # # attach table
	# # # rgrass::execGRASS(
	# # # 	cmd = "v.db.connect",
	# # # 	map = src,
	# # # 	table = srcTable,
	# # # 	key = "newcat",
	# # # 	flags = c(.quiet(), "o")
	# # # )

	# # # # reclass
	# # # srcIn <- src
	# # # src <- .makeSourceName("v_reclass", "vector")
	# # # rgrass::execGRASS(
	# # # 	cmd = "v.reclass",
	# # # 	input = srcIn,
	# # # 	output = src,
	# # # 	layer = srcTable,
	# # # 	column = "newcat",
	# # # 	type = "centroid",
	# # # 	flags = c(.quiet(), "overwrite")
	# # # )

	src

}

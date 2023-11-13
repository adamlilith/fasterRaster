#' Coerce as multipart GVector to a singlepart GVector
#'
#' @description `GVectors` can contain a mix of "singlepart" and "multipart" features. A singlepart feature is a single point, set of connected line segments, or a polygon. A multipart feature is a set of lines, sets of connected line segments, or set of polygons that are treated as a single feature. This function converts all multipart features to singlepart features. If the `GVector has an attribute table, it will be removed from the output.
#' 
#' @param x A `GVector`.
#' 
#' @returns A `GVector`.
#' 
#' @seealso [aggregate()]
#' 
#' @example man/examples/ex_aggregate_disagg.r
#' 
#' @aliases disagg
#' @rdname disagg
#' @exportMethod disagg
methods::setMethod(
	f = "disagg",
	signature = c(x = "GVector"),
	function(x) {

	.restore(x)

	srcDel <- .makeSourceName("v_category_del", "vector")
	rgrass::execGRASS(
		cmd = "v.category",
		input = sources(x),
		output = srcDel,
		option = "del",
		cat = -1,
		flags = c(.quiet(), "overwrite")
	)

	src <- .makeSourceName("v_category_add", "vector")
	rgrass::execGRASS(
		cmd = "v.category",
		input = srcDel,
		output = src,
		option = "add",
		flags = c(.quiet(), "overwrite")
	)

	# replicate data table
	if (nrow(x) == 0L) {
		table <- NULL
	} else {
		
		cats <- .vCats(x, db = FALSE, integer = TRUE)
		if (anyNA(cats)) {
			warning("At least one geometry has a combined category. Data table cannot be copied.")
			table <- NULL
		} else {
			table <- x@table
			table <- table[cats]
		}

	}

	# remove existing database
 	rgrass::execGRASS(
 		cmd = "v.db.connect",
 		map = src,
 		layer = "1",
 		flags = c(.quiet(), "overwrite", "d")
 	)

	# attach new database with same number of rows as categories in vector
	db <- data.frame(cat = .vCats(src))
	tf <- tempfile(fileext = ".csv")
	tft <- paste0(tf, "t")
	utils::write.csv(db, tf, row.names = FALSE)

	classes <- sapply(table, class)
	classes[!(classes %in% c("numeric", "integer", "character", "Date"))] <- '"String"'
	classes[classes == "numeric"] <- '"Real"'
	classes[classes == "integer"] <- '"Integer"'
	classes[classes == "character"] <- '"String"'
	classes[classes == "Date"] <- '"Date"'
	classes <- paste(classes, collapse = ",")

	write(classes, tft)

	srcTable <- .makeSourceName("db_in_ogr_table", NULL)
	rgrass::execGRASS(
		cmd = "db.in.ogr",
		input = tf,
		output = srcTable,
		# key = "cat",
		flags = c(.quiet(), "overwrite")
	)

	# connect database to vector
	rgrass::execGRASS(
		cmd = "v.db.connect",
		map = sources(x),
		table = srcTable,
		layer = "1",
		key = "cat_",
		flags = c(.quiet(), "overwrite", "o")
	)

	.makeGVector(src, table = table)

	} # EOF
)

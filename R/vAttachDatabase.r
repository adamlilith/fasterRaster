#' Add a database table to the GRASS representation of a GVector
#'
#' @description `.vAttachDatabase()` adds a table to a **GRASS** vector. The existing table should be removed first with [.dbRemove()]. This table is meant to be "invisible" to most users--they should use interact with attribute tables using the `GVector` slot @table. Some functions require tables (e.g., [extract()] and [spatSample()]).
#'
#' @param x A `GVector` or the name of a vector in **GRASS**.
#' 
#' @param table Either `NULL` (default) or a `data.frame` or `data.table`. If `NULL`, then a bare minimal table will be created with a column named `cat`, holding sequential integer values, and an integer column with a random string name. If provided, the first column must be named `cat` and have integer values. The column names and data type of each column are used to create the table. The values in the table are not actually copied to the attribute table.
#'
#' @param replace Logical: If `TRUE`, replace the existing database connection.
#' 
#' @returns Invisibly returns a `GVector` or the name of a vector in **GRASS**.
#' 
#' @aliases .vAttachDatabase
#' @rdname vAttachDatabase
#' @noRd
.vAttachDatabase <- function(x, table = NULL, replace = FALSE) {

	if (inherits(x, "GVector")) {
		.restore(x)
		src <- sources(x)
	} else {
		src <- x
	}

	if (replace || !.vHasTable(src)) {

		if (is.null(table)) {
			table <- data.frame(cat = .vCats(src))
		}

		if (!any(names(table) %in% "cat")) {
		
			cats <- data.table(cat = .vCats(src))
			table <- cbind(cats, table)
		
		}

		# columns <- names(table)
		# classes <- sapply(table, "class")
		# for (i in seq_len(ncol(table))) {

		# 	if (classes[i] == "integer") {
		# 		columns[i] <- paste0(columns[i], " INTEGER")
		# 	} else if (classes[i] == "numeric") {
		# 		columns[i] <- paste0(columns[i], " DOUBLE PRECISION")
		# 	} else {
		# 		nc <- nchar(table[ , i])
		# 		nc <- max(nc)
		# 		columns[i] <- paste0(columns[i], " VARCHAR(", nc, ")")
		# 	}

		# }

		# save table to disk
		tf <- tempfile(fileext = ".csv")
		tft <- paste0(tf, "t")
		utils::write.csv(table, tf, row.names = FALSE)
		
		classes <- sapply(table, class)
		classes[!(classes %in% c("numeric", "integer", "character", "Date"))] <- '"String"'
		classes[classes == "numeric"] <- '"Real"'
		classes[classes == "integer"] <- '"Integer"'
		classes[classes == "character"] <- '"String"'
		classes[classes == "Date"] <- '"Date"'
		classes <- paste(classes, collapse = ",")
		
		write(classes, tft)

		# import table as database
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


		# args <- list(
		# 	cmd = "v.db.addtable",
		# 	map = src,
		# 	columns = columns,
		# 	flags = .quiet(),
		# 	intern = TRUE
		# )

		# info <- do.call(rgrass::execGRASS, args = args)

	}
	invisible(x)

}

#' Convert the attribute table of a vector to a data.table
#'
#' @description All **GRASS** vectors have attribute tables with the first column named `cat` and holding integer values indexing each feature. This table is mostly independent from the **fasterRaster** attribute table, which is stored as a `data.table` in the `@table` slot of a `GRaster`. This function exports the **GRASS** attribute table to **R**.
#'
#' Values in the `cat` column are not necessarily unique--if a value appears more than once, the set of features they index are (in other software) called "multipart" features. The table can have more columns with metadata for each feature.
#'
#' This function is typically used by developers.
#'
#' @param x A `GVector`.
#'
#' @returns A `data.table`.
#'
#' @aliases db.as.data.table
#' @aliases db.as.data.table
#' @noRd
methods::setMethod(
	f = "db.as.data.table",
	signature = c(x = "GVector"),
	definition = function(x) {
	
	# tempFile <- tempfile(fileext = ".csv")
 	# suppressMessages(
	# 	data <- rgrass::execGRASS(
	# 		"db.out.ogr",
	# 		input = sources(x),
	# 		output = tempFile,
	# 		format = "CSV",
	# 		table = "rivers",
	# 		flags = c("quiet", "overwrite"),
	# 		intern = TRUE
	# 	)
	# )

	# out <- data.table::fread(tempFile)
 
	# # clean
	# names(out) <- trimws(names(out))

	# commas <- grepl(names(out), pattern = ",")
	# if (any(commas)) {
	# 	names(out)[commas] <- gsub(names(out)[commas], pattern = ",", replacement = "")
	# }

	# if (!getFastOptions("useDataTable")) out <- as.data.frame(out)

	### quieter, slower way
	data <- rgrass::execGRASS(
		"v.db.select",
		map = sources(x),
		intern = TRUE
	)

	# column names
	cols <- data[1L]
	cols <- strsplit(cols, "\\|")[[1L]]
	data <- data[-1L]

	# data values
	data <- strsplit(data, "\\|")
	out <- do.call(rbind.data.frame, data)
	colnames(out) <- cols

	out <- data.table::as.data.table(out)
	
	# # everything is exported as a character
	# ints <- which(datatype(x) == "integer")
	# nums <- which(datatype(x) == "numeric")
	
	# if (length(ints) > 0L) {
	# 	for (int in ints) {
	# 		if (any(out[[int]] == "")) {
	# 			out[[out[[int]] == "", int]] <- NA_character_
	# 		}
	# 		out[ , int] <- as.integer(out[[int]])
	# 	}
	# }
	
	# if (length(nums) > 0L) {
	# 	for (num in nums) {
	# 		if (any(out[[num]] == "")) {
	# 			out[out[[num]] == "", num] <- NA_character_
	# 		}
	# 		out[ , num] <- as.numeric(out[[num]])
	# 	}
	# }
	
	if (!getFastOptions("useDataTable")) out <- as.data.frame(x)
	
	out

	} # EOF
)

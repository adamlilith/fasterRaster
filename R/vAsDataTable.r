#' Convert the attribute table of a vector to a data.table
#'
#' @description All **GRASS** vectors have attribute tables with the first column named `cat` and holding integer values indexing each feature. This table is mostly independent from the **fasterRaster** attribute table, which is stored as a `data.table` in the `@table` slot of a `GRaster`. This function exports the **GRASS** attribute table to **R**.
#'
#' Values in the `cat` column are not necessarily unique--if a value appears more than once, the set of features they index are (in other software) called "multipart" features. The table can have more columns with metadata for each feature.
#'
#' This function is typically used by developers.
#'
#' @param x A `GVector` or the [sources()] of a `GVector` in **GRASS**.
#'
#' @returns A `data.table`.
#'
#' @aliases .vAsDataTable
#' @rdname dbToDataTable
#' @noRd
.vAsDataTable <- function(x) {

	if (inherits(x, "GVector")) {

		.restore(x)
		x <- sources(x)

	}

	data <- rgrass::execGRASS("v.db.select", map = x, intern = TRUE)

	### "data" can be
	# * a single vector headed by "cat" and no pipe characters if only this column is available
	# * a vector with pipe characters if more than one column is available

	cols <- data[1L]
	data <- data[-1L]
	
	### one column
	if (!grepl(data[1L], pattern = "\\|")) {
	
		if (any(grepl(data, pattern = "\\|"))) data <- gsub(data, pattern = "\\|", replacement = "")
		out <- data.table::data.table(cat = as.integer(data))
	
	### multiple columns
	} else {
	
		cols <- strsplit(cols, "\\|")[[1L]]

		# data values
		data <- strsplit(data, "\\|")
		out <- do.call(rbind.data.frame, data)
		
		# add NAs in missing columns
		diff <- length(cols) - ncol(out)
		if (diff > 0L) {
			
			empty <- matrix(NA, ncol = diff, nrow = nrow(out))

			out <- cbind(out, empty)
		
		}
		
		out <- data.table::as.data.table(out)
		colnames(out) <- cols

		for (i in seq_len(ncol(out))) {

			this <- out[[i]]
			if (!all(is.na(this)) && all(this == "")) {
				out[ , (i)] <- NA
			}

		}

	}
	out
}

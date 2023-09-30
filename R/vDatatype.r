#' GRASS vector column data types
#'
#' @description Returns the data type of the columns of a GRASS vector's attribute table. Column types can be "INTEGER", "DOUBLE PRECISION", or "VARCHAR".
#'
#' @param x A `GVector` or the name of a vector in **GRASS**.
#'
#' @returns A named character vector.
#' 
#' @aliases .vDatatype
#' @rdname vDatatype
#' @noRd 
.vDatatype <- function(x) {

	if (inherits(x, "GVector")) {
		.restore(x)
		src <- sources(x)
	} else {
		src <- x
	}

	args <- list(
		cmd = "db.describe",
		table = src,
		flags = c("quiet", "c"),
		intern = TRUE
	)

	info <- do.call(rgrass::execGRASS, args = args)

	info <- info[3L:length(info)]
	info <- strsplit(info, split = ":")

	names <- out <- rep(NA_character_, length(info))
	
	for (i in seq_along((info))) {
		names[i] <- info[[i]][2L]
		out[i] <- info[[i]][3L]
	}
	names(out) <- names
	out

}
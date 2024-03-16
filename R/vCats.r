#' Category column values of a GRASS vector
#'
#' @description Returns values in the `cat` column of a vector in **GRASS**.
#'
#' @param x A `GVector` or the name of a vector in **GRASS**.
#'
#' @param layer Integer, numeric integer, or character: Layer from which to obtain category values.
#'
#' @param db Logical: If `TRUE`, return category numbers from the database table associated with the vector. If `FALSE` (default), return category numbers from the actual vector.
#'
#' @param integer Logical: If `TRUE` (default), return category values as integers. In some cases, a geometry can have multiple categories, in which case `NA` is returned. If `FALSE`, return category values as strings (and thus, if a geometry has more than one category, does not convert to `NA`).
#'
#' @returns A vector.
#'
#' @example man/examples/ex_vFunctions.r
#'
#' @aliases .vCats
#' @rdname vCats
#' @noRd
.vCats <- function(x, layer = 1, db = FALSE, integer = TRUE) {

	if (inherits(x, "GVector")) {
		.locationRestore(x)
		src <- sources(x)
	} else {
		src <- x
	}

	layer <- as.character(layer)

	if (db) {
		dbase <- .vAsDataTable(src)
		if (any(names(dbase) == "frid")) {
			out <- dbase[["frid"]]
		} else if (any(names(dbase) == "cat")) {
			out <- dbase[["cat"]]
		} else {
			out <- NULL
		}
	} else {

		suppressMessages(out <- rgrass::execGRASS(
			cmd = "v.category",
			input = src,
			layer = layer,
			option = "print",
			flags = .quiet(),
			intern = TRUE,
			Sys_show.output.on.console = FALSE,
			echoCmd = FALSE
		))
	
	}

	if (integer) {
		
		# # ensure categories are ordered
		# out <- strsplit(out, split = "/")
		# out <- lapply(out, as.integer)
		# out <- lapply(out, sort)
		# out <- sapply(out, paste, collapse = "/")
		
		# # assign each category combination a new category value
		# uniq <- unique(out)
		# nuniq <- length(uniq)
		
		# out <- factor(out, seq_len(nuniq))
		# out <- levels(out)
		
		out <- suppressWarnings(as.integer(out))
		
	}
	out

}

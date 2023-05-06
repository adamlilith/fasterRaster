#' Process 'pmatch()' call
#'
#' Private. Shortcut for several lines of code to process `pmatch()`. Ignores case.
#'
#' @param x Character. String to match.
#' @param table Character vector. Values to which to match.
#' @param error Logical. If no match is found, return an error?
#' @param ... Arguments to pass to [pmatch()].
#'
#' @return One of the values in `table`.
#'
#' @noRd
.pmatch <- function(x, table, error = TRUE, ...) {

	x <- tolower(x)
	lowerTable <- tolower(table)
	match <- pmatch(x, lowerTable, ...)
	if (is.na(match)) {
		if (error) stop('Cannot find a match for ', sQuote('x'), '. Valid options include: ', paste(table, collapse=', '))
		out <- NA_character_
	} else {
		out <- table[match]
	}
	out

}

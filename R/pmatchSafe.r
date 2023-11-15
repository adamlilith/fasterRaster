#' Partial matching of strings with error checking
#'
#' This function is the same as [pmatch()], but it can throw an error instead of `NA` if not match is found, and can be forced to throw the error if more than the desired number of matches is found.
#'
#' @param x Character: String to match.
#' @param table Character vector: Values to which to match.
#' @param useFirst Logical: If `TRUE`, and there is more than one match for a given `x`, then the first value in `table` that matches `x` will be returned (without an error or warning).
#' @param error Logical: If no match is found, return an error?
#' @param ignoreCase Logical: If `TRUE` (default), ignore the case of values in `x` and `table` when checking for matches.
#' @param nmax Positive numeric integer: Maximum allowable number of matches. If more than this number of matches is found, an error will be thrown (regardless of the value of `error`).
#' @param ... Arguments to pass to [pmatch()].
#'
#' @returns One or more of the values in `table`.
#'
#' @examples
#' 
#' pmatchSafe("ap", c("apples", "oranges", "bananas"))
#' 
#' pmatchSafe("AP", c("apples", "oranges", "bananas"))
#' 
#' pmatchSafe("AP", c("apples", "oranges", "bananas"),
#'     ignoreCase = FALSE, error = FALSE)
#' 
#' pmatchSafe(c("ap", "ba"), c("apples", "oranges", "bananas"))
#' 
#' tryCatch(
#'     pmatchSafe("kumquats", c("apples", "oranges", "bananas")),
#' 	error = function(cond) FALSE
#' )
#' 
#' pmatchSafe("kumquats", c("apples", "oranges", "bananas"), error = FALSE)
#' 
#' pmatchSafe(c("ap", "corn"), c("apples", "oranges", "bananas"), error = FALSE)
#' 
#' tryCatch(
#'     pmatchSafe(c("ap", "ba"), c("apples", "oranges", "bananas"), nmax = 1),
#' 	error=function(cond) FALSE
#' )
#' 
#' @export 
pmatchSafe <- function(x, table, useFirst = FALSE, error = TRUE, ignoreCase = TRUE, nmax = length(x), ...) {

	if (ignoreCase) {
		x <- tolower(x)
		lowerTable <- tolower(table)
		match <- .pmatch(x, table = lowerTable, useFirst = useFirst, error = error)
	} else {
		match <- .pmatch(x, table = table, useFirst = useFirst, error = error)
	}
	
	if (length(match) > nmax) stop("Only ", nmax, " match(es) can be returned.")

	if (length(match) < length(x)) {
		if (error) stop("Cannot find a match. Valid options include: ", paste(table, collapse=", "))
	}

	match

}

#' @noRd
.pmatch <- function(x, table, useFirst, error) {

	nc <- nchar(x)

	matches <- integer()
	for (i in seq_along(x)) {
	
		theseMatches <- which(x[i] == substr(table, 1L, nc[i]))

		if (length(theseMatches) == 0L & error) stop("Cannot find a match. Valid options include: ", paste(table, collapse=", "))

		if (useFirst) theseMatches <- theseMatches[1L]
		matches <- c(matches, theseMatches)
	
	}

	table[matches]

}

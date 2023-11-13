#' Make a nearly-guaranteed unique string
#'
#' `rstring()` makes a string that is statically extremely likely to be unique.
#'
#' @param n Numeric integer: How many strings to make (default is 1).
#' 
#' @param x Numeric integer: Number of letters and digits to use to make the string. Default is 12, leading to a probability of two matching random strings of <3.7E-18 if `filesafe = TRUE` and <3.1E-22 if `FALSE`.
#' 
#' @param filesafe Logical: If `TRUE` (default), make file-safe names (leading character is a letter, only use letters and digits).
#'
#' @returns Character.
#'
#' @examples
#'
#' rstring(1)
#' rstring(5)
#' rstring(5, 3)
#'
#' @export
rstring <- function(n, x = 12, filesafe = TRUE) {

	n <- as.integer(n)
	x <- as.integer(x)
	if (n < 0L | x < 0L) stop(sQuote("n"), " and ", sQuote("x"), " must be > 0.")

	if (filesafe) {
		froms <- c(letters, LETTERS)
		start <- sample(froms, n, replace = TRUE)
		x <- x - 1L
	} else {
		froms <- c(letters, LETTERS, 0L:9L)
		start <- sample(froms, n, replace = TRUE)
	}

	froms <- c(letters, LETTERS, 0L:9L)
	out <- rep(NA_character_, n)
	for (i in seq_len(n)) {
		samps <- sample(froms, x, replace = TRUE)
		string <- c(start[i], samps)
		out[i] <- paste(string, collapse="")
	}
	out
	
}

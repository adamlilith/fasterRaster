#' Format a numeric series into an SQL value call
#'
#' @description This function takes as its argument a vector of integers or numeric values, and converts sequential runs to a range while keeping non-sequential values as-is. For example, `c(1, 5, 6, 7, 8, 9, 15, 16, 20)` becomes `"1,5-9,15-16,20"`. This reduces the number of characters necessary to supply to a SQL condition. This function is mainly of use to developers.
#'
#' @param x A vector of numerical values. The vector should be sorted from  lowers to highest for the most efficient "compression" of sequential ranges. Values will be coerced to class `integer`.
#'
#' @param maxChar Integer or numeric: Maximum number of characters to include in the output. If the output has more than this number of characters, the remainder is dropped, and the `trim` attribute of the output is set to `TRUE`. The default is 29900, which is the maximum length of an SQL statement that **GRASS** seems to be able to handle (minus a safety margin).
#'
#' @param sort Logical: If `TRUE` (default), sort `x` before converting to SQL. This can reduce the length of the output.
#'
#' @returns A character string. The string has three attributes. The `trim` attribute is `TRUE` or `FALSE`, depending on whether `maxChar` was reached or not (and subsequent numbers dropped from the string). The `lastIndex` attribute is the last index of `x` that was processed (i.e., the index of the last value in the output), and the number of values represented by the output.
#'
#' @examples
#'
#' x <- 1:5
#' seqToSQL(x)
#'
#' x <- c(1:5, 7)
#' seqToSQL(x)
#'
#' x <- c(1:5, 7, 15:16)
#' y <- c(1:5, 7, 15:16, 20)
#' seqToSQL(x)
#' seqToSQL(y)
#' 
#' seqToSQL(x, maxChar = 5)
#' seqToSQL(y, maxChar = 8)
#'
#' seqToSQL(10:1, sort = FALSE)
#' seqToSQL(10:1, sort = TRUE)
#'
#' @export
seqToSQL <- function(x, maxChar = 29900, sort = TRUE) {

	if (sort) x <- sort(x)
	x <- as.integer(x)
	x <- unique(x)
	n <- length(x)
	if (n == 0L) {
		out <- NULL
		trim <- NA		
		lastIndex <- NA_integer_
	} else if (n == 1L) {
		out <- as.character(x)
		trim <- FALSE
		lastIndex <- 1L
	} else {

		nMinus1 <- n - 1L

		out <- character()
		inseq <- FALSE
		xchar <- as.character(x)
		
		i <- 1L
		nc <- 0L
		while (nc <= maxChar & i <= nMinus1) {
		
			nextX <- x[i + 1L]
			nextInSeries <- x[i] + 1L
		
			# if sequential
			if (nextX == nextInSeries) {
			
				# if starting new series
				if (!inseq) {
					from <- xchar[i]
					inseq <- TRUE
				}
			
			} else {
			
				if (inseq) {
					to <- xchar[i]
					out <- paste(c(out, paste0(from, "-", to)), collapse = ",") 
					inseq <- FALSE
				} else {
					out <- paste(c(out, xchar[i]), collapse = ",") 
				}
			
			}
			
   			# hacky
			nc <- max(length(out), nchar(out) + inseq + 1L)
			i <- i + 1L
		
		} # next x
		
		## ended prematurely because string was too long
		if (nc > maxChar) {
		
		   	trim <- TRUE
			lastIndex <- i - 1L
		
		## got through the entire set
		} else {
			
			if (inseq && x[n - 1L] + 1L == x[n]) {
			
				to <- xchar[n]
				out <- paste(c(out, paste0(from, "-", to)), collapse = ",") 
				
			} else {
				# out <- c(out, as.character(c(n - 1L, n)))
				out <- paste(c(out, xchar[n]), collapse = ",") 
			}
			
			trim <- FALSE
			lastIndex <- n
			
		}
		
	}
	
	attr(out, "trim") <- trim
	attr(out, "lastIndex") <- lastIndex
	out

}

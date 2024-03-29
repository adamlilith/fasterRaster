#' Compare values accounting for differences due to floating point precision
#'
#' This function compares values while accounting for differences in floating point precision. This function is inspired by the **fpCompare** package.
#'
#' @param x,y Numeric
#' @param op Operator for comparison (must be in quotes): `"<"`, `">"`, `"<="`, `">="`, `"=="`, or `"!="`
#' @param tol Tolerance value: The largest absolute difference between `x` and `y` that is to be considered equality. Teh default is `.Machine$double.eps^0.5`.
#'
#' @return `TRUE`, `FALSE`, or `NA`
#' 
#' @examples
#' x <- 0.9 - 0.8
#' y <- 0.8 - 0.7
#' 
#' x < y
#' compareFloat(x, y, "<")
#' 
#' x <= y
#' compareFloat(x, y, "<=")
#' 
#' x == y
#' compareFloat(x, y, "==")
#' 
#' y > x
#' compareFloat(y, x, ">")
#' 
#' y >= x
#' compareFloat(y, x, ">=")
#' 
#' x != y
#' compareFloat(x, y, "!=")
#' 
#' @export

compareFloat <- function(x, y, op, tol = .Machine$double.eps^0.5) {

	# if (is.null(tol)) tol <- .Machine$double.eps^0.5

	out <- if (op == "<") {
		(y - x > tol)
	} else if (op == "<=") {
		(y - x > tol | abs(y - x) < tol)
	} else if (op == "==") {
		(abs(y - x) < tol)
	} else if (op == ">") {
		(x - y > tol)
	} else if (op == ">=") {
		(x - y > tol | abs(x - y) < tol)
	} else if (op == "!=") {
		(abs(x - y) > tol)
	} else {
		NA
	}

	out

}

#' Compare values accounting for differences due to floating point precision
#'
#' This function compares values while accounting for differences in floating point precision. For example, this seems non-sensical:\cr
#' \code{x <- 0.9 - 0.8} \cr
#' \code{y <- 0.8 - 0.7} \cr
#' \code{x == y} \cr
#' \code{compareFloat(x = 0.9 - 0.8, y = 0.8 - 0.7, '==')} \cr
#'This function is inspired by the \pkg{fpCompare} package.
#'
#' @param x,y Numeric
#' @param op Operator for comparison (must be in quotes): \code{'<'}, \code{'>'}, \code{'<='}, \code{'>='}, \code{'=='}, or \code{'!='}
#' @param tol Tolerance value. If \code{NULL}, then this is set equal to \code{.Machine$double.eps^0.5}.
#'
#' @return \code{TRUE}, \code{FALSE}, or \code{NA}
#' 
#' @examples
#' x <- 0.9 - 0.8
#' y <- 0.8 - 0.7
#' 
#' x < y
#' compareFloat(x, y, '<')
#' 
#' x <= y
#' compareFloat(x, y, '<=')
#' 
#' x == y
#' compareFloat(x, y, '==')
#' 
#' y > x
#' compareFloat(y, x, '>')
#' 
#' y >= x
#' compareFloat(y, x, '>=')
#' 
#' x != y
#' compareFloat(x, y, '!=')
#' 
#' @export

compareFloat <- function(x, y, op, tol = NULL) {

	if (is.null(tol)) tol <- .Machine$double.eps^0.5

	out <- if (op == '<') {
		(y - x > tol)
	} else if (op == '<=') {
		(y - x > tol | abs(y - x) < tol)
	} else if (op == '==') {
		(abs(y - x) < tol)
	} else if (op == '>') {
		(x - y > tol)
	} else if (op == '>=') {
		(x - y > tol | abs(x - y) < tol)
	} else if (op == '!=') {
		(abs(x - y) > tol)
	} else {
		NA
	}

	out

}

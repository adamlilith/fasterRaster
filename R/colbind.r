#' Add columns to the data table of a GVector
#'
#' @description `colbind()` is liek [cbind()], excet that it adds columns to the data table of a `GVector`. To combine rows, see [c()].
#'
#' @param x
#' @param ... `data.frame`s, `data.table`s, and/or `matrices`.
#'
#' @returns A `GVector`.
#'
#' @seealso [c()]
#'
#' @example man/examples/ex_c_colbind.r
#'
#' @aliases colbind
#' @rdname colbind
#' @exportMethod colbind
methods::setMethod(
	f = "colbind",
	signature = c(x = "GVector"),
	function(x, ...) {
	
	dots <- list(...)
	dots <- c(x@table, dots)
	
	table <- do.call(cbind, dots)
	if (!inherits(table, "data.table")) table <- data.table::as.data.table(table)
	x@table <- table
	x
	
	} # EOF
)

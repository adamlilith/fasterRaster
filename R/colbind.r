#' Add columns to the data table of a GVector
#'
#' @description `colbind()` is like [cbind()], except that it adds columns to the data table of a `GVector`. To combine rows, see [c()]. You combine multiple `data.frame`s, `data.table`s, or `matrices`, but you can only combine them with one `GVector` at a time.
#'
#' @param x A `GVector`, a `data.frame`, or a `data.table`. If `x` is a `GVector`, then subsequent items must be `data.frame`s, `data.table`s, or `matrices`.
#' @param ... A `GVector`, or `data.frame`s, `data.table`s, and/or `matrices`. If `x` is a `data.frame`, `data.table`, or `matrix`, then at least one argument in `...` must be a `GVector`.
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

#' @aliases colbind
#' @rdname colbind
#' @exportMethod colbind
methods::setMethod(
	f = "colbind",
	signature = c(x = "data.frame"),
	function(x, ...) {

	x <- data.table::as.data.table(x)
	colbind(x, ...)
	
	} # EOF
)

#' @aliases colbind
#' @rdname colbind
#' @exportMethod colbind
methods::setMethod(
	f = "colbind",
	signature = c(x = "matrix"),
	function(x, ...) {

	x <- data.table::as.data.table(x)
	colbind(x, ...)
	
	} # EOF
)

#' @aliases colbind
#' @rdname colbind
#' @exportMethod colbind
methods::setMethod(
	f = "colbind",
	signature = c(x = "data.table"),
	function(x, ...) {
	
	dots <- list(...)
	gv <- sapply(dots, inherits, "GVector")
	
	if (sum(gv) > 1L) stop("Can only combine tables with one GVector at a time.")
	tables <- lapply(dots, data.table::as.data.table)
	tables <- c(x, tables)
	
	table <- do.call(cbind, tables)
	if (!inherits(table, "data.table")) table <- data.table::as.data.table(table)
	out <- dots[[which(gv)]]
	out@table <- table
	out
	
	} # EOF
)

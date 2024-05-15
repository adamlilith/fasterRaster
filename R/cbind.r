#' Add columns to the data table of a GVector
#'
#' @description `colbind()` adds columns to the data table of a `GVector`. You can combine multiple a `GVector`'s data table with `data.frame`s, `data.table`s, `matrices`, or the data table(s) from other `GVector`(s). To combine two `GVector`s, see [rbind()].
#'
#' @param x,... The first argument must be a `GVector`. Subsequent arguments can be `data.frame`s, `data.table`s, `matrices`, or `GVector`s. Only the data tables of subsequent `GVector`s are added to the table in `x`; the geometries are ignored.
#'
#' @returns A `GVector`.
#'
#' @seealso [rbind()], [c()]
#'
#' @example man/examples/ex_cbind_rbind.r
#'
#' @aliases colbind
#' @rdname colbind
#' @exportMethod colbind
methods::setMethod(
	f = "colbind",
	signature = c(x = "GVector"),
	function(x, ...) {

	dots <- list(x, ...)
	for (i in seq_along(dots)) {
		if (inherits(dots[[i]], "GVector")) {
			dots[[i]] <- dots[[i]]@table
		}
	}
	
	table <- do.call(cbind, dots)
	if (!inherits(table, "data.table")) table <- data.table::as.data.table(table)
	x@table <- table
	x
	
	} # EOF
)


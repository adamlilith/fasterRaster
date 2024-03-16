#' Add columns to the data table of a GVector
#'
#' @description `cbind()` adds columns to the data table of a `GVector`. To combine two `GVector`s, see [rbind()]. You combine multiple a `GVector`'s data table with `data.frame`s, `data.table`s, `matrices`, or the data table(s) from other `GVector`(s).
#'
#' @param ... The first argument must be a `GVector`. Subsequent arguments can be `data.frame`s, `data.table`s, `matrices`, or `GVector`s. One the data tables of subsequent `GVector`s are used; the geometries are ignored.
#'
#' @returns A `GVector`.
#'
#' @seealso [rbind()], [c()]
#'
#' @example man/examples/ex_cbind_rbind.r
#'
#' @aliases cbind
#' @rdname cbind
#' @export cbind
cbind.GVector <- function(... ) {
	
	dots <- list(...)
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

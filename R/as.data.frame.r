#' Convert GVector to a data frame
#'
#' @description Convert a `GVector`'s attribute table to a `data.frame` or `data.table`.
#'
#' @param x A `GVector`.
#'
#' @returns A `data.frame` or `NULL` (if the `GRaster` has no attribute table).
#' 
#' @seealso [terra::as.data.frame()], [data.table::as.data.table()]
#' 
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @aliases as.data.frame
#' @rdname as.data.frame
#' @exportMethod as.data.frame
methods::setMethod(
	f = "as.data.frame",
	signature = c(x = "GVector"),
	definition = function(x) {
		if (nrow(x) > 0L) {
			as.data.frame(x@table)
		} else {
			NULL
		}

	} # EOF
)

#' @aliases as.data.table
#' @rdname as.data.frame
#' @exportMethod as.data.table
methods::setMethod(
	f = "as.data.table",
	signature = c(x = "GVector"),
	definition = function(x) {
		if (nrow(x) > 0L) {
			x@table
		} else {
			NULL
		}

	} # EOF
)

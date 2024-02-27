#' Remove a data table from a GVector
#'
#' @description This function removes a data table associated with a `GVector`.
#'
#' @param x A `GVector.
#'
#' @returns A `GVector`.
#'
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @aliases dropTable
#' @rdname dropTable
#' @exportMethod dropTable
methods::setMethod(
	f = "dropTable",
	signature = c(x = "GVector"),
	function(x) {
	
	out <- x
	out@table <- NULL
	out

	} # EOF
)

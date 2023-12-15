#' Return first or last part of the data frame of a GVector
#'
#' @description Return the first or last part of a `GVector`'s data table.
#'
#' @param x A `GVector`.
#' @param n Integer: Number of rows to display.
#' @param keepnums Logical: If no `rownames` are present, create them. Default is `TRUE`.
#' @param ... Other arguments.
#'
#' @returns A `data.table` or `data.frame`.
#' 
#' @seealso [terra::head()], [terra::tail()]
#'
#' @example man/examples/ex_GVector.r
#'
#' @aliases head
#' @rdname head
#' @exportMethod head
methods::setMethod(
	f = "head",
	signature = c(x = "GVector"),
	definition = function(x, n = 6L, keepnums = TRUE, ...) {

		nr <- nrow(x)
		out <- x@table[1L:n]
		if (!faster("useDataTable")) out <- as.data.frame(out)
		out

	} # EOF
)

#' @aliases tail
#' @rdname head
#' @exportMethod tail
methods::setMethod(
    f = "tail",
    signature = c(x = "GVector"),
    definition = function(x, n = 6L, keepnums = TRUE, ...) {

	nr <- nrow(x)
	out <- x@table[(nr - n + 1L):nr]
	if (!faster("useDataTable")) out <- as.data.frame(out)
	out

    } # EOF
)

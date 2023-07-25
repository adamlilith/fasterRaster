#' Return first or last part of the data frame of a GVector
#'
#' @description Return the first or last part of a data frame of a `GVector`.
#'
#' @param x A `GVector`.
#' @param n Integer: Number of rows to display.
#' @param keepnums Logical: If no `rownames` are present, create them. Default is `TRUE`.
#' @param ... Other arguments.
#'
#' @returns A `data.frame`.
#' 
#' @seealso [head()], [tail()]
#'
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @aliases head
#' @rdname head
#' @exportMethod head
methods::setMethod(
	f = "head",
	signature = c(x = "GVector"),
	definition = function(x, n = 6L, keepnums = TRUE, ...) {
		x <- as.data.frame(x)
		head(x, n = n, keepnums = keepnums, ...)
	} # EOF
)

#' @aliases tail
#' @rdname head
#' @exportMethod tail
methods::setMethod(
    f = "tail",
    signature = c(x = "GVector"),
    definition = function(x, n = 6L, keepnums = TRUE, ...) {
        x <- as.data.frame(x)
        tail(x, n = n, keepnums = keepnums, ...)
    } # EOF
)

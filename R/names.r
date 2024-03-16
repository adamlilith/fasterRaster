#' Name(s) of a GRaster or columns of a GVector's data table
#'
#' `names()` returns that names(s) of a `GRaster` or of columns of a `GVector`'s data table'.
#'
#' `names(value) <-` assigns a new name to the `GRaster` or to the columns of a `GVector`'s data table.
#'
#' @param x A `GRaster` or `GVector`.
#' @param value Character: Name(s) to assign to the raster(s).
#' 
#' @return Character vector.
#' 
#' @seealso [terra::names()]
#'
#' @example man/examples/ex_GRaster.r
#'
#' @aliases names
#' @rdname names
#' @exportMethod names
setMethod(
	f = "names",
	signature = "GRaster",
	definition = function(x) x@names
)

#' @rdname names
#' @aliases names<-
#' @exportMethod names<-
setMethod(
	f = "names<-",
	signature = "GRaster",
	definition = function(x, value) {
		x@names <- value
		methods::validObject(x)
		x
	}
)

#' @aliases names
#' @rdname names
#' @exportMethod names
setMethod(
	f = "names",
	signature = "GVector",
	definition = function(x) {
	
	if (nrow(x@table) > 0L) {
		names(x@table)
	} else {
		NULL
	}
	
	} # EOF
)

#' @rdname names
#' @aliases names<-
#' @exportMethod names<-
setMethod(
	f = "names<-",
	signature = "GVector",
	definition = function(x, value) {
	
	if (ncol(x) > 0L) names(x@table) <- value
	x
	
	} # EOF
)

#' Make unique names
#'
#' @param x A `GRaster`.
#' @returns A `GRaster`
#'
#' @noRd
.makeUniqueNames <- function(x) {

	if (any(duplicated(names(x)))) {
		names(x) <- make.unique(x@names)
	} else {
		x
	}
	x

}

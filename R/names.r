#' Name(s) of a GRaster or fields of a GVector
#'
#' Names(s) of a `GRaster` or of fields of a `GVector`.
#'
#' @param x A `GRaster` or `GVector`.
#' @param value Character: Name(s) to assign to the raster(s).
#' 
#' @details `GRaster`s have two types of names, a "sources", which is used internally to point to the **GRASS** representation of the raster (this is not of use to most users and is hidden by default), and an "rname", which is taken from the "**R**" name of the raster or the filename. This function returns the "rname".
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

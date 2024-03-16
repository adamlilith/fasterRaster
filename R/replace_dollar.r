#' Replace a raster layer or a column from a vector's data table
#'
#' @description The `$<-` notation can be used to replace a specific layer in a multi-layer `GRaster`, or a to replace a specific column from a `GVector`'s data table.
#'
#' @param x A `GRaster` or `GVector`.
#' @param name Character: Name of the `GRaster` layer to replace, or name of the `GVector` column to replace.
#' @param value Character: The name of a `GRaster` layer or the name of a column in a `GVector`'s data table. Names of rasters and vector tables' columns cab be obtained using [names()].
#'
#' @returns A `GRaster` or the column of a `GVector`.
#'
#' @example man/examples/ex_GRaster_GVector_subset_assign.r
#'
#' @seealso [$], \code{\link[fasterRaster]{[[<-}}, and \code{\link[fasterRaster]{add<-}}
#'
#' @name $<-
#' @aliases $<-,GRaster-method
#' @docType methods
#' @rdname replace_dollar
#' @exportMethod $<-
methods::setMethod(
    "$<-",
    signature = c(x = "GRaster"),
    function(x, name, value) {

	if (is.logical(name)) {
		if (length(name) < nlyr(x)) name <- rep(name, length.out = nlyr(x))
		i <- which(name)
	} else if (is.character(name)) {
		i <- match(name, names(x))
	}

	x[[name]] <- value
	names(x)[i] <- names(value)
	x

	} # EOF
)

#' @name $<-
#' @aliases $<-,GVector-method
#' @docType methods
#' @rdname replace_dollar
#' @exportMethod $<-
methods::setMethod(
	"$<-",
	signature = c(x = "GVector"),
	function(x, name, value) {

	ng <- ngeom(x)
	if (!is.null(value)) {
		lvalue <- length(value)
		if (lvalue < ng) value <- rep(value, length.out = ng) 
	}
	x@table <- data.table::set(x@table, i = NULL, j = name, value = value)
	x

	} # EOF
)

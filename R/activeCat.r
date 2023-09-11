#' Get or set the column with category labels in a categorical raster
#'
#' @description `GRaster`s can represent [categorical data][tutorial_raster_data_types]. Cell values are actually integers, each corresponding to a category, such as "desert" or "wetland." A categorical raster is associated with a table that matches each value to a category name. The table must be `NULL` (i.e., no categories--so not a categorical raster), or have at least two columns. The first column must have integers and represent raster values. One or more subsequent columns must have category labels. Which column corresponds to category labels can be seen using `activeCat()` and set using `activeCat()<-`.
#'
#' @param x A categorical `GRaster`.
#' 
#' @param layer Numeric, integer, logical, or character: Indicates for which layer(s) to get or set the active category column. This can be a number (the index of the raster(s)), a logical vector (`TRUE` ==> get/set the active category column, `FALSE` ==> leave as-is), or a character vector (names of layers).
#'
#' @param names Logical: If `TRUE`, display the name(s) of the active column(s). If `FALSE` (default), report the index of the active column. Following [terra::activeCat()], the first column in the levels table is ignored. So, an active column of "1" means the second column is active. "2" means the third column is active, and so on.
#'
#' @param value Numeric, integer, or character. Following [terra::activeCat()], the first column in each levels table is ignored. So, if you want the second column to be the category label, the use 1. If you want the third column, use 2, and so on. You can also specify the active column by its column name (though this can't be the first column).
#'
#' @returns `activeCat()` returns an integer or character vector of active column indices or names. `activeCat()<-` returns a `GRaster`.
#'
#' @example man/examples/ex_GRaster_categorical.r
#'
#' @aliases activeCat
#' @rdname activeCat
#' @exportMethod activeCat
methods::setMethod(
	f = "activeCat",
	signature = c(x = "GRaster"),
	function(x, layer = 1, names = FALSE) {
	
	layer <- .layerIndex(layer, x, recycle = TRUE)

	if (names) {

		ac <- x@activeCat
		out <- rep(NA_character_, length(layer))
		numLevels <- nlevels(x)
		for (i in layer) {
		
			if (numLevels[i] > 0L) {
				out[i] <- names(x@levels[[i]])[ac[i]]
			}

		}
	
	} else {
		out <- x@activeCat[layer] - 1L
	}
	names(out) <- names(x)[layer]
	out

	} # EOF
)

# # # #' @importFrom terra activeCat
# # # #' @export
# # # terra::activeCat

#' @aliases activeCat<-
#' @rdname activeCat
#' @exportMethod activeCat<-
methods::setMethod(
	f = "activeCat<-",
	signature = c(x = "GRaster", value = "numeric"),
	function(x, layer = 1, value) {
		.activeCatAssign(x = x, layer = layer, value = value)
	} # EOF
)

#' @aliases activeCat<-
#' @rdname activeCat
#' @exportMethod activeCat<-
methods::setMethod(
	f = "activeCat<-",
	signature = c(x = "GRaster", value = "integer"),
	function(x, layer = 1, value) {
		.activeCatAssign(x = x, layer = layer, value = value)
	} # EOF
)

#' Worker function for activeCat<-
#'
#' @param x A `GRaster`.
#' @param layer Numeric, integer, character, logical.
#' @param value Numeric or integer.
#'
#' @returns A `GRaster`.
#'
#' @noRd
.activeCatAssign <- function(x, layer, value) {

	layer <- .layerIndex(layer, x, recycle = TRUE)

	facts <- is.factor(x)
	if (any(!(which(facts) %in% layer))) stop("At least one layer is not categorical.\n  The active category cannot be set for this layer.")

	if (is.character(value)) {
		levs <- levels(x)
		for (i in layer) {
			value[i] <- match(value[i], names(levs[[i]]))
		}
	} else {
		value <- value + 1L
	}

	value <- as.integer(value)
	x@activeCat[layer] <- value
	methods::validObject(x)
	x

}

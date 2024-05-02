#' Get or set the column with category labels in a categorical raster
#'
#' @description These functions return or set the column of the labels to be matched to each value in the raster of a [categorical][tutorial_raster_data_types] `GRaster`. *Important*: Following [terra::activeCat()], the first column in the "levels" table is ignored, so an "active category" value of 1 means the second column is used as labels, a value of 2 means the third is used, and so on.
#' * `activeCat()` returns the column of the labels to be matched to each value in the raster for a single raster layer.
#' * `activeCats()` does the same, but for all layers of a `GRaster`.
#' * `activeCat()<-` sets the column to be used as category labels.
#'
#' @param x A categorical `GRaster`.
#' 
#' @param layer Numeric, integer, logical, or character: Indicates for which layer(s) to get or set the active category column. This can be a number (the index of the raster(s)), a logical vector (`TRUE` ==> get/set the active category column, `FALSE` ==> leave as-is), or a character vector (names of layers).
#'
#' @param names Logical: If `TRUE`, display the name(s) of the active column(s). If `FALSE` (default), report the index of the active column. Following [terra::activeCat()], the first column in the levels table is ignored. So, an active column of "1" means the second column is active. "2" means the third column is active, and so on.
#'
#' @param value Numeric, integer, or character. Following [terra::activeCat()], the first column in each levels table is ignored. So, if you want the second column to be the category label, use 1. If you want the third column, use 2, and so on. You can also specify the active column by its column name (though this can't be the first column's name).
#'
#' @returns `activeCat()` returns an integer or character of the active column index or name. `activeCats()` returns a vector of indices or names. `activeCat()<-` returns a `GRaster`.
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

#' @aliases activeCats
#' @rdname activeCat
#' @exportMethod activeCats
methods::setMethod(
	f = "activeCats",
	signature = c(x = "GRaster"),
	function(x, names = FALSE) {
	
	n <- nlyr(x)
	out <- rep(NA, n)
	for (i in seq_len(n)) out[i] <- activeCat(x, layer = i, names = names)
	out


	# out <- x@activeCat - 1L
	# if (names) {

	# 	ac <- out + 1L
	# 	out <- rep(NA_character_, length(layer))
	# 	numLevels <- nlevels(x)
	# 	for (i in layer) {
		
	# 		if (numLevels[i] > 0L) {
	# 			out[i] <- names(x@levels[[i]])[ac[i]]
	# 		}

	# 	}
	
	# } else {
	# 	out <- out - 1L
	# }
	# names(out) <- names(x)[layer]
	# out

	} # EOF
)

#' @aliases activeCat<-
#' @rdname activeCat
#' @exportMethod activeCat<-
methods::setMethod(
	f = "activeCat<-",
	signature = c(x = "GRaster"),
	function(x, layer = 1, value) {
	
	.activeCatAssign(x = x, layer = layer, value = value)
	
	} # EOF
)

#' @aliases activeCat<-
#' @rdname activeCat
#' @exportMethod activeCat<-
methods::setMethod(
	f = "activeCat<-",
	signature = c(x = "GRaster"),
	function(x, layer = 1, value) {
	
	.activeCatAssign(x = x, layer = layer, value = value)
	
	} # EOF
)

#' @aliases activeCat<-
#' @rdname activeCat
#' @exportMethod activeCat<-
methods::setMethod(
	f = "activeCat<-",
	signature = c(x = "GRaster"),
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

#' Minimum and maximum values or categories of a GRaster
#'
#' @description `minmax()` reports the minimum and maximum values across all non-NA cells of a `GRaster`. When the `cats` argument is `TRUE` and the raster is categorical, the function` reports the "lowest" and "highest" category values in a [categorical raster][tutorial_raster_data_types].
#'
#' @param x A `GRaster`.
#'
#' @return `minmax()` returns a numeric matrix, and `minmax(..., cats = TRUE)` returns a `data.frame` with category names. In the latter case, non-categorical rasters will have `NA` values.
#' 
#' @seealso [terra::minmax()]
#' 
#' @example man/examples/ex_GRaster.r
#'
#' @aliases minmax
#' @rdname minmax
#' @exportMethod minmax
setMethod(
	f = "minmax",
	signature = "GRaster",
	definition = function(x) matrix(c(x@minVal, x@maxVal), nrow=2, byrow=TRUE, dimnames=list(c("min", "max"), names(x)))
)

#' @noRd
methods::setMethod(
	f = ".minVal",
	signature = c(x = "GRaster"),
	function(x) x@minVal
)

#' @noRd
methods::setMethod(
	f = ".maxVal",
	signature = c(x = "GRaster"),
	function(x) x@maxVal
)

#' @aliases minmaxCat
#' @rdname minmax
#' @exportMethod minmaxCat
setMethod(
    f = "minmaxCat",
    signature = "GRaster",
    definition = function(x) {
	
	numLevels <- nlevels(x)
	for (i in seq_len(nlyr(x))) {

		if (numLevels[i] > 0L) {
   			
			thisMin <- x@levels[[i]][[x@activeCat[i]]][1L]
        	thisMax <- x@levels[[i]][[x@activeCat[i]]][numLevels[i]]

			this <- data.frame(TEMPTEMP_ = c(thisMin, thisMax), row.names = c("min", "max"))
		   	
			names(this) <- names(x)[i]

		} else {

			this <- data.frame(TEMPTEMP_ = c(NA_character_, NA_character_), row.names = c("min", "max"))
			
			names(this) <- names(x)[i]

		}

		if (i == 1L) {
			out <- this
		} else {
			out <- cbind(out, this)
		}

	} # next raster
	out

	} # EOF
)

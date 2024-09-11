#' Minimum and maximum values or categories of a GRaster
#'
#' @description `minmax()` reports the minimum and maximum values across all non-NA cells of a `GRaster`. When the `levels` argument is `TRUE` and the raster is categorical, the function reports the "lowest" and "highest" category values in a [categorical raster][tutorial_raster_data_types].
#'
#' @param x A `GRaster`.
#'
#' @param levels Logical: If `TRUE` and the raster is a categorical raster, return the "lowest" and "highest" categories. The default is `FALSE`.
#'
#' @return `minmax()` returns a numeric matrix, and `minmax(..., levels = TRUE)` returns a `data.frame` with category names. In the latter case, non-categorical rasters will have `NA` values.
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
	definition = function(x, levels = FALSE) {
	
	if (!levels) {
	
		out <- matrix(c(x@minVal, x@maxVal), nrow = 2, byrow = TRUE, dimnames = list(c("min", "max"), names(x)))

	} else {

		numLevels <- nlevels(x)
		for (i in seq_len(nlyr(x))) {

			if (numLevels[i] > 0L) {
				
				ac <- x@activeCat[[i]]
    			cats <- x@levels[[i]][ , ..ac]
				cats <- cats[[1L]]
				thisMin <- cats[1L]
				thisMax <- tail(cats, 1L)

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

	}
	out

	} # EOF
)


#' Get minimum value from GRaster metadata
#' @param x A `GRaster`.
#' @keywords internal
.minVal <- function(x) x@minVal

#' Get maximum value from GRaster metadata
#' @param x A `GRaster`.
#' @keywords internal
.maxVal <- function(x) x@maxVal

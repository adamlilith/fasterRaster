#' Get/create inrastName argument
#'
#' Retrieves or generates \code{inRastName}, which are names for each input raster.
#'
#' @param inRastName Either \code{NULL}, or a character vector, one per layer in \code{rast}.
#' @param rast A \code{SpatRaster} object with one or more layers.
#'
#' @return A character vector.
#'
#' @keywords internal
.getInRastName <- function(inRastName, rast) {

	# number of rasters
	if (inherits(rast, 'character')) {
		n <- length(rast)
	} else {
		rast <- terra::rast(rast)
		n <- terra::nlyr(rast)
		rastNames <- names(rast)
	}

	if (is.null(inRastName)) {

		# if rast is a raster
		if (inherits(rast, 'character')) {
			inRastName <- rast
		} else {
			inRastName <- if (any(is.null(rastNames)) || any(is.na(rastNames)) || any(rastNames == '')) {
				paste0('rast', 1L:n)
			} else {
				rastNames
			}
		}
	
	}
	
	if (length(inRastName) != n) stop('The number of names in "inRastName" is not the same as the number of layers in this raster.')

	inRastName

}

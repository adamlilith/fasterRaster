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

	# rast is a raster
	if (!inherits(rast, 'character')) {
	
		if (is.null(inRastName)) {
		
			n <- terra::nlyr(rast)
			rastNames <- names(rast)
			inRastName <- if (any(is.null(rastNames)) || any(is.na(rastNames)) || any(rastNames == '')) {
				paste0('rast', 1L:n)
			} else {
				rastNames
			}
		
		}
		
		if (length(inRastName) != n) stop('The number of names in "inRastName" is not the same as the number of layers in this raster.')
	
	# rast is a character
	} else {
		n <- length(rast)
		if (any(is.null(inRastName))) inRastName <- paste0('rast', 1L:n)
	}
	
	inRastName

}

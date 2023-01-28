#' Get raster or vector indicator
#'
#' Private. Used to determine if user wants rasters and/or vectors.
#'
#' @param rastOrVect Either 'rasters', 'vectors', or sometimes both.
#' @param n Maximum number of valid values for rastOrVect. If the length of rastOrVect is greater than "n", and error will be thrown.
#' @param nullOK If TRUE, then NULL values for rasteOrVect are OK.
#'
#' @return Character vector.
#'
#' @keywords internal
.getRastOrVect <- function(rastOrVect, n, nullOK) {

	if (length(rastOrVect) > n) stop(paste0('Argument "rastOrVect" can only have up to ', n, ' values.'))
	if (any(is.null(rastOrVect)) & !nullOK) {
		stop('Argument "rastOrVect" cannot be NULL.')
	} else if (any(is.null(rastOrVect)) & nullOK) {
		out <- NULL
	} else {

		out <- character()
		for (i in seq_along(rastOrVect)) {
			
			match <- pmatch(rastOrVect[i], c('rasters', 'vectors'))
			
			if (is.na(match)) {
				stop(paste0('No match found for value ', rastOrVect[i], ' in argument "rastOrVect".'))
			} else if (match == 1L) {
				out <- c(out, 'raster')
			} else if (match == 2L) {
				out <- c(out, 'vector')
			}
		
		}
		
		if (length(out) == 0L) {
			if (n == 1L) {
				stop('Argument "rastOrVect" must be "raster" or "vector".')
			} else {
				stop('Argument "rastOrVect" must be "raster" and/or "vector".')
			}
		}
	}
	
	out
	
}

#' Is there a raster or vector of the given name in the active 'GRASS' session?
#'
#' Indicates if a spatial object of the given class exists in the active \code{GRASS} session.
#'
#' @param x Name(s) of the object(s).
#' @param rastOrVect The type of object(s) of \code{x}. This can be \code{'raster'} and/or \code{'vector'}, or \code{NULL}.
#'
#' @return Logical for each value of \code{x}.
#'
#' @seealso \code{\link{fasterLs}} in \pkg{fasterRaster}
#'
#' @example man/examples/ex_fasterInfo.r
#'
#' @export

fasterExists <- function(x, rastOrVect = c('raster', 'vector')) {

	rastOrVect <- .getRastOrVect(rastOrVect, n=2, nullOK=TRUE)
	
	spatials <- fasterLs(rastOrVect = unique(rastOrVect))
	
	out <- x %in% spatials
	
	if (any(out)) {
	
		matchTypes <- names(spatials)

		if (length(rastOrVect) == 1L) {
			out <- out & rep(rastOrVect, length(x)) %in% matchTypes
		} else if (length(rastOrVect) == 2L) {
			out <- out &
				(rep(rastOrVect[1L], length(x)) %in% matchTypes | rep(rastOrVect[2L], length(x)) %in% matchTypes)
		} else {
			stop('Argument "rastOrVect" can only accept one or two values.')
		}
		
	}

	out

}

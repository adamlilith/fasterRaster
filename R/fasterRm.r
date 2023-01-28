#' Remove rasters/vectors from a GRASS session
#'
#' Remove rasters or vectors from a \code{GRASS} session. This will remove them from \code{GRASS}, but not from R if they exists in R.
#'
#' @param x Name(s) of the rasters and/or vectors to remove. If this is \code{'*'}, then \emph{everything} will be removed.
#' @param pattern If \code{TRUE}, then \code{x} is used as a pattern so that any rasters or vectors matching this pattern will be removed. For example, if an existing \code{GRASS} session has rasters named \code{bio1}, \code{bio2}, and \code{bio3}, plus a vector named \code{bioVector}, then this command will remove them all: \code{fasterRm('bio', pattern = TRUE)}. By default this is \code{FALSE}, so the value(s) in \code{x} must be exact.
#' @param rastOrVect The type of object to delete: either \code{'rasters'} and/or \code{'vectors'}. If \code{NULL} (default), then the function will attempt to guess whether \code{x} refers to a raster or vector. However, in \code{GRASS}, it is possible to have a raster and a vector of the same name. If this is the case, then you can specify whether \code{x} is a raster or vector (partial matching is supported).
#'
#' @return Invisibly returns the number of rasters and/or vectors removed. More notably, this function also removes rasters and/or vectors from a \code{GRASS} session.
#'
#' @seealso \code{\link{fasterLs}}, \href{https://grass.osgeo.org/grass82/manuals/g.remove.html}{\code{g.remove}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterOperations.r
#'
#' @export
fasterRm <- function(
	x,
	pattern = FALSE,
	rastOrVect = c('raster', 'vector')
) {
	
	flags <- c('quiet', 'f')
	
	rastOrVect <- .getRastOrVect(rastOrVect, n=2, nullOK=FALSE)
	spatials <- fasterLs(rastOrVect=rastOrVect, temps=TRUE)
	numFilesStart <- length(spatials)
	
	if (autoRegion) on.exit(regionExt('*'), add=TRUE) # resize extent to encompass all spatials
	
	# if any spatials
	if (length(spatials) > 0L) {

		if (length(x) == 1L && x == '*') {
			x <- spatials
		} else if (pattern) {
		
			theseSpatials <- character()
			for (i in seq_along(x)) {
			
				n <- nchar(x[i])
				if (any(x[i] == substr(spatials, 1L, n))) {
					matches <- spatials[x[i] == substr(spatials, 1L, n)]
					if (names(matches) %in% rastOrVect) {
						theseSpatials <- c(theseSpatials, matches)
					}
				}
			}
			
			x <- theseSpatials
			if (length(x) == 0L) warning('No rasters or vectors with this matching pattern found.')
			
		}

		if (length(x) == 0L) {
			warning('No rasters or vectors found. No files have been deleted.')
		} else {
		
			for (i in seq_along(x)) {
			
				thisSpatial <- spatials[spatials %in% x[i]]
				spatialType <- names(spatials)[i]
				
				if (spatialType %in% rastOrVect) {
					rgrass::execGRASS('g.remove', flags=c('quiet', 'f'), type=spatialType, name=thisSpatial)
				}
			
			}
		
		}
		
	}
	
	# resize region to encompass all
	numFilesEnd <- length(fasterLs(rastOrVect=rastOrVect, temps=TRUE))
	
	out <- numFilesStart - numFilesEnd
	invisible(out)

}

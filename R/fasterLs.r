#' Names of all rasters and/or spatial vectors in a GRASS session
#'
#' Displays the names of all rasters and/or vectors that have been exported to or created in a GRASS session.
#'
#' @param rastOrVect The type of spatial objects to display. This can include \code{'rasters'} (all rasters) and/or \code{'vectors'} (all spatial vectors). Partial matching is supported.
#' @param temps If \code{TRUE}, then remove any temporary files from the results. By default, these are not returned as part of the results. Some \pkg{fasterRaster} functions create temporary rasters or vectors which begin with the string "\code{TEMPTEMP_}".
#' 
#' @return Nothing (displays names of rasters and/or vectors).
#'
#' @seealso \code{\link{fasterInfoRast}}, \code{\link{fasterInfoVect}}, \href{https://grass.osgeo.org/grass82/manuals/g.list.html}{\code{g.list}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterInfo.r
#'
#' @export

fasterLs <- function(
	rastOrVect = c('rasters', 'vectors'),
	temps = FALSE
) {
	
	flags <- .getFlags(replace = FALSE)
	if (is.null(rastOrVect)) rastOrVect <- c('rasters', 'vectors')
	rastOrVect <- .getRastOrVect(rastOrVect, n=2, nullOK=FALSE)
	n <- nchar(rastOrVect)

	# fetch names
	rasts <- vects <- character()
	if (any(rastOrVect == 'raster')) {
	
		rasts <- rgrass::execGRASS('g.list', flags=flags, type='raster', intern=TRUE, echoCmd=FALSE)
		if (length(rasts) > 0L) names(rasts) <- rep('raster', length(rasts))
	
	}

	if (any(rastOrVect == 'vector')) {
	
		vects <- rgrass::execGRASS('g.list', flags=flags, type='vector', intern=TRUE, echoCmd=FALSE)
		if (length(vects) > 0L) names(vects) <- rep('vector', length(vects))
	
	}
	
	out <- c(rasts, vects)
	if (!temps) {
		keeps <- !grepl(out, pattern='TEMPTEMP_')
		out <- out[keeps]
	}
	
	out

}

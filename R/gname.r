#' Name for a raster or vector in a 'GRASS' session
#'
#' Retrieves the name of a raster or vector in **GRASS**.
#'
#' @param x The object for which to obtain the name: a `GRaster`, `GVector`, or character. If a character, then the character itself is returned.
#' @param n Number of names to make.
#'
#' @noRd
# if (!isGeneric('.gname')) setGeneric(name='.gname', def=function(x) { standardGeneric('.gname') })

setMethod(
	f = '.gname',
	signature = 'GSpatial',
	definition = function(x) x@gname
)
setMethod(
	f = '.gname',
	signature = 'character',
	definition = function(x) x
)
.makeGname <- function(x = NULL, rastOrVect = NULL, n = 1L) {

	if (is.null(x) & is.null(rastOrVect)) stop('Both <x> and <rastOrVect> cannot be NULL at the same time.')

	rname <- ''
	if (!is.null(x) && inherits(x, c('SpatRaster', 'stars'))) {
		rastOrVect <- 'rast'
		rname <- names(x)
		n <- terra::nlyr(x)
	} else if (!is.null(x) && inherits(x, c('SpatVector', 'sf'))) {
		rastOrVect <- 'vect'
	} else {
		rastOrVect <- .pmatch(rastOrVect, c('raster', 'vector', 'group'))
	}

	if (rastOrVect == 'raster') rastOrVect <- 'rast'
	if (rastOrVect == 'vector') rastOrVect <- 'vect'
	if (rastOrVect == 'group') rastOrVect <- 'group'

	fname <- tempfile()
	fname <- basename(fname)
	fname <- sub(fname, pattern='file', replacement='')
	if (n > 1L) fname <- paste0(fname, '_', 1L:n)
	gname <- if (rname[1L] != '') {
		paste0(rastOrVect, '_', rname, '_', fname)
	} else {
		paste0(rastOrVect, '_', fname)
	}
	gname

}

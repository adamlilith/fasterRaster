#' Make 'GRASS' name for a raster or vector
#'
#' @param x The object for which to make the name. If `NULL`, we assume that the object is to be made (it does not yet exist in `GRASS`). We should define `rastOrVect` in these cases.
#' @param rastOrVect 'raster' or 'vector'
#' @param n Number of names to make.
#'
#' @keywords internal

.makeGname <- function(x = NULL, rastOrVect = NULL, n = 1L) {
# .makeGname <- function(x = NULL, n = 1L) {

	if (is.null(x) & is.null(rastOrVect)) stop('Both <x> and <rastOrVect> cannot be NULL at the same time.')

	rname <- ''
	if (!is.null(x) && inherits(x, c('SpatRaster', 'stars'))) {
		rastOrVect <- 'rast'
		rname <- names(x)
		n <- terra::nlyr(x)
	} else if (!is.null(x) && inherits(x, c('SpatVector', 'sf'))) {
		rastOrVect <- 'vect'
	} else {
		rastOrVect <- .pmatch(rastOrVect, c('raster', 'vector'))
	}

	if (rastOrVect == 'raster') rastOrVect <- 'rast'
	if (rastOrVect == 'vector') rastOrVect <- 'vect'

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

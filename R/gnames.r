#' Name of a raster or vector in a GRASS session
#'
#' @description `gnames()` retrieves the name of a raster or vector in **GRASS**. `GRaster`s and `GVector`s are actually pointers to objects stored in a **GRASS* database. When using **fasterRaster** functions on rasters and vectors, the commands are translated into **GRASS** commands and executed on the objects named in the pointers. These objects use a "gname" (which is really a filename) to refer to the **GRASS** objects. In select cases, it can help to get the "gnames" of a `GRaster` or `GVector`. This function is not of use to most users.
#'
#' @param x Either a `GSpatial` object or one that inherits from it (i.e., a `GRaster` or `GVector`), *or* a character. If a character, then the character itself is returned.
#'
#' @returns Character.
#'
#' @example man/examples/ex_GRaster.r
#' 
#' @aliases gnames
#' @rdname gnames
#' @export
#' @exportMethod gnames
methods::setMethod(
	f = 'gnames',
	signature = 'GSpatial',
	definition = function(x) x@gnames
)

#' @rdname gnames
#' @aliases gnames
#' @exportMethod gnames
methods::setMethod(
	f = 'gnames',
	signature = 'character',
	definition = function(x) x
)

#' Make gnames
#'
#' @param rastOrVect Character: `raster`, `raster3D`, or `vector`
#' @param n Numeric integer: Number of names to make
#' @noRd
.makeGname <- function(x = NULL, rastOrVect = NULL, n = 1L) {

	if (is.null(x) & is.null(rastOrVect)) stop('Both ', sQuote('x'), ' and ', sQuote('rastOrVect'), ' cannot be ', dQuote('NULL'), ' at the same time.')

	rastOrVect <- tolower(rastOrVect)

	names <- ''
	if (!is.null(x) && inherits(x, c('SpatRaster', 'stars'))) {
		rastOrVect <- 'raster'
		names <- names(x)
		n <- terra::nlyr(x)
	} else if (!is.null(x) && inherits(x, c('SpatVector', 'sf'))) {
		rastOrVect <- 'vector'
	} else if (!is.null(x) && inherits(x, 'character')) {
		names <- x
	} else {
		rastOrVect <- tolower(rastOrVect)
		rastOrVect <- .pmatch(rastOrVect, c('raster', 'raster3d', 'vector', 'group', 'region'))
	}

	if (rastOrVect == 'raster3d') rastOrVect <- 'rast3d'
	if (rastOrVect %in% c('GRaster', 'raster')) rastOrVect <- 'rast'
	if (rastOrVect %in% c('GVector', 'vector')) rastOrVect <- 'vect'
	if (rastOrVect == 'group') rastOrVect <- 'group'
	if (rastOrVect == 'region') rastOrVect <- 'region'

	gn <- rstring(1L)
	if (n > 1L) gn <- paste0(gn, '_', 1L:n)
	gn <- if (names[1L] != '') {
		paste0(rastOrVect, '_', names, '_', gn)
	} else {
		paste0(rastOrVect, '_', gn)
	}
	gn

}

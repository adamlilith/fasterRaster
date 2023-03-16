#' Rename a raster or vector in an existing GRASS session
#'
#' Rename a raster or vector in an existing GRASS session.
#'
#' @inheritParams .sharedArgs_replace
#' @param from,to Names of the raster or vector to rename.
#' @param rastOrVect Either \code{NULL} (default), \code{'raster'}, or \code{'vector'}. This specifies the type of object to be renamed. Partial matching is allowed. If left as \code{NULL} (default), the function will try to identify if the object is a raster or vector, and return an error if there is both a raster and vector of given name. Note that unlike in \pkg{R}, \code{GRASS} can have rasters and vector's with the same name.
#' @param warn If \code{TRUE} (default), print a warning when renaming a raster to the name of an existing raster or vector (which overwrite it).
#' 
#' @return The function invisibly returns \code{TRUE} if the desired rasters and/or vectors were named, and \code{FALSE} if raster and/or vector to be renamed did not exist in the \code{GRASS} session. Notably, a raster or vector or both are renamed in an existing \code{GRASS} session.
#'
#' @seealso \code{\link{fasterLs}}, \code{\link{fasterRm}}, \href{https://grass.osgeo.org/grass82/manuals/g.rename.html}{\code{g.list}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterOperations.r
#'
#' @export
fasterRename <- function(
	from,
	to,
	rastOrVect = NULL,
	replace = fasterGetOptions('replace', FALSE),
	warn = TRUE
) {
	
	flags <- .getFlags(replace=replace)
	
	rastOrVect <- .determineRastOrVect(x=from, rastOrVect=rastOrVect, temps=TRUE)
	fromTo <- c(from, to)

	success <- FALSE

	# rename raster
	if (rastOrVect == 'raster') {
	
		spatials <- fasterLs('rasters')

		if (to %in% spatials) {
			if (!replace) {
				stop('This would replace an existing raster. Use a different name for "to" or set "replace" to TRUE.')
			} else {
				warning(paste0('Overwriting raster ', to, '.'))
			}
		}
		rgrass::execGRASS('g.rename', flags=flags, raster=fromTo, intern=TRUE)
		success <- TRUE
		
	} else if (rastOrVect == 'vector') {
	
		spatials <- fasterLs('vectors')

		if (to %in% spatials) {
			if (!replace) {
				stop('This would replace an existing vector. Use a different name for "to" or set "replace" to TRUE.')
			} else {
				warning(paste0('Overwriting vector ', to, '.'))
			}
		}
		rgrass::execGRASS('g.rename', flags=flags, vector=fromTo, intern=TRUE)
		success <- TRUE
	
	}

	invisible(success)

}

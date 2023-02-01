#' Make a copy of a raster or vector
#'
#' Make a copy of a raster or vector in an active \code{GRASS} session.
#'
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_autoRegion
#'
#' @param from Name of the raster or vector to copy.
#' @param to New name for the raster or vector.
#' @param rastOrVect The type of object named in \code{from}. This should be either \code{'raster'} or \code{'vector'} (partial matching is supported). Typically, this does not need to be defined if there is just one object of the given name. However, \code{GRASS} allows for rasters and vectors of the same name, so if the object named in \code{from} is ambiguous, then this should be defined.
#'
#' @return \code{TRUE} (invisibly). Also copies a raster or vector in the active \code{GRASS} session.
#'
#' @seealso \code{\link{fasterRename}} and \code{\link{fasterLs}} in \pkg{fasterRaster}; \code{GRASS} module \href{https://grass.osgeo.org/grass82/manuals/g.copy.html}{g.copy}
#'
#' @examples /man/examples/ex_fasterOperations.r
#'
#' @export

fasterCopy <- function(
	from,
	to,
	rastOrVect = NULL,
	replace = fasterGetOptions('replace', FALSE),
	autoRegion = fasterGetOptions('autoRegion', TRUE)
) {

	### begin common
	flags <- .getFlags(replace=replace)
	### end common

	if (from == to) stop('Arguments "from" and "to" must be different.')
	
	# detect conflicts (a raster and vector of the same name)
	rastOrVect <- .isRastOrVect(x=from, rastOrVect=rastOrVect, errorNotFound=TRUE, errorAmbig=TRUE, temps=TRUE)
	spatials <- fasterLs(rastOrVect=rastOrVect)

	if (!replace) {
		if (any(to %in% spatials)) stop('There is already an object with the name given in "to".\nEither use a different name or set "replace" to TRUE.')
	}

	if (is.null(rastOrVect)) {
		if (sum(from %in% spatials) > 1L) stop('Both a raster and vector have the name given in "from". Please use argument "rastOrVect".')
		rastOrVect <- names(spatials)[spatials %in% from]
	}

	# resize region
	if (autoRegion & names(spatials[spatials %in% from]) == 'raster') {
		on.exit(regionExt('*'), add=TRUE) # resize extent to encompass all spatials
		regionReshape(from)
	}

	# copy raster
	fromTo <- paste0(from, ',', to)
	if (pmatch(rastOrVect, c('raster', 'vector')) == 1L) {
		rgrass::execGRASS('g.copy', raster=fromTo, flags=flags)
	} else if (pmatch(rastOrVect, c('raster', 'vector')) == 2L) {
		rgrass::execGRASS('g.copy', vector=fromTo, flags=flags)
	} else {
		stop('Cannot find the object of the stated type. See argument "vectOtRast".')
	}

	invisible(TRUE)

}

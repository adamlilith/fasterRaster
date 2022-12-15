#' Information on a raster or vector in a GRASS session
#'
#' Displays information on a raster or vector already in an R session. For further
#'
#' @param name The name of the raster or vector in the R session. This must be in quotes.
#' @param flags Either \code{NULL} (default) or flags to affect the information shown. Flags should be in quotes and without the preceeding "-" sign (example: \code{flags = 'e'}).
#' @return Nothing (displays information on the raster or vector).
#'
#' @examples
#'
#' \donttest{
#'
#' rastFile <- system.file('extdata', 'madElev.tif', package='fasterRaster')
#' madElev <- rast(rastFile)
#'
#' # start a GRASS session
#' initGrass(madElev, rastName = 'elev')
#' fasterInfoRast('madElev')
#'
#' # start a GRASS session
#' exportVectToGrass(madCoast0, vectName = 'madCoast0')
#' fasterInfoVect('madCoast0')
#'
#' }
#'
#' @export
fasterInfoRast <- function(rastName, flags = NULL) {
	rgrass::execGRASS('r.info', map=name, flags=NULL)
}

#' @name fasterInfoVect
#' @title Information on a raster or vector in a GRASS session
#' @rdname fasterInfoRast
#' @export
fasterInfoVect <- function(rastName, flags = NULL) {
	rgrass::execGRASS('v.info', map=name, flags=NULL)
}

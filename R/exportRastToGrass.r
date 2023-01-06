#' Export raster(s) to an existing GRASS session
#'
#' Export one or more rasters to an existing \code{GRASS} session.
#'
#' @inheritParams .sharedArgs_rast_plural
#' @inheritParams .sharedArgs_inRastName_plural
#'
#' @return \code{TRUE} (invisibly, if the raster was successfully exported). The function also exports a raster to a \code{GRASS} session so it can be used by other functions.
#'
#' @seealso \code{\link[rgrass]{write_RAST}} in \pkg{rgrass}
#'
#' @examples man/examples/ex_exportRastToGrass.R
#'
#' @export
exportRastToGrass <- function(
	rast,
	inRastName = NULL
) {

	if (!inherits(rast, 'SpatRaster')) rast <- terra::rast(rast)
	inRastName <- .getInRastName(inRastName, rast)

	# NB writing first raster in a stack actualy writes all of them
	suppressMessages(rgrass::write_RAST(rast[[1L]], vname=inRastName[1L], flags=c('quiet', 'overwrite')))
	
	# if multi-layer raster, rasters are imported using the first name plus a ".#" where # is a number, so they need renamed
	n <- terra::nlyr(rast)
	if (n > 1L) {
	
		baseName <- inRastName[1L]
	
		for (i in 1L:n) {
		
			from <- paste0(baseName, '.', i)
			to <- inRastName[i]
			fasterRename(from=from, to=to, type='raster')
		
		}
	
	}
	
	invisible(TRUE)

}

#' Export a spatial vector to an existing GRASS session
#'
#' Export a spatial vector to an existing \code{GRASS} session.
#'
#' @param vect A \code{SpatVector} (\pkg{terra} package) or \code{sf} (\pkg{sf} package) spatial vector object.
#' @param inVectName Name of vector in \code{GRASS}. If \code{NULL} (default), the name will be set to \code{'vect'}.
#'
#' @return \code{TRUE} (invisibly, if the vector was successfully exported). The function also exports a vector to a \code{GRASS} session so it can be used by other functions.
#'
#' @seealso \code{\link[rgrass]{write_VECT}} in \pkg{rgrass}
#'
#' @examples man/examples/ex_exportRastToGrass.R
#'
#' @export
exportVectToGrass <- function(
	vect,
	inVectName = NULL
) {

	if (is.null(inVectName)) inVectName <- 'vect'

	if (!inherits(vect, 'SpatVector')) vect <- terra::vect(vect)
	suppressMessages(rgrass::write_VECT(vect, vname=inVectName, flags=c('quiet', 'overwrite')))
	invisible(TRUE)
	
}

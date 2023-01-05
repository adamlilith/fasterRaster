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
#' @examples man/examples/ex_exportXToGrass.R
#'
#' @export
exportRastToGrass <- function(
	rast,
	inRastName = NULL
) {

	if (!inherits(rast, 'SpatRaster')) rast <- terra::rast(rast)
	inRastName <- .getInRastName(inRastName, rast)
	if (length(inRastName) != terra::nlyr(rast)) stop('The number of names in "inRastName" is not the same as the number of layers in "rast."')
	
	n <- terra::nlyr(rast)
	for (i in 1L:n) {
		vname <- inRastName[[i]]
		thisRast <- terra::subset(rast, i)
		suppressMessages(rgrass::write_RAST(thisRast, vname=vname, flags=c('quiet', 'overwrite')))
	}
		
	invisible(TRUE)

}

#' Export a spatial vector to an existing GRASS session
#'
#' Export a spatial vector to an existing \code{GRASS} session.
#'
#' @param vect A \code{SpatVector} (\pkg{terra} package) or \code{sf} (\pkg{sf} package) spatial vector object.
#' @param inVectName Name of vector in \code{GRASS}. By default, this will be \code{'vect'}.
#'
#' @return \code{TRUE} (invisibly, if the vector was successfully exported). The function also exports a vector to a \code{GRASS} session so it can be used by other functions.
#'
#' @seealso \code{\link[rgrass]{write_VECT}} in \pkg{rgrass}
#'
#' @examples man/examples/ex_exportXToGrass.R
#'
#' @export
exportVectToGrass <- function(
	vect,
	inVectName = 'vect'
) {

	if (!inherits(vect, 'SpatVector')) vect <- terra::vect(vect)
	suppressMessages(rgrass::write_VECT(vect, vname=inVectName, flags=c('quiet', 'overwrite')))
	invisible(TRUE)
	
}

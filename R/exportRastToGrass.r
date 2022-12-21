#' Export raster to an existing GRASS session
#'
#' Export a raster to an existing \code{GRASS} session.
#'
#' @param rast A \code{SpatRaster} raster from the \pkg{terra} package.
#' @param inRastName What to name the raster in \pkg{GRASS}. By default, this will be the name of the raster (i.e., \code{names(rast)}).
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
	inRastName = ifelse(is.null(names(rast)), 'rast', names(rast))
) {

	if (!inherits(rast, 'SpatRaster')) rast <- terra::rast(rast)
	suppressMessages(rgrass::write_RAST(rast, vname=inRastName, flags=c('quiet', 'overwrite')))
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

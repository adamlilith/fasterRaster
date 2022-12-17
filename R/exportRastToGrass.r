#' Export raster to an existing GRASS session
#'
#' Export a raster to an existing \code{GRASS} session.
#'
#' @param rast A \code{SpatRaster} raster from the \pkg{terra} package.
#' @param grassName What to name the raster in \pkg{GRASS}.
#'
#' @return Nothing (exports a raster to a \code{GRASS} session so it can be used by other functions).
#'
#' @examples man/examples/ex_exportXToGrass.r
#'
#' @export
exportRastToGrass <- function(
	rast,
	grassName = 'rast'
) {

	if (!inherits(rast, 'SpatRaster')) rast <- terra::rast(rast)
	rgrass::write_RAST(rast, vname=grassName, overwrite=TRUE, verbose=FALSE)

}

#' Export a spatial vector to an existing GRASS session
#'
#' Export a spatial vector to an existing \code{GRASS} session.
#'
#' @param vect A \code{SpatVector} (\pkg{terra} package) or \code{sf} (\pkg{sf} package) spatial vector object.
#' @param grassName Name of vector in \pkg{GRASS}.
#'
#' @return Nothing (exports a vector to a \code{GRASS} session so it can be used by other functions).
#'
#' @examples man/examples/ex_exportXToGrass.r
#'
#' @export
exportVectToGrass <- function(
	vect,
	grassName = 'vect'
) {

	if (!inherits(vect, 'Spatvector')) vect <- terra::vect(vect)
	rgrass::write_VECT(vect, vname=grassName, v.in.ogr_flags='overwrite')
	
}

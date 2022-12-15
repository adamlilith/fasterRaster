#' Write raster to an existing GRASS session
#'
#' Write a raster to an existing GRASS session with handling for very big rasters.
#' @param rast A \code{SpatRaster} raster.
#' @param grassName What to name the raster in GRASS.
#' @return Nothing (exports a raster to a GRASS session so it can be used by other functions).
#'
#' @examples
#'
#' \donttest{
#'
#' rastFile <- system.file('extdata', 'madForest2000.tif', package='fasterRaster')
#' madForest2000 <- rast(rastFile)
#'
#' rastFile <- system.file('extdata', 'madElev.tif', package='fasterRaster')
#' madElev <- rast(rastFile)
#'
#' # start a GRASS session
#' initGrass(madForest2000, grassDir = grassDir)
#' exportRastToGrass(madElev, grassName = 'madElev', grassDir = grassDir)
#'
#' }
#'
#' @export
exportRastToGrass <- function(
	rast,
	grassName = 'rast'
) {

	if (inherits(rast, 'Raster')) rast <- terra::rast(rast)
	rgrass::write_RAST(rast, vname=grassName, overwrite=TRUE, verbose=FALSE)

}

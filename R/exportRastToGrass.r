#' Write raster to an existing GRASS session
#'
#' Write a raster to an existing GRASS session with handling for very big rasters.
#' @param rast Raster layer.
#' @param grassName Name of raster in GRASS.
#' @param tempDir Temporary directory.
#' @return Nothing (exports a raster to a GRASS session so it can be used by other functions).
#' @export
exportRastToGrass <- function(
	rast,
	grassName = 'rast',
	tempDir = raster::tmpDir()
) {

	rgrass7::use_sp()

	rastSGDF <- methods::as(rast, 'SpatialGridDataFrame')
	rgrass7::writeRAST(rastSGDF, vname=grassName, overwrite=TRUE)

}

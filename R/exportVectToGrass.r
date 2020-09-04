#' Write vector to an existing GRASS session
#'
#' Write a raster to an existing GRASS session with handling for very big rasters.
#' @param vect Raster layer.
#' @param grassName Name of vector in GRASS.
#' @param tempDir Temporary directory.
#' @return Nothing (exports a vector to a GRASS session so it can be used by other functions).
#' @export
exportVectToGrass <- function(
	vect,
	grassName = 'vect',
	tempDir = raster::tmpDir()
) {

	rgrass7::use_sp()
	rgrass7::writeVECT(vect, vname=grassName, v.in.ogr_flags='overwrite')
	
}

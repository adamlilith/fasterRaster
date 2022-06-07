#' Write vector to an existing GRASS session
#'
#' Write a raster to an existing GRASS session with handling for very big rasters.
#' @param vect Vector layer. This can be an object of class \code{Spatial} (\pkg{sp} package, so for example, a \code{SpatialPolygons} object), or \code{SpatVector} (\pkg{terra} package), or \code{sf} (\pkg{sf} package).
#' @param grassName Name of vector in GRASS.
#' @param tempDir Temporary directory.
#' @return Nothing (exports a vector to a GRASS session so it can be used by other functions).
#' @export
exportVectToGrass <- function(
	vect,
	grassName = 'vect',
	tempDir = raster::tmpDir()
) {

	if (inherits(vect, 'SpatVector')) vect <- sf::st_as_sf(vect)
	if (inherits(vect, 'sf')) vect <- as(vect, 'Spatial')

	rgrass7::use_sp()
	rgrass7::writeVECT(vect, vname=grassName, v.in.ogr_flags='overwrite')
	
}

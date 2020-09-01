#' Write raster to an existing GRASS session
#'
#' Write a raster to an existing GRASS session with handling for very big rasters.
#' @param rast Raster layer.
#' @param vname Name of raster in GRASS.
#' @param tempDir Temporary directory.
#' @return Nothing (exports a raster to a GRASS session so it can be used by other functions).
#' @export
exportRastToGrass <- function(
	rast,
	vname = 'rast',
	tempDir = raster::tmpDir()
) {

	rgrass7::use_sp()

	# # fast export
	# success <- tryCatch(.tryWrite(rast), error=function(err) return(FALSE))
	
	# # slow but error-resistant export
	# if (class(success) == 'logical' && !success) {

		# raster::writeRaster(rast, paste0(tempDir, '/', vname), format='GTiff', overwrite=TRUE)
		# rgrass7::execGRASS('r.import', input=paste0(tempDir, '/', vname, '.tif'), output=vname, flags=c('overwrite', 'quiet'))
		
	# }

	rastSGDF <- methods::as(rast, 'SpatialGridDataFrame')
	rgrass7::writeRAST(rastSGDF, vname=vname, overwrite=TRUE)

}

# .tryWrite <- function(rast, vname) {
	# rastSGDF <- methods::as(rast, 'SpatialGridDataFrame')
	# rgrass7::writeRAST(rastSGDF, vname=vname, overwrite=TRUE)
# }

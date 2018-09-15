#' Write raster to an existing GRASS session
#'
#' Write a raster to an existing GRASS session with handling for very big rasters.
#' @param x Raster layer.
#' @param vname Name of raster in GRASS.
#' @param tempDir Temporary directory.
#' @return Nothing (exports a raster to a GRASS session so it can be used by other functions).
#' @export
exportRastToGrass <- function(
	x,
	vname = 'rast',
	tempDir = raster::tmpDir()
) {

	tryWrite <- function(x, vname) {
		x <- as(x, 'SpatialGridDataFrame')
		rgrass7::writeRAST(rastSGDF, vname=vname)
	}

	# fast export
	success <- tryCatch(tryWrite(x), error=function(err) return(FALSE))
	
	# slow but error-resistant export
	if (class(success) == 'logical' && !success) {

		writeRaster(x, paste0(tempDir, '/', vname), format='GTiff', overwrite=TRUE)
		rgrass7::execGRASS('r.import', input=paste0(tempDir, '/', vname, '.tif'), output=vname, flags=c('overwrite', 'quiet'))
		
	}

}

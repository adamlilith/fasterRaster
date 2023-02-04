#' Checks whether the operation would overwrite existing rasters or vectors
#'
#' Private. SHOULD ONLY BE USED **AFTER** inRasteName or inVectName is parsed.
#'
#' @param replace TRUE or FALSE
#' @param inRastName,inVectName Name(s) of rasters/vectors to be exported to GRASS
#' @param outGrassName Name(s) of rasters/vectors to be created in GRASS
#'
#' @return TRUE (invisibly) or throws an error.
#'
#' @keywords internal
.checkRastExists <- function(replace, rast = NULL, inRastName = NULL, outGrassName = NULL) {

	if (!replace) {

		if (!inherits(rast, 'character')) {
		
			x <- inRastName
			alreadyExist <- fasterExists(x, rastOrVect='rasters')
			if (any(alreadyExist)) stop('These raster(s) already exist:\n', paste(paste('  * ', x[alreadyExist]), collapse='\n'), '\n  Either use different raster names or "inRastNames", or set "replace" = TRUE.')
		
		} else if (!is.null(rast)) {
		
			x <- rast
			alreadyExist <- fasterExists(x, rastOrVect='rasters')
			if (!any(alreadyExist)) stop('These raster(s) are not in GRASS yet:\n', paste(paste('  * ', x[!alreadyExist]), collapse='\n'), '\n  Export them to GRASS using initGrass(), fasterRast(), or another "faster" function.')

		}
		
		if (!is.null(outGrassName)) {
		
			x <- outGrassName
			alreadyExist <- fasterExists(x, rastOrVect='rasters')
			if (any(alreadyExist)) stop('These raster(s) already exist:\n', paste(paste('  * ', x[alreadyExist]), collapse='\n'), '\n  Either use different "outGrassNames", or set "replace" = TRUE.')
		
		}
	
	}
	
	invisible(TRUE)

}

.checkVectExists <- function(replace, vect = NULL, inVectName = NULL, outGrassName = NULL) {

	if (!replace) {

		if (!inherits(vect, 'character')) {
		
			x <- inVectName
			alreadyExist <- fasterExists(x, rastOrVect='vectors')
			if (any(alreadyExist)) stop('These raster(s) already exist:\n', paste(paste('  * ', x[alreadyExist]), collapse='\n'), '\n  Either use different "inRastNames" or set "replace" = TRUE.')
		
		} else if (!is.null(vect)) {
		
			x <- vect
			alreadyExist <- fasterExists(x, rastOrVect='vectors')
			if (!any(alreadyExist)) stop('These vector(s) are not in GRASS yet:\n', paste(paste('  * ', x[!alreadyExist]), collapse='\n'), '\n  Export them to GRASS using initGrass(), fasterVect(), or another "faster" function.')

		}
		
		if (!is.null(outGrassName)) {
		
			x <- outGrassName
			alreadyExist <- fasterExists(x, rastOrVect='vectors')
			if (any(alreadyExist)) stop('These vector(s) already exist:\n', paste(paste('  * ', x[alreadyExist]), collapse='\n'), '\n  Either use different "outGrassNames", or set "replace" = TRUE.')
		
		}
	
	}
	
	invisible(TRUE)

}

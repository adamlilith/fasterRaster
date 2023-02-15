#' Create a mask to affect subsequent operations on rasters
#'
#' This function creates a raster mask to be used for subsequent operations on rasters. A mask can be created from a raster or vector. If the mask is named "\code{MASK}" in the \code{GRASS} session (which it is by default), nearly all operations on other rasters will only operate cells that are not \code{NA} in the mask.To remove a mask, set the argument \code{removeMask} to \code{TRUE} when using this function, or use \code{\link{fasterRm}} or \code{\link{fasterRename}} on the \code{MASK} raster.\cr
#' 
#' This function is similar to \code{\link{fasterMask}}, except that this one only creates a mask. That function creates a mask and removes from a focal raster all cells that are \code{NA} in the mask.
#'
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_autoRegion
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_outGrassName
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_trimRast
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @param mask Either of:
#' \itemize{
#'	\item A \code{SpatRaster}, \code{SpatVector}, or \code{sf} object.
#'	\item The name of a raster or vector in the active \code{GRASS} session.
#' }
#' @param inMaskName The name of the raster or vector to be used as a mask. If not specified, and \code{mask} is a raster, then this will take the name of the raster if the name exists, or \code{'inputRast'} if not. If not specified and \code{mask} is a vector in \code{GRASS}, then this will become the name of the vector. If not specified and \code{mask} is a vector in \code{R}, then this will become "inputVect".
#'
#' @param rastOrVect Either \code{'raster'}, \code{'vector'}, or \code{NULL} (default). The type of object that \code{mask} is. This is typically not needed, unless \code{mask} is the name of an object in \code{GRASS} and there is more than one object of that name.
#'
#' @param clip If \code{TRUE} (default), then the mask will reflect the "shape" of the input object. That is, the \code{MASK} raster will have \code{NA} cells wherever the input object had \code{NA} cells (if input was a raster), or \code{NA} cells wherever the vector was not present (if a vector). If \code{FALSE}, then the entire extent of the input will become the mask (i.e., it will be rectangular).
#'
#' @return A \code{SpatRaster}. In the active \code{GRASS} session a raster is created, and possibly another raster named \code{MASK}.
#'
#' @example man/examples/ex_fasterMask.r
#'
#' @seealso \code{\link[terra]{mask}} in package \pkg{terra}; \code{GRASS} module }\href{https://grass.osgeo.org/grass82/manuals/r.mask.html}{\code{r.mask}}
#'
#' @export

fasterMakeMask <- function(
	mask,
	inMaskName,
	rastOrVect = NULL,
	clip = TRUE,
	
	outGrassName = 'MASK',
	
	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	trimRast = fasterGetOptions('trimRast', TRUE),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	# special... needed for "dots" in commons block
	if (missing(inMaskName)) inMaskName <- NULL

	### commons v1 -- modified
	##########################
		
		### flags
		flags <- .getFlags(replace=replace)
		
		### restore
		# on.exit(.restoreLocation(), add=TRUE) # return to starting location
		if (autoRegion) on.exit(regionExt('*'), add=TRUE) # resize extent to encompass all spatials

		### ellipses and initialization arguments
		initsDots <- .getInitsDots(..., callingFx = 'fasterMakeMask')
		inits <- initsDots$inits
		dots <- initsDots$dots

	###############
	### end commons

	### function-specific
	
	args <- list(
		cmd = 'r.mask',
		flags = flags
	)
	args <- c(args, dots)
	
	# arguments for "r.mask"
	.checkRastExists(replace=replace, rast=NULL, inRastName=NULL, outGrassName=outGrassName, ...)
	
	if (inherits(mask, 'SpatRaster')) {
		if (!inherits(mas, 'character') & !inherits(mas, 'SpatRaster')) mask <- terra::rast(mask)
		inMaskName <- .getInRastName(inMaskName, rast=mask)
		.checkRastExists(replace=replace, rast=rast, inRastName=inRastName, outGrassName=NULL, ...)
		rastOrVect <- 'raster'
		inits$rast <- mask
		inits$inRastName <- inMaskName
		args <- c(args, raster=inMaskName)
	} else if (inherits(mask, c('SpatVector', 'sf'))) {
		if (!inherits(mask, 'character') & !inherits(mask, 'SpatVector')) mask <- terra::vect(mask)
		inMaskName <- .getInVectName(inMaskName, vect=mask)
		.checkVectExists(replace=replace, vect=vect, inVectName=inVectName, outGrassName=NULL)
		rastOrVect <- 'vector'
		inits$vect <- mask
		inits$inVectName <- inMaskName
		args <- c(args, vector=inMaskName)
	} else if (inherits(mask, 'character')) {
		
		inMaskName <- mask
		rastOrVect <- .matchRastOrVect(rastOrVect, n=1, naOK=TRUE)
		if (is.na(rastOrVect)) rastOrVect <- .determineRastOrVect(x=mask, errorNotFound=TRUE, dupsOK=FALSE)
		
		if (rastOrVect == 'raster') {
			.checkRastExists(replace=replace, rast=mask, inRastName=inMaskName, outGrassName=NULL, ...)
			args <- c(args, raster=mask)
			inits$rast <- mask
			inits$inRastName <- inMaskName
		} else if (rastOrVect == 'vector') {
			.checkVectExists(replace=replace, vect=mask, inVectName=inMaskName, outGrassName=NULL, ...)
			args <- c(args, vector=mask)
			inits$vect <- mask
			inits$inVectName <- inMaskName
		}
 		
	}

	### initialize GRASS
	input <- do.call('startFaster', inits)
	
	### execute
	if (autoRegion & rastOrVect != 'vector') regionReshape(inMaskName)
	do.call(rgrass::execGRASS, args=args)
	
	# force all cells to 1
	if (!clip) {
	
		rgrass::execGRASS('r.mapcalc', expression = paste0(outGrassName, ' = 1'), flags=c('quiet', 'overwrite'), intern=TRUE)
	
	}
	
	### export
	if (grassToR) {

		out <- fasterWriteRaster(outGrassName, paste0(tempfile(), '.tif'), overwrite=TRUE, trimRast=trimRast)
		out
		
	} else { invisible(TRUE) }
	
}


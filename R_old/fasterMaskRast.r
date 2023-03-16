#' Create a mask to affect subsequent operations on rasters
#'
#' This function creates a copy of a raster, but forces some cells to \code{NA} by using a "mask". A mask can be created from a raster or a spatial vector.  If from a raster, then only cells in the input raster that overlap with non-\code{NA} cells in the mask are copied. If from a vector, only cells that overlap with the vector are copied.\emph{Currently, this function only works on numeric (not categorical) rasters.}\cr
#' 
#' In \code{GRASS}, a mask raster is always named \code{MASK}. If such a raster exists in the active \code{GRASS} session, it causes operations on rasters to affect only those portions of the raster that fall within a non-\code{NA} (in \code{GRASS}, a non-\code{NULL}) cell. It will continue to affect nearly all \pkg{fasterRaster} functions that operate on rasters until it is removed. To remove a mask, set the argument \code{removeMask} to \code{TRUE} when using this function, or use \code{\link{fasterRm}} or \code{\link{fasterRename}} on the \code{MASK} raster.\cr
#'
#' This function is similar to \code{\link{fasterMakeMask}}, except that this creates a mask, masks a focal raster, then (by default) removes the mask. That function creates a mask for use with subsequent operations.
#' 
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_trimRast
#' @inheritParams .sharedArgs_autoRegion
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_outGrassName
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
#' @param removeMask If \code{TRUE} (default), then the \code{MASK} will be deleted from the \code{GRASS} session. If \code{FALSE}, it will noyt be removed so it can be used in subsequent operations. Later, you can remove a mask using \code{\link{fasterRm}} or \code{\link{fasterRename}}.
#' @param trim If \code{TRUE} (default) and the output is imported to \code{R}, then all rows and columns that are entirely \code{NA} will be removed.
#'
#' @return A \code{SpatRaster}. In the active \code{GRASS} session a raster is created, and possibly another raster named \code{MASK}.
#'
#' @example man/examples/ex_fasterMask.r
#'
#' @seealso \code{\link{fasterMakeMask}} in \pkg{fasterRaster}; code{\link[terra]{mask}} in package \pkg{terra}; \code{GRASS} module }\href{https://grass.osgeo.org/grass82/manuals/r.mask.html}{\code{r.mask}}
#'
#' @export

fasterMaskRast <- function(
	rast,
	mask,
	inRastName,
	inMaskName,
	rastOrVect = NULL,
	
	removeMask = TRUE,
	trim = TRUE,
	outGrassName = 'maskedRast',
	
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
		
		### arguments
		.checkRastExists(replace=replace, rast=NULL, inRastName=NULL, outGrassName=outGrassName, ...)
		if (!missing(rast)) {
			if (!inherits(rast, 'character') & !inherits(rast, 'SpatRaster')) rast <- terra::rast(rast)
			inRastName <- .getInRastName(inRastName, rast=rast)
			.checkRastExists(replace=replace, rast=rast, inRastName=inRastName, outGrassName=NULL, ...)
		} else {
			rast <- inRastName <- NULL
		}
		
		### flags
		flags <- .getFlags(replace=replace)
		
		### restore
		# on.exit(.restoreLocation(), add=TRUE) # return to starting location
		if (autoRegion) on.exit(regionExt('*'), add=TRUE) # resize extent to encompass all spatials

		### ellipses and initialization arguments
		initsDots <- .getInitsDots(..., callingFx = 'fasterMask')
		inits <- initsDots$inits
		dots <- initsDots$dots

	###############
	### end commons

	### function-specific
	
	# arguments for "r.mask"
	if (inherits(mask, 'SpatRaster')) {
		if (!inherits(mask, 'character') & !inherits(mask, 'SpatRaster')) mask <- terra::rast(mask)
		inMaskName <- .getInRastName(inMaskName, rast=mask)
		.checkRastExists(replace=replace, rast=mask, inRastName=inMaskName, outGrassName=NULL, ...)
		rastOrVect <- 'raster'
	} else if (inherits(mask, c('SpatVector', 'sf'))) {
		if (!inherits(mask, 'character') & !inherits(mask, 'SpatVector')) mask <- terra::vect(mask)
		inMaskName <- .getInVectName(inMaskName, vect=mask)
		.checkVectExists(replace=replace, vect=mask, inVectName=inMaskName, outGrassName=NULL, ...)
		rastOrVect <- 'vector'
	} else if (inherits(mask, 'character')) {
	
		inMaskName <- mask
		rastOrVect <- .matchRastOrVect(rastOrVect, n=1, naOK=TRUE)
		if (is.na(rastOrVect)) rastOrVect <- .determineRastOrVect(x=mask, errorNotFound=TRUE, dupsOK=FALSE)
		if (rastOrVect == 'raster') {
			.checkRastExists(replace=replace, rast=mask, inRastName=inMaskName, outGrassName=NULL, ...)
		} else if (rastOrVect == 'vector') {
			.checkVectExists(replace=replace, vect=mask, inVectName=inMaskName, outGrassName=NULL, ...)
		}
	}

	args <- list(
		cmd = 'r.mask',
		flags = flags
	)
	args <- c(args, dots)
	
	# initialize GRASS
	input <- do.call('startFaster', inits)
	
	### execute
	
	# export mask object
	if (rastOrVect == 'raster') {
		fasterRast(rast=mask, inRastName=inMaskName, replace=replace, autoRegion=TRUE)
		args <- c(args, raster=inMaskName)
	} else {
		fasterVect(vect=mask, inVectName=inMaskName, replace=replace, autoRegion=TRUE)
		args <- c(args, vector=inMaskName)
	}

	do.call(rgrass::execGRASS, args=args)

	# # make copy of raster to mask
	# fromTo <- paste0(inRastName, ',TEMPTEMP_rastCopy')
	# thisFlags <- unique(flags, 'overwrite')
	# rgrass::execGRASS('g.copy', raster=fromTo, flags=thisFlags)
	
	# # multiply by mask
	# ex <- '= TEMPTEMP_rastCopy * MASK'
	# fasterApp(
		# 'TEMPTEMP_rastCopy',
		# expression = ex,
		# outGrassName = outGrassName,
		# replace = replace,
		# grassToR = FALSE,
		# autoRegion = FALSE,
		# grassDir = grassDir
	# )

	fasterApp(
		rast = inRastName,
		ex = paste('=', inRastName),
		outGrassName = outGrassName,
		replace = replace,
		grassToR = FALSE,
		autoRegion = FALSE,
		grassDir = grassDir,
		...
	)

	### export
	if (grassToR) {

		out <- fasterWriteRaster(outGrassName, paste0(tempfile(), '.tif'), overwrite=TRUE, trimRast=trimRast)
		if (removeMask) fasterRm('MASK', rastOrVect = 'raster')
		out
		
	} else { invisible(TRUE) }
	
}


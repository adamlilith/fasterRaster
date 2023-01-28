#' Mask values in a raster using another raster or vector
#'
#' This function creates a copy of a raster, except that some cells can be set to \code{NA} by using a "mask". A mask can be created from a raster or a spatial vector.  If from a raster, then only cells in the inoput raster that overlap with non-\code{NA} cells in the mask are copied. If from a vector, only cells that overlap with the vector are copied.\cr
#' 
#' In \code{GRASS}, a mask raster is always named \code{MASK}. If such a raster exists in the active \code{GRASS} session, it causes operations on rasters to affect only those portions of the raster that fall within a non-\code{NA} (in \code{GRASS}, a non-\code{NULL}) cell. It will continue to affect nearly all \pkg{fasterRaster} functions that operate on rasters until it is removed. To remove a mask, set the argument \code{removeMask} to \code{TRUE} when using this function, or use \code{\link{fasterRm}} or \code{\link{fasterRename}} on the \code{MASK} raster.
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_grassToR
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
#' @param removeMask If \code{TRUE} (default), then the \code{MASK} will be deleted from the \code{GRASS} session. If \code{FALSE}, it will noyt be removed so it can be used in subsquent operations. Later, you can remove a mask using \code{\link{fasterRm}} or \code{\link{fasterRename}}.
#'
#' @return A \code{SpatRaster}. In the active \code{GRASS} session a raster is created, and possinly another raster named \code{MASK}.
#'
#' @example man/examples/ex_fasterMask.r
#'
#' @seealso \code{\link[terra]{mask}} in package \pkg{terra}; \code{GRASS} module }\href{https://grass.osgeo.org/grass82/manuals/r.mask.html}{\code{r.mask}}
#'
#' @export

fasterMask <- function(
	rast,
	mask,
	inRastName,
	inMaskName,
	rastOrVect = NULL,
	
	removeMask = TRUE,
	outGrassName = 'maskedRast',
	
	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	# special... needed for "dots" in commons block
	if (missing(inMaskName)) inMaskName <- NULL

	### commons v1 -- modified
	##########################
		
		### arguments
		if (exists('rast', where=environment(), inherits=FALSE)) {
			inRastName <- .getInRastName(inRastName, rast=rast)
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
		inMaskName <- .getInRastName(inMaskName, rast=mask)
		rastOrVect <- 'raster'
	} else if (inherits(mask, c('SpatVector', 'sf'))) {
		inMaskName <- .getInVectName(inMaskName, vect=mask)
		rastOrVect <- 'vector'
	} else if (inherits(mask, 'character')) {
		inMaskName <- mask
		rastOrVect <- .getRastOrVect(rastOrVect, n=1, nullOK=TRUE)
		if (is.null(rastOrVect)) {
		
			spatials <- fasterLs(rastOrVect=rastOrVect)
			spatials <- spatials[spatials %in% mask]
			rastOrVect <- names(spatials)
		
			if (length(spatials) > 1L) stop('There is more than one object with the name given\nby "mask" in GRASS. Use the "rastOrVect" argument.')
		
		}
		
	}

	args <- list(
		cmd = 'r.mask',
		flags = flags
	)
	args <- c(args, dots)
	
	# initialize GRASS
	input <- do.call('initGrass', inits)
	
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

	# make copy of raster to mask
	fromTo <- paste0(inRastName, ',TEMPTEMP_rastCopy')
	thisFlags <- unique(flags, 'overwrite')
	rgrass::execGRASS('g.copy', raster=fromTo, flags=thisFlags)
	
	# multiply by mask
	ex <- '= TEMPTEMP_rastCopy * MASK'
	fasterApp('TEMPTEMP_rastCopy', expression=ex, outGrassName=outGrassName, replace=replace, grassToR=FALSE, autoRegion=FALSE, grassDir=grassDir)

	### export
	if (grassToR) {

		out <- fasterWriteRaster(outGrassName, paste0(tempfile(), '.tif'), overwrite=TRUE)
		out <- terra::setMinMax(out)
		out
		
	}
	
}


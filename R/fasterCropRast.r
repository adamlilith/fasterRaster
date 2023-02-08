#' Crop a raster to the area of overlap with another raster or vector
#'
#' Crop a raster to the extent of another raster or vector. NB: This function is actually an alias for \code{\link{fasterMask}}, which creates a mask from a raster or vector, then creates a copy of the overlapping portion of the focal raster. \emph{Currently, this function only works on numeric (not categorical) rasters.}
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_trimRast
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @param template Any of:
#' \itemize{
#'		\item A \code{SpatRaster}, \code{SpatVector}, or object of class \code{sf}, used to define the extent of the crop region.
#'		\item The name of raster or vector already in the \code{GRASS} session.
#' }
#' @param rastOrVect Either \code{'raster'} or \code{'vector'}. If \code{NULL} (default), then the function will attempt to guess whether \code{template} refers to a raster or vector. However, in \code{GRASS}, it is possible to have a raster and a vector of the same name. If this is the case, then you can specify whether \code{template} is a raster or vector (partial matching is supported).
#'
#' @return A \code{SpatRaster}. Also creates a raster in a new grass session named \code{outGrassName}.
#' 
#' @seealso \code{\link{fasterMask}} and \code{\link{fasterMakeMask}} in \pkg{fasterRaster}; \code{\link[terra]{crop}} in \pkg{terra}; \code{GRASS} module \href{https://grass.osgeo.org/grass82/manuals/r.clip.html}{\code{r.clip}}--but note that this function does \emph{not} use \code{r.clip}.
#' 
#' @examples man/examples/ex_fasterCropRast.r
#'
#' @export

fasterCropRast <- function(
	rast,
	template,
	inRastName,
	inTemplateName,
	rastOrVect = NULL,
	outGrassName = 'croppedRast',
	trim = TRUE,

	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	trimRast = fasterGetOptions('trimRast', TRUE),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	### arguments
	if (missing(inTemplateName)) inTemplateName <- NULL

	### commons v1
	##############

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
		initsDots <- .getInitsDots(..., callingFx = 'fasterCropRast')
		inits <- initsDots$inits
		dots <- initsDots$dots

	###############
	### end commons

	### initialize GRASS
	input <- do.call('startFaster', inits)

	### execute

	# copy existing mask
	maskExists <- fasterExists('MASK', rastOrVect='raster')
	if (maskExists) {

		tempMaskName <- paste0('TEMPTEMP_MASK', round(1E9 * runif(1)))
		fasterRename('MASK', tempMaskName, rastOrVect='raster')
		on.exit(
			fasterRename(
				from = tempMaskName,
				to = 'MASK',
				rastOrVect = 'raster',
				replace = TRUE
			),
			add=TRUE
		)

	}

	# make mask
	fasterMakeMask(
		mask = template,
		inMaskName = inTemplateName,
		rastOrVect = rastOrVect,
		
		clip = FALSE,
		
		outGrassName = 'MASK',
		
		replace = replace,
		restartGrass = FALSE,
		grassToR = FALSE,
		autoRegion = autoRegion,
		grassDir = grassDir,
		...
	)

	# copy focal raster
	fasterCopy(
		from = inRastName,
		to = outGrassName,
		rastOrVect = 'raster',
		replace = replace,
		autoRegion = autoRegion
	)
	
	# # multiply focal by mask
	# ex <- paste0(' = ', inRastName, ' * MASK')
	# fasterApp(
		# rast = inRastName,
		# inRastName = inRastName,
		# expression = ex,

		# outGrassName = outGrassName,

		# replace = TRUE,
		# grassToR = FALSE,
		# autoRegion = autoRegion,
		# grassDir = grassDir,
		# ...
	# )

	# remove mask
	fasterRm('MASK', rastOrVect = 'raster')
	
	# return
	if (grassToR) {

		out <- fasterWriteRaster(outGrassName, paste0(tempfile(), '.tif'), overwrite=TRUE, trimRast=trimRast)
		out

	} else { invisible(TRUE) }

}

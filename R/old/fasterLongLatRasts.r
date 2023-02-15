#' Longitude and latitude rasters
#'
#' Create two rasters, one with cell values equal to cell latitude and the other cell longitude (in geographic, or unprojected coordinates).
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_trimRast
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @param mask Logical. If \code{TRUE} (default), cells that have \code{NA}s in \code{rast} will also have \code{NA}s in the output rasters.
#' @param outGrassName Character vector with two elements, one for longitude and one for latitude.  Name of output in \pkg{GRASS}. This is useful if you want to refer to the output objects in \code{GRASS} later in a \code{GRASS} session.
#'
#' @return If \code{grassToR} if \code{TRUE}, then a \code{SpatRaster} stack with the same extent, resolution, and coordinate reference system as \code{rast}. Regardless, rasters with names given by \code{outGrassName} are written into the \code{GRASS} session. Values represent cells' centroid coordinates in unprojected (WGS84) format.
#'
#' @seealso \href{https://grass.osgeo.org/grass82/manuals/r.latlong.html}{\code{r.latlong}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterLongLatRasts.r
#'
#' @export

fasterLongLatRasts <- function(
	rast,
	inRastName,
	mask = TRUE,
	outGrassName = c('longitudeRast', 'latitudeRast'),

	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	trimRast = fasterGetOptions('trimRast', TRUE),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

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
		initsDots <- .getInitsDots(..., callingFx = 'fasterLongLatRasts')
		inits <- initsDots$inits
		dots <- initsDots$dots

	###############
	### end commons

	### initialize GRASS
	input <- do.call('startFaster', inits)

	### function-specific
	args <- list(
		cmd = 'r.latlong',
		input = inRastName,
		output = NA,
		flags = flags
	)
	args <- c(args, dots)

	### execute
	if (autoRegion) regionReshape(inRastName)

	# mask
	if (mask){
		
		# pre-existing mask?
		if (fasterExists('MASK', rastOrVect='raster')) {
			tempMask <- .makeTempName('TEMPTEMP_oldMASK')
			fasterRename('MASK', tempMask)
			maskExists <- TRUE
		} else {
			maskExists <- FALSE
		}
		
		# make a new mask
		fasterMakeMask(inRastName, inRastName, rastOrVect='raster', clip=TRUE, restartGrass=FALSE, autoRegion=FALSE, ...)

	}
	
	# latitude
	args$output <- outGrassName[2L]
	do.call(rgrass::execGRASS, args=args)
	
	# longitude
	args$flags <- c(args$flags, 'l') # longitude
	args$output <- outGrassName[1L]
	do.call(rgrass::execGRASS, args=args) # latitude

	if (mask) {
	
		fasterRm('MASK', rastOrVect='raster')
	
		if (maskExists) {

			fasterRm(tempMask)
			fasterRename(tempMask, 'MASK', rastOrVect='raster')

		}
		
	}

	if (grassToR) {
	
		long <- fasterWriteRaster(outGrassName[1L], paste0(tempfile(), '.tif'), overwrite=TRUE, trimRast=trimRast)
		lat <- fasterWriteRaster(outGrassName[2L], paste0(tempfile(), '.tif'), overwrite=TRUE, trimRast=trimRast)
		out <- c(long, lat)
		names(out) <- outGrassName
		out
		
	} else { invisible(TRUE) }

}

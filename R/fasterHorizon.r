#' Height of the horizon from an elevation raster
#'
#' This function calculates a raster where values represent the height of the horizon for each cell in one or more directions. Height is given in radians degrees or radians.
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_trimRast
#' @inheritParams .sharedArgs_outGrassName
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @param units Either \code{'radians'} (default) or \code{'degrees'}.
#' @param directions Integer vector with zero or positive values. Direction(s) in which to calculate horizon height for each cell. By default, these are given in degrees clockwise from 0, so 0 is north, 90 east, 180 south, and 270 west. However, if you define \code{northIs0 = FALSE}, then the directions are given degrees counterclockwise from east, so east is 0, north 90, west 180, and south 270. Regardless, the default is to calculate horizon angle in all four directions. One raster is created per direction. Note that the output will be labeled according to the angle of the directions (e.g., \code{horizonHeight_090} will be horizon height facing east if \code{northIs0 = TRUE} (default), but horizon height facing north if \code{northIs0 = FALSE}. Note that \code{GRASS} automatically rounds these values down to the nearest integer, so this function does the same but also produces a warning.
#' @param northIs0 Logical. If \code{TRUE} (default), argument \code{directions} specifies horizon height clockwise from 0, so 0 is north, 90 east, 180 south, and 270 west. If \code{FALSE}, angles are counterclockwise from east, so east is 0, north 90, west 180, and south 270. The latter is the default for the \code{GRASS} function \code{r.horizon}.
#' @param bufferZone Numeric >= 0 (default is 0). A buffer of the specified width will be generated around the raster before calculation of horizon angle. If the coordinate system is in longitude/latitude (e.g., WGS84 or NAD83), then this is specified in degrees. Otherwise units are map units (usually meters).
#' @param maxDist Either \code{NULL} (default) or numeric >= 0. Maximum distance to consider when finding horizon height in meters. If \code{NULL} (default), the maximum distance is the full extent of the raster. Smaller values can decrease run time but also reduce accuracy.
#' @param distance Numeric in the range [0.5, 1.5] (default is 1). This determines the step size when searching for the horizon from a given point. The default value of 1 goes cell-by-cell (i.e., search distance step size is one cell width).
#'
#' @param ... Arguments to send to \code{GRASS} module \href{https://grass.osgeo.org/grass82/manuals/r.horizon.html}{\code{r.horizon}}.
#'
#' @return If \code{grassToR} if \code{TRUE}, then a raster or raster stack stack with the same extent, resolution, and coordinate reference system as \code{rast}. Otherwise, a raster is written into the \code{GRASS} session. The name of this raster is as \code{paste0(outGrassName, '_', xxx)}. For example, if \code{outGrassName = 'horizonHeight'}, and \code{directions} is \code{c(0, 90, 180, 270)}, then four rasters will be written: \code{horizonHeight_000}, \code{horizonHeight_090}, \code{horizonHeight_180}, and \code{horizonHeight_270}. Note the padding with zeros before angles <10.
#'
#' @seealso \href{https://grass.osgeo.org/grass82/manuals/r.horizon.html}{\code{r.horizon}} in \code{GRASS}
#.
#' @example man/examples/ex_fasterHorizon.r
#'
#' @export

fasterHorizon <- function(
	rast,
	inRastName,
	units = 'degrees',
	directions = c(0, 90, 180, 270),
	northIs0 = TRUE,
	bufferZone = 0,
	maxDist = NULL,
	mask = NULL,
	outGrassName = 'horizonHeightRast',

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
		initsDots <- .getInitsDots(..., callingFx = 'fasterHorizon')
		inits <- initsDots$inits
		dots <- initsDots$dots

	###############
	### end commons

	### errors?

	### function-specific
	if (units == 'degrees') flags <- c(flags, 'd')
	if (northIs0) flags <- c(flags, 'c')
	if (any(directions %% 1 != 0)) {
		directions <- floor(directions)
		warning('Non-integer value(s) used for argument "directions." Value(s)\nhas been rounded down (GRASS will do this anyway).')
	}
	
	args <- list(
		cmd = 'r.horizon',
		elevation = inRastName,
		output = outGrassName,
		direction = NA,
		bufferzone = bufferZone,
		flags = flags
	)
	args <- c(args, dots)
	
	if (!is.null(maxDist)) args$maxdistance <- maxDist

	### initialize GRASS
	input <- do.call('startFaster', inits)

	### execute
	if (autoRegion) regionReshape(inRastName)
	for (direction in directions) {

		thisOutGrassName <- outGrassName
		args$output <- thisOutGrassName
		args$direction <- direction

		do.call(rgrass::execGRASS, args=args)
		
	}

	### return to R
	if (grassToR) {

		for (direction in directions) {
			
			degs <- sprintf('%03.0f', direction)
			thisOut <- fasterWriteRaster(paste0(outGrassName, '_', degs), paste0(tempfile(), '.tif'), overwrite=TRUE, trimRast=trimRast)
			
			out <- if (exists('out', inherits=FALSE)) {
				c(out, thisOut)
			} else {
				thisOut
			}
			
		}

		out
		
	} else { invisible(TRUE) }
	
}

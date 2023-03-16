#' Project one or more rasters to a different coordinate reference system
#'
#' Project and resample one or more rasters. This function works by transferring a raster between two \code{GRASS} \href{location}s with different coordinate reference systems. You can use the default "from" location from which the raster is transferred, or name your own. By default, you do not need to name the destination location (which is named "default"), or you can specify one using the \code{location} argument (supplied in the \code{...}). \strong{Note}: Even if the same method for local interpolation is used (bilinear, bicubic, lanczos), \pkg{terra}'s \code{\link[terra]{project}} function and this function will usually yield rasters that differ slightly (i.e., in numerical values by <~1% or so, and in extent up to the first decimal place).  See the \strong{\code{Examples}} section for illustrations./cr
#' When projecting rasters that "wrap around" (i.e., whole-world rasters or rasters that have edges that actually circle around to meet on the globe), the \code{trimRast} argument should be \code{FALSE} to avoid removing rows and columns from the "edge" of the map.
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_memory
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_trimRast
#' @inheritParams .sharedArgs_outGrassName_multiple
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @param template This provides the "destination" coordinate reference system for the raster. This argument can be either a \code{SpatRaster} or the name of a \code{SpatRaster} in the active \code{GRASS} session. The raster will be resampled to the resolution of the \code{template} raster.
#'	
#' @param inTemplateName The name of the \code{SpatRaster} template. Ignored if \code{template} is the name of a raster existing in the active \href{location} (i.e., a character).
#'
#' @param method Character, method for resampling cells:
#' \itemize{
#' 		\item \code{nearest}: Nearest neighbor (uses value from 1 cell).
#' 		\item \code{bilinear}: Bilinear interpolation (default; uses weighted values from 4 cells).
#' 		\item \code{bicubic}: Bicubic interpolation (uses weighted values from 16 cells).
#' 		\item \code{lanczos}: Lanczos interpolation (uses weighted values from 25 cells).
#' }
#'
#' @param na.rm If \code{TRUE} (default), then for interpolating the value of a particular cell, any \code{NA}s in surrounding cells will be ignored. If \code{FALSE}, then the focal cell will become \code{NA} if any surrounding cells are also \code{NA}.
#'
#' @param fromLocation The name of the \href{location} from which the raster in \code{rast} will be projected from. If this location does not exist, it will be created, but it can also be a pre-existing location created by another \pkg{fasterRaster} function or using \code{\link{startFaster}}.
#'
#' @details When projecting rasters with continuous values, the argument \code{method} can be one of \code{'bilinear'}, \code{'bicubic'}, or \code{'lanczos'}. If \code{na.rm} is \code{TRUE} (default), then each of these methods actually corresponds to \code{GRASS}'s "fallback" methods that ignore cells with \code{NA}s. If \code{na.rm} is \code{FALSE}, then the "plain" (non-fallback) method is used.
#'
#' @return If \code{grassToR} if \code{TRUE}, then a raster or raster stack with the same extent, resolution, and coordinate reference system as \code{rast}. Regardless, a raster with the name given by \code{outGrassName} is written into the \code{GRASS} session.
#'
#' @seealso \code{\link[terra]{project}} in \pkg{terra}; \href{https://grass.osgeo.org/grass82/manuals/r.proj.html}{\code{r.proj}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterProjectRast.r
#'
#' @export

fasterProjectRast <- function(
	rast,
	template,
	inRastName = NULL,
	inTemplateName = NULL,
	method = 'bilinear',
	na.rm = TRUE,
	fromLocation = 'fromLocation',
	outGrassName = inRastName,

	memory = fasterGetOptions('memory', 300),
	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	trimRast = fasterGetOptions('trimRast', TRUE),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	ells <- list(...)

	### commons v1
	##############

		### arguments

		# rast
		if (!inherits(rast, 'character') & !inherits(rast, 'SpatRaster')) rast <- terra::rast(rast)
		inRastName <- .getInRastName(inRastName, rast=rast)
		if (.getSessionStarted() & 'location' %in% names(ells)) {
			ells$location <- NULL
		}
		.checkRastExists(replace=replace, rast=NULL, inRastName=NULL, outGrassName=outGrassName, location=fromLocation, unlist(ells))
		if (is.null(outGrassName)) outGrassName <- inRastName

		# template
		if (!is.null(template)) {
			if (!inherits(template, 'character') & !inherits(template, 'SpatRaster')) template <- terra::rast(template)
			inTemplateName <- .getInRastName(inTemplateName, rast=template)
			.checkRastExists(replace=replace, rast=template, inRastName=inTemplateName, outGrassName=NULL, ...)
		}

		### flags
		flags <- .getFlags(replace=replace)
		
		### restore
		# on.exit(.restoreLocation(), add=TRUE) # return to starting location
		if (autoRegion) on.exit(regionExt('*'), add=TRUE) # resize extent to encompass all spatials

		### ellipses and initialization arguments
		initsDots <- .getInitsDots(..., callingFx = 'fasterProjectRast')
		inits <- initsDots$inits
		dots <- initsDots$dots

	###############
	### end commons
	
	# get method
	methods <- c('bilinear', 'bicubic', 'lanczos')
	method <- tolower(method)
	match <- pmatch(method, methods)
	if (is.na(match)) stop('Argument "method" must be "bilinear", "bicubic", or "lanczos".')
	method <- methods[match]

	### export focal raster to "from" location
	initsFrom <- inits
	initsFrom$rast <- rast
	initsFrom$inRastName <- inRastName
	initsFrom$location <- fromLocation
	if ('dir' %in% names(inits)) initsFrom$dir <- ells$dir
	fromGrass <- do.call('startFaster', initsFrom)

	### export template to current location or switch to pre-existing location
	if (!is.null(template)) {
		inits$rast <- template
		inits$inRastName <- inTemplateName
	} else {
		inits$rast <- NULL
		inits$inRastName <- NULL
	}
	if (!('location' %in% inits)) location <- 'default'

	do.call('startFaster', inits)

	### function-specific
	if (na.rm & method != 'nearest') method <- paste0(method, '_f')

	### transfer raster to the target location
	args <- list(
		cmd = 'r.proj',
		input = NA,
		output = NA,
		location = fromLocation,
		mapset = 'PERMANENT',
		method = method,
		memory = memory,
		flags = flags,
		intern = TRUE
	)
	args <- c(args, dots)
	
	if (!trimRast) args$flags <- c(args$flags, 'n')

	numRastLayers <- length(inRastName)
	if (autoRegion) regionReshape(inTemplateName[1L])
	
	for (i in 1L:numRastLayers) {

		thisArgs <- args
		thisArgs$input <- inRastName[i]
		thisArgs$output <- outGrassName[i]
		
		do.call(rgrass::execGRASS, thisArgs)
	
	}

	# return
	if (grassToR) {

		for (i in 1L:numRastLayers) {

			thisOut <- fasterWriteRaster(outGrassName[i], paste0(tempfile(), '.tif'), overwrite=TRUE, trimRast=trimRast)
			out <- if (exists('out', inherits=FALSE)) {
				c(out, thisOut)
			} else {
				thisOut
			}
			
		}
		
		out
			
	} else { invisible(TRUE) }
	
}

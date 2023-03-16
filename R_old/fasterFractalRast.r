#' Create a raster with a fractal pattern
#'
#' Create a raster with a fractal pattern.
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_trimRast
#' @inheritParams .sharedArgs_outGrassName
#' @param dimension Numeric. Fractal dimension. Must be >2 and <3. Default is 2.05.
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @return If \code{grassToR} if \code{TRUE}, then a raster object with the same coordinate reference system, resolution, and extent as \code{rast}. Regardless, a raster is written into the \code{GRASS} session with the name given by \code{outGrassName}.
#'
#' @seealso \href{https://grass.osgeo.org/grass82/manuals/r.surf.fractal.html}{\code{r.surf.fractal}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterFractalRast.r
#'
#' @export

fasterFractalRast <- function(
	rast,
	inRastName,
	dimension = 2.05,
	outGrassName = 'fractalRast',

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
		initsDots <- .getInitsDots(..., callingFx = 'fasterFractalRast')
		inits <- initsDots$inits
		dots <- initsDots$dots

	###############
	### end commons

	### errors?
	if (dimension <= 2 | dimension >= 3) stop('Argument "dimension" must be >2 and <3.')

	### function-specific
	args <- list(
		cmd = 'r.surf.fractal',
		# input = inRastName,
		output = outGrassName,
		dimension = dimension,
		flags = flags
	)
	args <- c(args, dots)

	### initialize GRASS
	input <- do.call('startFaster', inits)

	### execute
	if (autoRegion) regionReshape(inRastName)
	do.call(rgrass::execGRASS, args=args)

	### export
	if (grassToR) {

		out <- fasterWriteRaster(outGrassName, paste0(tempfile(), '.tif'), overwrite=TRUE, trimRast=trimRast)
		out
		
	} else { invisible(TRUE) }

}

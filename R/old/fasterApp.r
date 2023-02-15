#' User-defined calculations on one or more rasters
#'
#' This function creates a raster from mathematical or logical operations on one or more rasters.
#'
#' @inheritParams .sharedArgs_rast_multiple
#' @inheritParams .sharedArgs_inRastName_multiple
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_outGrassName
#' @inheritParams .sharedArgs_trimRast
#' @inheritParams .sharedArgs_autoRegion
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @param expression Character. Formula to evaluate. Below are examples where the input raster is named "rast" Note that unlike in the \code{GRASS} \href{https://grass.osgeo.org/grass82/manuals/r.mapcalc.html}{\code{r.mapcalc}} module, the expression should \emph{not} start with the name of the output or an equals sign. The output raster is named by \code{outGrassName}, and this will be created by executing the formula.
#' \itemize{
#' \item Take the input raster "rast", multiplies each cell by 0, then adds 1 to each cell: \code{'= (rast * 0) + 1'}
#' \item Take the input raster "rast", multiplies square the value of each cell: \code{'= rast^2'}
#' \item Take the input raster "rast" (implicitly), and sets each cell value to 17: \code{'= 17'}
#' \item Take the input rasters "rast1" and "rast2", and multiplies them together. Note that in this case, argument \code{rast} will actually be a \code{SpatRaster} "stack" of two rasters with later names "rast1" and "rast2": \code{'= rast1 * rast2'}
#' \item Take the input raster "rast" and redefines each cell value to equal the sum of it and the four cells in the rooks's neighborhood (north, south, east, west of it): \code{'= rast[0, 0] + rast[-1, 0] + rast[0, -1] + rast[0, 1] + rast[1, 0]'}
#' \item Low-pass (averaging) filter across a 3x3 neighborhood: \code{'(= rast[-1, -1] + rast[-1, 0] + rast[-1, 1] + rast[0, -1] + rast[0, 0] + rast[0, 1] + rast[1, -1] + rast[1, 0] + rast[1, 1]) / 9'}
#' \item High-pass ("edge-finding") filter across a 3x3 neighborhood: \code{'= -0.7 * rast[-1, -1] -1 * rast[-1, 0] -0.7 * rast[-1, 1] -1 * rast[0, -1] + 6.8 * rast[0, 0] -1 * rast[0, 1] -0.7 * rast[1, -1] -1 * rast[1, 0] -0.7 * rast[1, 1]'}
#' }
#' See the documentation for \href{https://grass.osgeo.org/grass82/manuals/r.mapcalc.html}{\code{r.mapcalc}} for more information. Note that some operations that may make sense in \code{GRASS} may not make sense when exported back to R (e.g., changing color).
#'
#' @return If \code{grassToR} if \code{TRUE}, then a raster object with the same coordinate reference system, extent, and resolution as \code{rast}. Regardless, a raster by a name given in \code{expression} (before the "=" symbol) is written into the \code{GRASS} session.
#'
#' @details The function \code{\link[terra]{focal}}, \code{\link[terra]{focalCpp}}, and \code{\link{fasterFocal}} \emph{may} be faster for focal calculations. 
#'
#' @seealso \code{\link[terra]{app}} and \code{\link[terra]{focal}} in \pkg{terra}; \code{\link{fasterFocal}} in \pkg{fasterRaster}; \href{https://grass.osgeo.org/grass82/manuals/r.mapcalc.html}{\code{r.mapcalc}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterApp.r
#'
#' @export

fasterApp <- function(
	rast,
	inRastName,
	expression,
	outGrassName = 'appRast',
	
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
		initsDots <- .getInitsDots(..., callingFx = 'fasterApp')
		inits <- initsDots$inits
		dots <- initsDots$dots

	### function-specific

	# expresssion
	if (!is.character(expression)) stop('Argument "expression" must be a text string.')
	expression <- trimws(expression)
	if (substr(expression, 1, 1) == '=' & substr(expression, 2, 2) != ' ') stop('An expression must begin with an equals sign then a space.')
	if (substr(expression, 1, 1) != '=') expression <- paste0('= ', expression)
	expression <- paste(outGrassName, expression)

	# execute
	args <- list(
		cmd = 'r.mapcalc',
		expression = expression,
		flags = flags
	)
	args <- c(args, dots)

	### initialize GRASS
	input <- do.call('startFaster', inits)

	### execute
	if (autoRegion) regionReshape(inRastName)
	do.call(rgrass::execGRASS, args=args)

	if (grassToR) {

		out <- fasterWriteRaster(outGrassName, paste0(tempfile(), '.tif'), overwrite=TRUE, trimRast=trimRast)
		out
		
	} else { invisible(TRUE) }
	
}

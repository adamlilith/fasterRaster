#' Generic function to call a GRASS function (module)
#'
#' This function is wrapper for \code{\link[rgrass]{execGRASS}}, plus code necessary to initiate a \code{GRASS} session. Many of the functions in \pkg{fasterRaster} actually utilize this function.  This function works best for modules that take one raster and/or one vector as input and produce one raster or vector as output.
#'
#' @inheritParams .sharedArgs_inRastName_multiple
#' @inheritParams .sharedArgs_inVectName
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_trimRast
#'
#' @param mod Name of \code{GRASS} module (e.g., \code{'r.latlong'} or \code{'v.buffer'}.
#' @param rast A \code{SpatRaster} or the name of a raster already in an existing \code{GRASS} session. If the \code{GRASS} module does not use a raster, this can be \code{NULL}.
#' @param vect A \code{SpatVector}, an \code{sf} object, or the name of a spatial vector already in an existing \code{GRASS} session. If the \code{GRASS} module does not use a vector, this can be \code{NULL}.
#' @param out \code{NULL}, \code{'raster'}, or \code{'vector'} (partial string matching is used). Type of output expected from the \code{GRASS} module.
#' @param flags List of flags for the \code{GRASS} module. The default (\code{c('quiet', 'overwrite')}) causes the module to report little/no messages and to overwrite existing files of the same name. For more flags, see the help documentation for the respective \code{GRASS} module.
#'
#' @return A raster, vector, or some other product of the \code{GRASS} module.
#'
#' @seealso See the \code{OS Geo GRASS} website for a catalog and detailed descriptions of \href{https://grass.osgeo.org/grass82/manuals/vector.html}{vector} and \href{https://grass.osgeo.org/grass82/manuals/raster.html}{raster} modules. Related functions include \code{\link[rgrass]{parseGRASS}} and \code{\link[rgrass]{execGRASS}} in \pkg{rgrass}, and \code{\link{fasterRast}} and \code{\link{fasterVect}} for exporting rasters and vectors to an existing \code{GRASS} session.
#'
#' @example man/examples/ex_faster.r
#'
#' @export

faster <- function(
	mod,
	rast,
	vect,
	inRastName,
	inVectName,
	...,
	out = NULL,
	outGrassName = 'output',
	flags = 'quiet',
	
	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	trimRast = fasterGetOptions('trimRast', TRUE),
	outVectClass = fasterGetOptions('outVectClass', 'SpatVector'),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL)
) {

	flags <- .getFlags(replace=replace, flags=flags)

	args <- list(cmd=mod, flags=flags)
	args <- c(args, list(output = outGrassName))
	args <- c(args, list(...))

	if (!is.null(vect)) if (!inherits(vect, 'character')) inVectName <- 'vect'

	# execute
	if (
		(inherits(rast, c('SpatRaster', 'Raster')) & inherits(vect, 'character')) |
		(inherits(rast, 'character') & inherits(vect, c('SpatVector', 'sf', 'Spatial')))
	) {
	
		stop('Argument "rast" must be:\n* NULL;\n* the name of a raster already in a GRASS session; or\n* a SpatRaster.\nArgument "vect" must be:\n* NULL;\n* the name of a vector already in a GRASS session; or\n* a SpatVector or sf object.\nBoth "rast" and "vect" cannot be NULL simultaneously.\nOne cannot be a name and the other a raster/vector.')
		
	} else if (!is.null(rast) | !is.null(vect)) {
		
		if (is.null(inits)) inits <- list()
		inits <- c(inits, list(rast=rast, vect=vect, inRastName=inRastName, inVectName=inVectName, replace=replace, grassDir=grassDir))
		input <- do.call('startFaster', inits)

		args <- if (!is.null(rast) & is.null(vect)) {
			c(args, list(input=input[['raster']]))
		} else if (is.null(rast) & !is.null(vect)) {
			c(args, list(input=input[['vector']]))
		} else {
			c(args, list(r=input[['raster']], v=input[['vector']]))
		}
	
	}
	
	success <- do.call(rgrass::execGRASS, args=args)
	
	# resize region to encompass all
	if (autoRegion) regionReshape(outGrassName)
	
	# return
	if (grassToR) {

		if (pmatch(out, c('raster', 'vector')) == 1L) {
			
			y <- fasterWriteRaster(outGrassName, paste0(tempfile(), '.tif'), overwrite=TRUE, trimRast=trimRast)
		
		} else if (pmatch(out, c('raster', 'vector')) == 2L) {
		
			y <- rgrass::read_VECT(outGrassName, flags='quiet')
			if (outVectClass == 'sf') y <- sf::st_as_sf(y)
			
		} else {
			stop('Argument "out" must be either "raster" or "vector".')
		}
			
		y
		
	} else { invisible(TRUE) }

}

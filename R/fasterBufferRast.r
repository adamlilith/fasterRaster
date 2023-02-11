#' Buffer cells of a raster
#'
#' Create a buffer around non-\code{NA} cells or cells with values of 0 in a raster. The output will be a raster.

#' This function is a potentially faster version of the \code{\link[terra]{buffer}} function in the \pkg{terra} package for calculating a buffer around non-\code{NA} cells or cells with values of 0 in a raster. The output will be a raster.
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
#' @param width Numeric. Maximum distance cells must be from focal cells to be within the buffer.
#'
#' @param units Indicates the units of \code{width}. Can be one of:
#' \itemize{
#' 		\item \code{'cells'}: Units are numbers of cells.
#'		\item \code{'meters'} (default), \code{'kilometers'}, \code{'feet'}, \code{'miles'}, or \code{'nautmiles'}.
#' }
#' Partial matching is used and case is ignored.
#'
#' @param method Only used if \code{units} is \code{'cells'}. Indicates the manner in which distances are calculated for adding of cells. Valid values include \code{'Euclidean'}, \code{'Manhattan'} ("taxi-cab" distance), or \code{'maximum'} (maximum of the north-south and east-west distances between points). Partial matching is used and case is ignored.
#'
#' @param ignore Only used if \code{units} is not \code{'cells'}. Either {NA} (default) or 0. The buffer will be drawn around cells that are not {NA} or not 0, depending on this value.
#'
#' @param out Any of:
#' \itemize{
#'		\item \code{'terra'} (default): The output raster will be the same as if using the \code{link[terra]{buffer}} function in the \pkg{terra} package. Cells in the buffer plus the cells around which buffers are created have values of 1, and all other cells are \code{NA}.
#' 		\item \code{'GRASS'}: All cells in the buffer are represented as 2's, all cells around which buffers are created are represented as 1's, and all other cells are \code{NA}.
#'		\item \code{'buffer'}: All cells in the buffer are represented as 1's and all others as \code{NA}.
#' }
#' Partial matching is used and case is ignored.
#'
#' @param lowMemory Only used if \code{units} is not \code{'meters'}. If \code{FALSE} (default) use faster, memory-intensive procedure. If \code{TRUE} then use the slower, low-memory version. This is only used if \code{units} is \code{'meters'}. To help decide, consider using the low-memory version on a system with 1 GB of RAM for a raster larger than about 32000x32000 cells, or for a system with  with 8 GB of RAM a raster larger than about 90000x90000 cells.
#'
#' @return If \code{grassToR} if \code{TRUE}, then a raster with the same extent, resolution, and coordinate reference system as \code{rast}. Regardless, a raster with a name given by \code{outGrassName} is written into the \code{GRASS} session.
#'
#' @seealso \code{\link[terra]{buffer}} in the \pkg{terra} package; \code{GRASS} modules \href{https://grass.osgeo.org/grass82/manuals/r.buffer.html}{\code{r.buffer}} and \href{https://grass.osgeo.org/grass82/manuals/r.buffer.html}{\code{r.grow}}
#'
#' @example man/examples/ex_fasterBufferRast.R
#'
#' @export

fasterBufferRast <- function(
	rast,
	inRastName,
	width,
	units = 'meters',
	method = 'Euclidean',
	ignore = NA,
	out = 'terra',
	lowMemory = FALSE,
	outGrassName = 'bufferedRast',

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
		initsDots <- .getInitsDots(..., callingFx = 'fasterBufferRast')
		inits <- initsDots$inits
		dots <- initsDots$dots

	###############
	### end commons

	### errors?
	
	# units
	distUnits <- c('meters', 'kilometers', 'feet', 'miles', 'nautmiles')
	unitsChoices <- c('cells', distUnits)
	match <- pmatch(tolower(units), unitsChoices)
	if (is.na(match)) stop('Argument "units" must be "cells", "meters", "kilometers", "feet", "miles", or "nautmiles".')
	units <- unitsChoices[match]

	# output
	outs <- c('grass', 'terra', 'buffer')
	match <- pmatch(tolower(out), outs)
	if (is.na(match)) stop('Argument "out" must be "terra", "GRASS", or "buffer".')
	out <- outs[match]
	
	### function-specific
	
	# if r.buffer (buffer by distance)
	if (units %in% distUnits) {
	
		if (lowMemory) {
			fx <- 'r.buffer.lowmem'
		} else {
			fx <- 'r.buffer'
		}

		args <- list(
			cmd = fx,
			input = inRastName,
			output = outGrassName,
			distances = width,
			units = units,
			flags = flags,
			intern = TRUE
		)

	# if r.grow (buffer by cells)
	} else if (units == 'cells') {
		
		fx <- 'r.grow'

		methods <- c('euclidean', 'manhattan', 'maximum')
		match <- pmatch(tolower(method), methods)
		if (is.na(match)) stop('Argument "method" must be "Euclidean", "Manhattan", or "maximum".')
		method <- methods[match]

		args <- list(
			cmd = fx,
			input = inRastName,
			output = outGrassName,
			radius = width,
			metric = method,
			flags = flags,
			intern = TRUE
		)

	}

	args <- c(args, dots)

	# initialize GRASS
	input <- do.call('startFaster', inits)

	### reshape region
	if (autoRegion) regionReshape(inRastName)

	### execute: GRASS output is 1 for buffered, 2 for the actual buffer, and NULL for everywhere else
	do.call(rgrass::execGRASS, args=args)

	### re-process
	if (units %in% distUnits) {

		# terra-style output
		if (out != 'grass') {
		
			# tempOutRast <- .makeTempName('distanceRast')
			
			# 1 for buffered/buffer, 0 for else
			ex <- if (out == 'terra') {
				paste0(outGrassName, ' = if(isnull(', outGrassName, '), 0, 1)')
			# 1 for buffer, NA for all else
			} else if (out == 'buffer') {
				paste0(outGrassName, ' = if(', outGrassName, ' == 2, 1, null())')
			}
			
			rgrass::execGRASS('r.mapcalc', expression=expression, flags=c('quiet', 'overwrite'), intern=TRUE)
			# fasterRm(outGrassName)
			# fasterRename(outGrassName, outGrassName, rastOrVect='raster', replace=TRUE, warn=FALSE)
		
		}

	# if units are cells
	} else if (units == 'cells') {

		# tempOutRast <- .makeTempName('distanceRast')

		# 1s for buffered, 2 for buffer
		ex <- if (out == 'grass') {
			paste0(outGrassName, ' = if(!isnull(', inRastName, '), 1, if(!isnull(', outGrassName, '), 2, null()))')
		# 1 for buffer/buffered, 0 for all else
		} else if (out == 'terra') {
			paste0(outGrassName, ' = if(!isnull(', outGrassName, '), 1, 0)')
		} else if (out == 'buffer') {
			# paste0(tempOutRast, ' = if(!isnull(', inRastName, '), null(), if(!is.null(', outGrassName, '), 1, null()))')
			# paste0(tempOutRast, ' = if(!isnull(', outGrassName, ') - !isnull(', inRastName, '), 1, null(), null())')
			paste0(outGrassName, ' = if(!isnull(', outGrassName, ') - !isnull(', inRastName, '), 1, null())')
		}

		rgrass::execGRASS('r.mapcalc', expression=ex, flags=c('quiet', 'overwrite'), intern=TRUE)
		# fasterRm(outGrassName)
		# fasterRename(tempOutRast, outGrassName, rastOrVect='raster')
	
	}

	### export
	if (grassToR) {

		out <- fasterWriteRaster(outGrassName, paste0(tempfile(), '.tif'), datatype='Byte', overwrite=TRUE, trimRast=trimRast)
		out

	} else { invisible(TRUE) }

}

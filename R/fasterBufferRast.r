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
#' @inheritParams .sharedArgs_outGrassName
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#' 
#' @param width Numeric. Maximum distance cells must be from focal cells to be within the buffer. Note that this function can only handle one value of \code{width} (unlike the function \code{r.buffer} in \code{GRASS}).
#' @param units Either \code{'meters'} (default), \code{'kilometers'}, \code{'feet'}, \code{'miles'}, or \code{'nautmiles'}. Indicates the units of \code{width}.
#' @param ignore Either {NA} (default) or 0. The buffer will be drawn around cells that are not {NA} or not 0, depending on this value.
#' @param out Any of:
#' \itemize{
#'		\item \code{'terra'} (default): The output raster will be the same as if using the \code{link[terra]{buffer}} function in the \pkg{terra} package. Cells in the buffer plus the cells around which buffers are created have values of 1, and all other cells are \code{NA}.
#' 		\item \code{'grass'}: All cells in the bufer are represented as 2's, all cells around which buffers are created are represented as 1's, and all other cells are \code{NA}.
#'		\item \code{'buffer'}: All cells in the buffer are represented as 1's and all others as \code{NA}.
#' }
#' @param lowMemory Logical. If \code{FALSE} (default) use faster, memory-intensive procedure. If \code{TRUE} then use the slower, low-memory version. To help decide, consider using the low-memory version on a system with 1 GB of RAM for a raster larger than about 32000x32000 cells, or for a system with  with 8 GB of RAM a raster larger than about 90000x90000 cells.
#'
#' @return If \code{grassToR} if \code{TRUE}, then a raster with the same extent, resolution, and coordinate reference system as \code{rast}. Regardless, a raster with a name given by \code{outGrassName} is written into the \code{GRASS} session.
#'
#' @seealso \code{\link[terra]{buffer}} in the \pkg{terra} package; \href{https://grass.osgeo.org/grass82/manuals/r.buffer.html}{\code{r.buffer}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterBufferRast.R
#'
#' @export

fasterBufferRast <- function(
	rast,
	inRastName,
	width,
	units = 'meters',
	ignore = NA,
	out = 'terra',
	lowMemory = FALSE,
	outGrassName = 'bufferedRast',
	
	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	outVectClass = fasterGetOptions('outVectClass', 'SpatVector'),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	### commons v1
	##############

		### arguments
		if (exists('rast', where=environment(), inherits=FALSE)) {
			inRastName <- .getInRastName(inRastName, rast)
		} else {
			rast <- inRastName <- NULL
		}

		if (exists('vect', where=environment(), inherits=FALSE)) {
			inVectName <- .getInVectName(inVectName, vect=vect)
		} else {
			vect <- inVectName <- NULL
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


	### function-specific
	fx <- if (lowMemory) {
		'r.buffer.lowmem'
	} else {
		'r.buffer'
	}

	args <- list(
		cmd = fx,
		input = inRastName,
		output = outGrassName,
		distances = width,
		units = units,
		flags = flags
	)
	args <- c(args, dots)

	out <- tolower(out)
	if (!(out %in% c('terra', 'grass', 'buffer'))) stop('Argument "out" must be "terra", "grass", or "buffer".')

	# initialize GRASS
	input <- do.call('initGrass', inits)

	### execute
	if (autoRegion) regionReshape(inRastName)

	# GRASS-style output
	if (out == 'grass') {
	
		do.call(rgrass::execGRASS, args=args)
	
	} else {
	
		# default GRASS output format
		theseArgs <- args
		theseArgs$output <- 'TEMPTEMP_bufferRast'
		theseArgs$flags <- unique(c(theseArgs$flags, 'overwrite'))

		do.call(rgrass::execGRASS, args=theseArgs)

		# terra-style output
		if (out == 'terra') {
		
			# GRASS output is 1 for buffered, 2 for the actual buffer, and NULL for everywhere else
			# ex <- paste0(outGrassName, ' = if(TEMPTEMP_bufferRast==1 || TEMPTEMP_bufferRastAsTerra==2, 1, null())')

			# ex <- '= if(isnull(TEMPTEMP_bufferRast), null(), 1)'
			ex <- '= if(isnull(TEMPTEMP_bufferRast), 0, 1)'

			fasterApp(
				'TEMPTEMP_bufferRast',
				expression = ex,
				replace = replace,
				grassToR = FALSE,
				outGrassName = outGrassName,
				autoRegion = FALSE, # already done above
				grassDir = grassDir
			)
			
		# buffer only output
		} else if (out == 'buffer') {
		
			# GRASS output is 1 for buffered, 2 for the actual buffer, and NULL for everywhere else
			ex <- '= if(TEMPTEMP_bufferRast == 2, 1, null())'
			fasterApp(
				'TEMPTEMP_bufferRast',
				expression = ex,
				replace = TRUE,
				grassDir = grassDir,
				grassToR = FALSE,
				outGrassName = outGrassName
			)
		
		}
		
	}

	### export
	if (grassToR) {

		out <- fasterWriteRaster(outGrassName, paste0(tempfile(), '.tif'), datatype='Byte', overwrite=TRUE)
		out <- terra::setMinMax(out)
		out
		
	}

}

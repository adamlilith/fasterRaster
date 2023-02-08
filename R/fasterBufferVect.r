#' Buffer a spatial vector
#'
#' Creates a buffer around a spatial vector.
#'
#' @inheritParams .sharedArgs_vect
#' @inheritParams .sharedArgs_inVectName
#' @inheritParams .sharedArgs_outVectClass
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_outGrassName
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @param width Size of the buffer in map units (usually meters). To create "inward" buffers, you can use negative numbers.
#' @param union If \code{FALSE} (default), then each input feature will have its own buffer, and attributes from each input feature will be transferred to each buffer feature. If \code{TRUE}, the union buffers of different features that intersect.
#' 
#' @return A \code{SpatVector} (\pkg{terra} package) or an object of class \code{sf} (\code{sf} package), if \code{options(grassVectOut = 'sf')}.
#'
#' @seealso \code{\link{fasterBufferRast}} in \pkg{fasterRaster}; \code{\link[terra]{buffer}} in package \pkg{terra}; \code{\link[sf]{st_buffer}} in package \pkg{sf}; \href{https://grass.osgeo.org/grass82/manuals/v.buffer.html}{\code{v.buffer}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterBufferVect.r
#'
#' @export

fasterBufferVect <- function(
	vect,
	inVectName,
	width,
	union = FALSE,
	outGrassName = 'bufferedVect',

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

		if (!missing(vect)) {
			if (!inherits(vect, 'character') & !inherits(vect, 'SpatVector')) vect <- terra::vect(vect)
			inVectName <- .getInVectName(inVectName, vect=vect)
			.checkVectExists(replace=replace, vect=vect, inVectName=inVectName, outGrassName=NULL)
		} else {
			vect <- inVectName <- NULL
		}

		### flags
		flags <- .getFlags(replace=replace)
		
		### restore
		# on.exit(.restoreLocation(), add=TRUE) # return to starting location
		if (autoRegion) on.exit(regionExt('*'), add=TRUE) # resize extent to encompass all spatials

		### ellipses and initialization arguments
		initsDots <- .getInitsDots(..., callingFx = 'fasterBufferVect')
		inits <- initsDots$inits
		dots <- initsDots$dots

	###############
	### end commons

	### function-specific
	if (!union) flags <- c(flags, 't')

	args <- list(
		cmd = 'v.buffer',
		input = inVectName,
		output = outGrassName,
		distance = width,
		flags = flags
	)
	args <- c(args, dots)

	# initialize GRASS
	input <- do.call('startFaster', inits)

	### execute
	do.call(rgrass::execGRASS, args=args)

	### export
	if (grassToR) {

		out <- rgrass::read_VECT(outGrassName, flags='quiet')
		if (outVectClass == 'sf') out <- sf::st_as_sf(out)
		out
		
	} else { invisible(TRUE) }
 
}

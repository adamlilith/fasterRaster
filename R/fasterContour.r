#' Contour vectors from a raster
#'
#' This function creates a spatial vector object from a raster representing contour lines in the raster.
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_outVectClass
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_outGrassName
#' @inheritParams .sharedArgs_autoRegion
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#' 
#' @param levels Numeric vector. Levels of values in \code{rast} at which contours should be drawn. You can specify contour levels using this argument or by providing values for \code{step}, \code{minlevel}, and \code{maxlevel}. \code{levels} will override use of \code{step}, if both of them are specified.
#' @param cut Integer >= 0. Minimum number of points necessary to generate a contour line. A value of 0 implies no limit. Default is 2.
#'
#' @return If \code{grassToR} is \code{TRUE}, then this function returns either a \code{SpatVector} or \code{sf} object, depending on the value of \code{outVectClass}. Regardless, a vector is written into the \code{GRASS} session. The name of this vector in \code{GRASS} is given by \code{outGrassName}.
#'
#' @seealso \code{\link[terra]{as.contour}} in \pkg{terra}; \code{GRASS} module \code{\href{https://grass.osgeo.org/grass82/manuals/r.contour.html}{r.contour}}
#'
#' @example man/examples/ex_fasterContour.R
#'
#' @export

fasterContour <- function(
	rast,
	inRastName,
	levels,
	cut = 2,
	outGrassName = 'contourVect',

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
		.checkRastExists(replace=replace, rast=NULL, inRastName=NULL, outGrassName=outGrassName, ...)
		if (!missing(rast)) {
			if (!inherits(rast, 'character') & !inherits(rast, 'SpatRaster')) rast <- terra::rast(rast)
			inRastName <- .getInRastName(inRastName, rast=rast)
			.checkRastExists(replace=replace, rast=rast, inRastName=inRastName, outGrassName=NULL, ...)
		} else {
			rast <- inRastName <- NULL
		}
		
		.checkVectExists(replace=replace, vect=NULL, inVectName=NULL, outGrassName=outGrassName, ...)

		### flags
		flags <- .getFlags(replace=replace)
		
		### restore
		# on.exit(.restoreLocation(), add=TRUE) # return to starting location
		if (autoRegion) on.exit(regionExt('*'), add=TRUE) # resize extent to encompass all spatials

		### ellipses and initialization arguments
		initsDots <- .getInitsDots(..., callingFx = 'fasterContour')
		inits <- initsDots$inits
		dots <- initsDots$dots

	###############
	### end commons

	### function-specific
	if (!is.numeric(levels)) stop('Argument "levels" must be a numeric vector.')

	args <- list(
		cmd = 'r.contour',
		input = inRastName,
		output = outGrassName,
		levels = levels,
		cut = cut,
		flags = flags
	)
	args <- c(args, dots)
	
	### initialize GRASS
	input <- do.call('startFaster', inits)

	### execute
	do.call(rgrass::execGRASS, args=args)
	
	if (grassToR) {

		out <- rgrass::read_VECT(outGrassName, flags='quiet')
		if (outVectClass == 'sf') out <- sf::st_as_sf(out)
		out
		
	} else { invisible(TRUE) }
	
}

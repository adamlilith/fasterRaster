#' Minimum convex hull around a spatial vector
#'
#' Create a minimum convex hull around a spatial vector.
#'
#' @inheritParams .sharedArgs_vect
#' @inheritParams .sharedArgs_inVectName
#' @inheritParams .sharedArgs_outVectClass
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_outGrassName
#' @inheritParams .sharedArgs_autoRegion
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @return A \code{SpatVector} (\pkg{terra} package) or an object of class \code{sf} (\code{sf} package), if \code{options(grassVectOut = 'sf')}.
#'
#' @seealso \code{\link[terra]{convHull}} in package \pkg{terra}; \code{\link[sf]{st_convex_hull}} in package \pkg{sf}; \href{https://grass.osgeo.org/grass82/manuals/v.hull.html}{\code{v.hull}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterPoints.r
#'
#' @export

fasterConvHull <- function(
	vect,
	inVectName,
	outGrassName = 'convHullVect',

	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	outVectClass = fasterGetOptions('outVectClass', 'SpatVector'),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	### commons v1
	##############

		.checkVectExists(replace=replace, vect=NULL, inVectName=NULL, outGrassName=outGrassName, ...)
		if (!missing(vect)) {
			if (!inherits(vect, 'character') & !inherits(vect, 'SpatVector')) vect <- terra::vect(vect)
			inVectName <- .getInVectName(inVectName, vect=vect)
			.checkVectExists(replace=replace, vect=vect, inVectName=inVectName, outGrassName=NULL, ...)
		} else {
			vect <- inVectName <- NULL
		}

		### flags
		flags <- .getFlags(replace=replace)
		
		### restore
		# on.exit(.restoreLocation(), add=TRUE) # return to starting location
		# if (autoRegion) on.exit(regionExt('*'), add=TRUE) # resize extent to encompass all spatials

		### ellipses and initialization arguments
		initsDots <- .getInitsDots(..., callingFx = 'fasterConvHull')
		inits <- initsDots$inits
		dots <- initsDots$dots

	###############
	### end commons

	### function-specific
	args <- list(
		cmd = 'v.hull',
		input = inVectName,
		output = outGrassName,
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


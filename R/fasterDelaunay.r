#' Delaunay triangulation for points
#'
#' This function creates a Delaunay triangulation spatial vector from a set of spatial points.
#'
#' @inheritParams .sharedArgs_inVectName
#' @inheritParams .sharedArgs_outVectClass
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_outGrassName
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @param vect A \code{SpatVector}, an \code{sf} object, or the name of a spatial vector in the active \code{GRASS} session. This must be a "points" vector.
#'
#' @return A \code{SpatVector} (\pkg{terra} package) or an object of class \code{sf} (\code{sf} package), if \code{options(grassVectOut = 'sf')}.
#'
#' @seealso \code{\link{fasterVoronoi}} in \pkg{fasterRaster}; \code{\link[terra]{delaunay}} in package \pkg{terra}; \code{\link[sf]{st_triangulate}} in package \pkg{sf}; \href{https://grass.osgeo.org/grass82/manuals/v.delaunay.html}{\code{v.delaunay}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterPoints.r
#'
#' @export

fasterDelaunay <- function(
	vect,
	inVectName,
	# regionMult = 0.1,
	outGrassName = 'delaunayVect',

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
		if (autoRegion) on.exit(regionExt('*'), add=TRUE) # resize extent to encompass all spatials

		### ellipses and initialization arguments
		initsDots <- .getInitsDots(..., callingFx = 'fasterDelaunay')
		inits <- initsDots$inits
		dots <- initsDots$dots

	###############
	### end commons

	### errors?
	if (inherits(vect, 'SpatVector') && terra::geomtype(vect) != 'points') stop('Input vector must represent points.')
	if (inherits(vect, 'sf')) {
	
		type <- sf::st_geometry_type(vect)
		type <- as.character(type)
		if (!all(type %in% c('POINT', 'MULTIPOINT'))) stop('Input vector must represent points.')
		
	}

	# ### function-specific
	# flags <- c(flags, 'r') # use only points in current region
	
	args <- list(
		cmd = 'v.delaunay',
		input = inVectName,
		output = outGrassName,
		flags = flags,
		intern = TRUE
	)
	args <- c(args, dots)

	### initialize GRASS
	input <- do.call('startFaster', inits)

	### execute
	
	# # expand/shrink region
	# if (regionMult < 0) {
		# warning('Extent multipler is <0. Some points in the vector will be excluded from triangulation.')
	# } else if (regionMult != 0) {
		
		# extent <- fasterExt(inVectName, rastOrVect='vector')
		
		# if (length(regionMult) == 1L) regionMult <- rep(regionMult, 2L)
		
		# xx <- extent[2L] - extent[1L]
		# yy <- extent[4L] - extent[3L]
		
		# xDelta <- regionMult[1L] * xx
		# yDelta <- regionMult[2L] * yy
		
		# extent[1L] <- extent[1L] - xDelta
		# extent[2L] <- extent[2L] + xDelta
		# extent[3L] <- extent[3L] - yDelta
		# extent[4L] <- extent[4L] + yDelta
		
		# regionExt(x=extent)
		
	# }
	
	do.call(rgrass::execGRASS, args=args)

	### export
	if (grassToR) {

		out <- rgrass::read_VECT(outGrassName, flags='quiet')
		if (outVectClass == 'sf') out <- sf::st_as_sf(out)
		out

	} else { invisible(TRUE) }
	
}

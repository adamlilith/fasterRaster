#' Delauney triangulation for points
#'
#' This function creates a Delaunay triangulation spatial vector from a set of spatial points.
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
	outGrassName = 'delaunayVect',

	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	outVectClass = fasterGetOptions('outVectClass', 'SpatVector'),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	### begin common
	flags <- .getFlags(replace=replace)
	# inRastName <- .getInRastName(inRastName, rast)
	if (is.null(inVectName)) inVectName <- 'vect'
	
	# region settings
	success <- .rememberRegion()
	# on.exit(.restoreRegion(), add=TRUE)
	on.exit(regionResize(), add=TRUE)
	
	if (is.null(inits)) inits <- list()
	### end common

	inits <- c(inits, list(rast=NULL, vect=vect, inRastName=NULL, inVectName=inVectName, replace=replace, grassDir=grassDir))
	input <- do.call('initGrass', inits)
	
	# execute
	rgrass::execGRASS(
		'v.delaunay',
		input = inVectName,
		output = outGrassName,
		flags = flags
	)

	# return
	if (grassToR) {

		out <- rgrass::read_VECT(outGrassName, flags='quiet')
		if (!is.null(options()$grassVectOut) && !is.na(options()$grassVectOut)) {
			if (options()$grassVectOut == 'sf') out <- sf::st_as_sf(out)
		}
		out

	}

	# remember starting region
	success <- regionResize()
	

}


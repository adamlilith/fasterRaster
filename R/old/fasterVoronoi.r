#' Voronoi diagrams for points or polygons
#'
#' This function creates a Voronoi diagram spatial vector from a set of spatial points or polygons.
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
#' @param type Either \code{'points'} (default) or \code{'polygons'}, indicating the type of vector in \code{vect} (lines are not supported).
#'
#' @return A \code{SpatVector} (\pkg{terra} package) or an object of class \code{sf} (\code{sf} package), if \code{options(grassVectOut = 'sf')}.
#'
#' @seealso \code{\link{fasterDelaunay}} in \pkg{fasterRaster}; \code{\link[terra]{voronoi}} in package \pkg{terra}; \code{\link[sf]{st_voronoi}} in package \pkg{sf}; \href{https://grass.osgeo.org/grass82/manuals/v.voronoi.html}{\code{v.voronoi}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterPoints.r
#'
#' @export

fasterVoronoi <- function(
	vect,
	inVectName,
	type = 'points',
	outGrassName = 'voronoiVect',
	
	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	outVectClass = fasterGetOptions('outVectClass', 'SpatVector'),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	flags <- .getFlags(replace=replace)
 
	# initialize GRASS and export raster to GRASS
	if (is.null(inVectName)) inVectName <- 'vect'

	if (is.null(inits)) inits <- list()
	inits <- c(inits, list(rast=NULL, vect=vect, inRastName=NULL, inVectName=inVectName, replace=replace, grassDir=grassDir))
	input <- do.call('startFaster', inits)
	
	# execute
	type <- tolower(type)
	if (pmatch(type, c('points', 'polygons')) == 2) flags <- c(flags, 'a')
	rgrass::execGRASS(
		'v.voronoi',
		input = inVectName,
		output = outGrassName,
		flags = flags
	)

	# resize region to encompass all
	success <- resizeRegion()

	# return
	if (grassToR) {

		out <- rgrass::read_VECT(outGrassName, flags='quiet')
		if (.getOutVectClass(outVectClass) == 'sf') {
			if (options()$grassVectOut == 'sf') out <- sf::st_as_sf(out)
		}
		out

	} else { invisible(TRUE) }


}


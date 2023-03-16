#' Create a spatial grid of hexagons
#'
#' This function creates a hexagonal grid over the region of interest. The hexagons are spatial vectors. Using \code{\link{fasterPointsInGrid}}, you can enumerate the number of points in each hexagon.
#'
#' @inheritParams .sharedArgs_vect
#' @inheritParams .sharedArgs_inVectName
#' @inheritParams .sharedArgs_outVectClass
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_outGrassName
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule

fasterHexagonGrid <- function(
	vect,
	inVectName,
	outGrassName = 'hexagonVector',

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
	
	# # region settings
	# success <- .rememberRegion()
	# on.exit(.restoreRegion(), add=TRUE)
	# on.exit(regionResize(), add=TRUE)
	
	if (is.null(inits)) inits <- list()
	### end common

	inits <- c(inits, list(rast=NULL, vect=vect, inRastName=NULL, inVectName=inVectName, replace=replace, grassDir=grassDir))
	input <- do.call('startFaster', inits)
	
	success <- regionResize()

# TBD
}

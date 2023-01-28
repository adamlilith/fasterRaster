#' Longitude and latitude rasters
#'
#' Create two rasters, one with cell values equal to cell latitude and the other cell longitude (in geographic, or unprojected coordinates).
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @param mask Logical. If \code{TRUE} (default), cells that have \code{NA}s in \code{rast} will also have \code{NA}s in the output rasters.
#' @param outGrassName Character vector with two elements, one for longitude and one for latitude.  Name of output in \pkg{GRASS}. This is useful if you want to refer to the output objects in \code{GRASS} later in a \code{GRASS} session.
#'
#' @return If \code{grassToR} if \code{TRUE}, then a \code{SpatRaster} stack with the same extent, resolution, and coordinate reference system as \code{rast}. Regardless, rasters with names given by \code{outGrassName} are written into the \code{GRASS} session. Values represent cells' centroid coordinates in unprojected (WGS84) format.
#'
#' @seealso \href{https://grass.osgeo.org/grass82/manuals/r.latlong.html}{\code{r.latlong}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterLongLatRasts.r
#'
#' @export

fasterLongLatRasts <- function(
	rast,
	inRastName,
	mask = TRUE,
	outGrassName = c('longitudeRast', 'latitudeRast'),

	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	### begin common
	flags <- .getFlags(replace=replace)
	inRastName <- .getInRastName(inRastName, rast)
	# if (is.null(inVectName)) inVectName <- 'vect'
	
	# # region settings
	# success <- .rememberRegion()
	# on.exit(.restoreRegion(), add=TRUE)
	# on.exit(regionResize(), add=TRUE)
	
	if (is.null(inits)) inits <- list()
	### end common

	inits <- c(inits, list(rast=rast, vect=NULL, inRastName=inRastName, inVectName=NULL, replace=replace, grassDir=grassDir))
	input <- do.call('initGrass', inits)
	
	# calculate longitude/latitude
	rgrass::execGRASS('r.latlong', input=input, output=outGrassName[1], flags=c(flags, 'l'))
	rgrass::execGRASS('r.latlong', input=input, output=outGrassName[2], flags=flags)
	
	if (mask) {

		ex <- paste0(outGrassName[1L], ' = if(isnull(', inRastName, '), null(), ', outGrassName[1L], ')')
		fasterApp(outGrassName[1L], expression=ex, replace=replace, grassDir=grassDir, grassToR=FALSE)
		
		ex <- paste0(outGrassName[2L], ' = if(isnull(', inRastName, '), null(), ', outGrassName[2L], ')')
		fasterApp(outGrassName[2L], expression=ex, replace=replace, grassDir=grassDir, grassToR=FALSE)
		
	}
	
	if (grassToR) {
		long <- fasterWriteRaster(outGrassName[1L], paste0(tempfile(), '.tif'), overwrite=TRUE)
		lat <- fasterWriteRaster(outGrassName[2L], paste0(tempfile(), '.tif'), overwrite=TRUE)
		out <- c(long, lat)
		names(out) <- outGrassName
		out
	}

}

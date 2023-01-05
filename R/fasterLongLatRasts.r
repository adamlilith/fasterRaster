#' Longitude and latitude rasters
#'
#' Create two rasters, one with cell values equal to cell latitude and the other cell longitude (in geographic, or unprojected coordinates).
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_grassDir_grassToR
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
	mask = TRUE,
	grassDir = options()$grassDir,
	grassToR = TRUE,
	inRastName = 'rast',
	outGrassName = c('longitudeRast', 'latitudeRast')
) {

	flags <- c('quiet', 'overwrite')
	
	# initialize GRASS
	inRastName <- .getInRastName(inRastName, rast)
	input <- initGrass(rast=rast, vect=NULL, inRastName=inRastName, inVectName=NULL, grassDir=grassDir)
	
	# calculate longitude/latitude
	rgrass::execGRASS('r.latlong', input=input, output=outGrassName[1], flags=c(flags, 'l'))
	rgrass::execGRASS('r.latlong', input=input, output=outGrassName[2], flags=flags)
	
	if (mask) {

		ex <- paste0(outGrassName[1L], ' = if(isnull(', inRastName, '), null(), ', outGrassName[1L], ')')
		fasterApp(outGrassName[1L], expression=ex, grassDir=grassDir, grassToR=FALSE)
		
		ex <- paste0(outGrassName[2L], ' = if(isnull(', inRastName, '), null(), ', outGrassName[2L], ')')
		fasterApp(outGrassName[2L], expression=ex, grassDir=grassDir, grassToR=FALSE)
		
	}
	
	if (grassToR) {
		long <- rgrass::read_RAST(outGrassName[1L])
		lat <- rgrass::read_RAST(outGrassName[2L])
		out <- c(long, lat)
		names(out) <- outGrassName
		out
	}

}

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
#' @param ... Arguments to pass to \code{\link[rgrass]{execGRASS}} when used for rasterizing (i.e., function \code{r.latlong} in \code{GRASS}).
#'
#' @return If \code{grassToR} if \code{TRUE}, then a raster stack with the same extent, resolution, and coordinate reference system as \code{rast}. Regardless, rasters with names given by \code{outGrassName} are written into the \code{GRASS} session.
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
	inRastName = ifelse(is.null(names(rast)), 'rast', names(rast)),
	outGrassName = c('longitude', 'latitude'),
	...
) {

	flags <- c('quiet', 'overwrite')
	
	# initialize GRASS
	input <- initGrass(rast=rast, vect=NULL, inRastName=inRastName, inVectName=NULL, grassDir=grassDir)
	
	# calculate longitude/latitude
	rgrass::execGRASS('r.latlong', input=input, output=outGrassName[1], flags=c(flags, 'l'), ...)
	rgrass::execGRASS('r.latlong', input=input, output=outGrassName[2], flags=flags, ...)
	
	if (mask) {
		fasterApp('rast', expression='mask = rast * 0 + 1', grassDir=grassDir, grassToR=FALSE)
		ex <- paste0(outGrassName[1], ' = ', outGrassName[1], ' * mask')
		fasterApp(outGrassName[1], expression=ex, grassDir=grassDir, grassToR=FALSE)
		ex <- paste0(outGrassName[1], ' = ', outGrassName[2], ' * mask')
		fasterApp(outGrassName[2], expression=ex, grassDir=grassDir, grassToR=FALSE)
	}
	
	if (grassToR) {
		long <- rgrass::read_RAST(outGrassName[1])
		lat <- rgrass::read_RAST(outGrassName[2])
		long <- terra::rast(long)
		lat <- terra::rast(lat)
		out <- c(long, lat)
		# if (mask) out <- (rast * 0L + 1L) * out
		names(out) <- outGrassName
		out
	}

}

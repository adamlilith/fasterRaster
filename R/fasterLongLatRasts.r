#' Longitude and latitude rasters
#'
#' This function is a potentially faster version of the \code{\link[enmSdm]{longLatRasters}} function in the \pkg{enmSdm} package which calculates two rasters, one with cell values equaling the longitude of their centers and the other with cell values equaling the latitude of their centers. Please note that the raster must be projected (i.e., not in a geographic coordinate system, not in WGS84, NAD83, etc.) for the \code{GRASS}7 \code{r.longlat} module, on which this function is based, to work.
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_grassDir_grassToR
#' @param mask Logical. If \code{TRUE} (default), cells that have \code{NA}s in \code{rast} will also have \code{NA}s in the output rasters.
#' @param outGrassName Character vector with two elements, one for longitude and one for latitude.  Name of output in \pkg{GRASS}. This is useful if you want to refer to the output objects in \code{GRASS} later in a \code{GRASS} session.
#' @param ... Arguments to pass to \code{\link[rgrass]{execGRASS}} when used for rasterizing (i.e., function \code{r.latlong} in \code{GRASS}).
#'
#' @return If \code{grassToR} if \code{TRUE}, then a raster stack with the same extent, resolution, and coordinate reference system as \code{rast}. Regardless, rasters with names given by \code{outGrassName} are written into the \code{GRASS} session.
#'
#' @details See the documentation for the \code{GRASS} module \code{r.longlat}{https://grass.osgeo.org/grass82/manuals/r.longlat.html}.
#'
#' @examples man/examples/ex_fasterLongLatRasts.r
#'
#' @export

fasterLongLatRasts <- function(
	rast,
	mask = TRUE,
	grassDir = options()$grassDir,
	grassToR = TRUE,
	outGrassName = c('longitude', 'latitude'),
	...
) {

	flags <- c('quiet', 'overwrite')
	
	# initialize GRASS
	input <- initGrass(rast=rast, vect=NULL, grassDir=grassDir)
	
	# calculate longitude/latitude
	rgrass::execGRASS('r.latlong', input=input, output=outGrassName[1], flags=c(flags, 'l'), ...)
	rgrass::execGRASS('r.latlong', input=input, output=outGrassName[2], flags=flags, ...)
	
	if (mask) {
		fasterMapcalc('rast', expression='mask = rast * 0 + 1', grassDir=grassDir, alreadyInR=TRUE, grassToR=FALSE)
		ex <- paste0(outGrassName[1], ' = ', outGrassName[1], ' * mask')
		fasterMapcalc(outGrassName[1], expression=ex, grassDir=grassDir, alreadyInR=TRUE, grassToR=FALSE)
		ex <- paste0(outGrassName[1], ' = ', outGrassName[2], ' * mask')
		fasterMapcalc(outGrassName[2], expression=ex, grassDir=grassDir, alreadyInR=TRUE, grassToR=FALSE)
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

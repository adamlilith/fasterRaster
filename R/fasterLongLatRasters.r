#' Longitude and latitude rasters
#'
#' This function is a potentially faster version of the \code{\link[enmSdm]{longLatRasters}} function in the \pkg{enmSdm} package which calculates two rasters, one with cell values equaling the longitude of their centers and the other with cell values equaling the latitude of their centers. Please note that the raster must be projected (i.e., not in a geographic coordinate system, not in WGS84, NAD83, etc.) for the GRASS7 \code{r.longlat} module, on which this function is based, to work.
#' @param rast Either a raster or the name of a raster in an existing GRASS session. This serves as a template for calculating longitude and latitude rasters.
#' @param mask Logical. If \code{TRUE} (default), cells that have \code{NA}s in \code{rast} will also have \code{NA}s in the output rasters.
#' @param grassDir Character or \code{NULL} (default). Name of the directory in which GRASS is installed. Example for a Windows system: \code{'C:/Program Files/GRASS GIS 8.2'}. If this is \code{NULL}, R will search for the directory in which GRASS is installed. This usually fails, or if it succeeds, takes several minutes.
#' @param alreadyInGrass Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{rast}. If \code{FALSE}, use a raster already in GRASS with the name given by \code{rast}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#' @param grassToR Logical, if \code{TRUE} (default) then the output will be returned to R. If \code{FALSE}, then the output is left in the GRASS session and named the value in \code{outGrassName} \code{longitude} and \code{latitude}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param outGrassName Character vector with two elements, one for longitude and one for latitude.  Name of output in GRASS. This is useful if you want to refer to the output objects in GRASS later in a session.
#' @param ... Arguments to pass to \code{\link[rgrass]{execGRASS}} when used for rasterizing (i.e., function \code{r.latlong} in GRASS).
#' @return If \code{grassToR} if \code{TRUE}, then a raster stack with the same extent, resolution, and coordinate reference system as \code{rast}. Regardless, rasters with names given by \code{outGrassName} are written into the GRASS session.
#' @details See the documentation for the GRASS module \code{r.longlat}{https://grass.osgeo.org/grass82/manuals/r.longlat.html}.
#' @seealso \code{\link[enmSdm]{longLatRasters}}
#' @examples
#' \donttest{
#' # change this to where GRASS 7 is installed on your system
#' grassDir <- 'C:/Program Files/GRASS GIS 8.2'
#' 
#' # note that in the example below using enmSdm::longLatRasters()
#' # will be *much* faster than using fasterLongLatRasts()
#' # because the template raster is so small
#' 
#' data(madForest2000)
#' unMasked <- fasterLongLatRasts(rast=madForest2000, mask=FALSE,
#' grassDir=grassDir)
#' plot(unMasked)
#' masked <- fasterLongLatRasts(rast=madForest2000, grassDir=grassDir)
#' plot(masked)
#' # same as (and slower than, for this raster):
#' # ll3 <- enmSdm::longLatRasters(madForest2000)
#' # plot(ll3)
#' 
#' }
#' @export

fasterLongLatRasts <- function(
	rast,
	mask = TRUE,
	grassDir = options('grassDir'),
	alreadyInGrass = FALSE,
	grassToR = TRUE,
	outGrassName = c('longitude', 'latitude'),
	...
) {

	flags <- c('quiet', 'overwrite')
	
	# initialize GRASS
	input <- initGrass(alreadyInGrass, rast=rast, vect=NULL, grassDir=grassDir)
	
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
		long <- raster::raster(long)
		lat <- raster::raster(lat)
		out <- raster::stack(long, lat)
		# if (mask) out <- (rast * 0L + 1L) * out
		names(out) <- outGrassName
		out
	}

}

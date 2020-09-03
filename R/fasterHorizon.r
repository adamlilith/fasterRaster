#' Calculate horizon angle height raster
#'
#' This function calculates a raster where values represent the height of the horizon for any cell in a particular direction. Height is given in radians (default) or degrees. It utilizes the GRASS function \code{r.horizon}.
#' @param rast Either a raster or the name of a raster in an existing GRASS session with values representing elevation (typically in meters).
#' @param units Either \code{'radians'} (default) or \code{'degrees'}.
#' @param directions Numeric vector. Direction(s) in which to calculate horizon height for each cell. By default, these are given in degrees clockwise from 0, so 0 is north, 90 east, 180 south, and 270 west. However, if you define \code{northIs0 = FALSE}, then the directions are given degrees counterclockwise from east, so east is 0, north 90, west 180, and south 270. Regardless, the default is to calculate horizon angle in all four directions. One raster is created per direction. Note that the output will be labeled according to the angle of the directions (e.g., \code{horizonHeight_090} will be horizon height facing east if \code{northIs0 = TRUE} (default), but horizon height facing north if \code{northIs0 = FALSE}.
#' @param northIs0 Logical. If \code{TRUE} (default), argument \code{directions} specifies horizon height clockwise from 0, so 0 is north, 90 east, 180 south, and 270 west. If \code{FALSE}, angles are counterclockwise from east, so east is 0, north 90, west 180, and south 270. The latter is teh default for the GRASS function \code{r.horizon}.
#' @param bufferZone Numeric >= 0 (default is 0). A buffer of the specified width will be generated around the raster before calculation of horizon angle. If the coordinate system is in longitude/latitude (e.g., WGS84 or NAD83), then this is specified in degrees. Otherwise units are map units (usually meters).
#' @param maxDist Either \code{NULL} (default) or numeric >= 0. Maximum distance to consider when finding horizon height. If \code{NULL} (default), the maximum distance is the full extent of the raster. Smaller values can decrease run time but also reduce accuracy.
#' @param distance Numeric in the range [0.5, 1.5] (default is 1). This determines the step size when searching for the horizon from a given point. The default value of 1 goes cell-by-cell (i.e., search distance step size is one cell width).
#' @param grassDir Character or \code{NULL} (default). Name of the directory in which GRASS is installed. Example: \code{'C:/Program Files/GRASS GIS 7.8'}. If this is \code{NULL}, R will search for the directory in which GRASS is installed. This usually fails, or if it succeeds, takes several minutes.
#' @param alreadyInGrass Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{rast}. If \code{FALSE}, use a raster already in GRASS with the name given by \code{rast}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#' @param grassToR Logical, if \code{TRUE} (default) then the product of the calculations will be returned to R. If \code{FALSE}, then the product is left in the GRASS session and named \code{longitude} and \code{latitude}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param outGrassName Character. Name of output in GRASS. This is useful if you want to refer to the output object in GRASS later in a session.
#' @param ... Arguments to pass to \code{\link[rgrass7]{execGRASS}} when calculating horizon height (i.e., function \code{r.horizon} in GRASS).
#' @return If \code{grassToR} if \code{TRUE}, then a raster or raster stack stack with the same extent, resolution, and coordinate reference system as \code{rast}. Otherwise, a raster is written into the GRASS session. The name of this raster is as \code{paste0(outGrassName, '_', xxx)}. For example, if \code{outGrassName = 'horizonAngleHeight'}, and \code{directions} is \code{c(0, 90, 180, 270)}, then four rasters will be written: \code{horizAngleHeight_000}, \code{horizAngleHeight_090}, \code{horizAngleHeight_180}, and \code{horizAngleHeight_270}. Note the padding with zeros before angles <10.
#' @details See (r.horizon)[https://grass.osgeo.org/grass78/manuals/r.horizon.html] for more details. Note that if you get an error saying "", then you should add the EPSG code to the beginning of the raster coordinate reference system string (its "proj4string"). For example, \code{proj4string(rast) <- CRS('+init=epsg:32738')}. EPSG codes for various projections, datums, and locales can be found at (Spatial Reference)[http://spatialreference.org/].
#' @seealso
#' @examples
#' \donttest{
#' # change this to where GRASS 7 is installed on your system
#' grassDir <- 'C:/Program Files/GRASS GIS 7.8'
#'
#' data(madElev)
#' elevHeight_rad <- fasterHorizon(madElev, grassDir=grassDir)
#' plot(elevHeight_rad)
#' }
#' @export

fasterHorizon <- function(
	rast,
	units = 'radians',
	directions = c(0, 90, 180, 270),
	northIs0 = TRUE,
	bufferZone = 0,
	maxDist = NULL,
	grassDir = NULL,
	alreadyInGrass = FALSE,
	grassToR = TRUE,
	outGrassName = 'horizonAngleHeight',
	...
) {

	flags <- c('quiet', 'overwrite')
	if (units == 'degrees') flags <- c(flags, 'd')
	
	# initialize GRASS
	input <- initGrass(alreadyInGrass, rast=rast, vect=NULL, grassDir=grassDir)
	
	# calculate horizon height
	for (direction in directions) {

		if (is.null(maxDist)) {
			rgrass7::execGRASS('r.horizon', elevation=input, direction=direction, bufferzone=bufferZone, output=outGrassName, flags=flags, ...)
		} else {
			rgrass7::execGRASS('r.horizon', elevation=input, direction=direction, bufferzone=bufferZone, maxdistance=maxDist, output=outGrassName, flags=flags, ...)
		}

	}

	if (grassToR) {

		for (direction in directions) {
	
			thisOut <- rgrass7::readRAST(paste0(outGrassName, '_', omnibus::prefix(direction, 3)))
			thisOut <- raster::raster(thisOut)
			
			out <- if (exists('out', inherits=FALSE)) {
				raster::stack(out, thisOut)
			} else {
				thisOut
			}
			
		}

		if (northIs0) directions <- fasterConvertDegree(directions)

		angles <- paste0(ifelse(directions < 100, '0', ''), ifelse(directions < 10, '0', ''), directions)
		names(out) <- paste0(outGrassName, '_', angles)
		out <- out[[order(names(out))]]
		out
		
	}
	
}

#' Calculate raster slope, aspect, curvature, and partial slopes.
#'
#' This function is a potentially faster version of the \code{\link[raster]{terrain}} function in the \pkg{raster} package for calculating slope and aspect of a raster. It can also calculate profile curvature, tangential curvature, and slope in the east-west or north-south directions.
#' @param rast Either a raster with elevation data or the name of a raster in an existing GRASS session.
#' @param slope Logical, if \code{TRUE} (default) then calculate slope.
#' @param slopeUnits Character, "units" in which to calculate slope: either \code{degrees} for degrees or \code{percent}.
#' @param aspect Logical, if \code{TRUE} then calculate aspect. Aspect is given in degrees from North going clockwise (0 = north, 90 = east, 180 = south, 270 = west).
#' @param northIs0 Logical. If \code{TRUE} (default), aspect will be reported such that 0 is north, and degrees run clockwise (90 is east, 180 south, 270 west). If \code{FALSE}, then aspect will be reported such that 0 is east, and degrees run counterclockwise (90 is north, 180 west, 270 south). The latter is the default in GRASS7.
#' @param profileCurve Logical, if \code{TRUE}, calculate profile curvature. Default is \code{FALSE}.
#' @param tanCurve Logical, if \code{TRUE}, calculate tangential curvature. Default is \code{FALSE}.
#' @param eastWestSlope Logical, if \code{TRUE}, calculate slope in east-west direction. Default is \code{FALSE}.
#' @param northSouthSlope Logical, if \code{TRUE}, calculate slope in north-south direction. Default is \code{FALSE}.
#' @param grassDir Character or \code{NULL} (default). Name of the directory in which GRASS is installed. Example: \code{'C:/Program Files/GRASS GIS 7.8'}. If this is \code{NULL}, R will search for the directory in which GRASS is installed. This usually fails, or if it succeeds, takes several minutes.
#' @param alreadyInGrass Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{rast}. If \code{FALSE}, use a raster already in GRASS with the name given by \code{rast}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#' @param grassToR Logical, if \code{TRUE} (default) then the product of the calculations will be returned to R. If \code{FALSE}, then the product is left in the GRASS session and named \code{slope}, \code{aspect}, \code{profileCurve}, \code{tanCurve}, \code{eastWestSlope}, or \code{northSouthSlope}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param ... Arguments to pass to \code{\link[rgrass7]{execGRASS}} when used for rasterizing (i.e., function \code{r.slope.aspect} in GRASS).
#' @return If \code{grassToR} if \code{TRUE}, then a raster or raster stack with the same extent, resolution, and coordinate reference system as \code{rast}. Otherwise, raster(s) with the name(s) \code{slope}, \code{aspectNorthIs0} or \code{aspectEastIs0} depending on whether \code{northIs0} is \code{TRUE} or \code{FALSE}), \code{profileCurve}, \code{tanCurve}, \code{eastWestSlope}, or \code{northSouthSlope} are written into the GRASS session.
#' @details See \href{r.slope.aspect}{https://grass.osgeo.org/grass78/manuals/r.slope.aspect.html} for more details.  Note that if you get an error saying "", then you should add the EPSG code to the beginning of the raster and vector coordinate reference system string (its "proj4string"). For example, \code{proj4string(x) <- CRS('+init=epsg:32738')}. EPSG codes for various projections, datums, and locales can be found at \href{Spatial Reference}{http://spatialreference.org}.
#' @seealso \code{\link[raster]{terrain}}
#' @examples
#' \donttest{
#' # change this according to where GRASS 7 is installed on your system
#' grassDir <- 'C:/Program Files/GRASS GIS 7.8'
#' 
#' data(madElev)
#' 
#' # could also use terrain() which may be faster
#' # in this example
#' topo <- fasterTerrain(rast=madElev, slope = TRUE, aspect=TRUE,
#' grassDir=grassDir)
#'
#' # terrainn function from the raster package... much slower in this example
#' # slp <- terrain(elev, opt='slope', unit='degrees')
#' # asp <- terrain(elev, opt='aspect', unit='degrees')
#' # topo <- stack(slp, asp)
#' # names(topo) <- c('slope', 'aspect')
#' plot(topo)
#' }
#' @export

fasterTerrain <- function(
	rast,
	slope = TRUE,
	slopeUnits = 'degrees',
	aspect = FALSE,
	northIs0 = TRUE,
	profileCurve = FALSE,
	tanCurve = FALSE,
	eastWestSlope = FALSE,
	northSouthSlope = FALSE,
	grassDir = NULL,
	alreadyInGrass = FALSE,
	grassToR = TRUE,
	...
) {

	flags <- c('quiet', 'overwrite')
	
	# initialize GRASS
	input <- initGrass(alreadyInGrass, rast=rast, vect=NULL, grassDir=grassDir)

	# slope
	if (slope) rgrass7::execGRASS('r.slope.aspect', elevation=input, slope='slope', format=slopeUnits, flags=flags)
	
	# aspect (0 = east and goes counter-clockwise, so convert so 0 = north going clockwise)
	if (aspect) {
		rgrass7::execGRASS('r.slope.aspect', elevation=input, aspect='aspectEastIs0', flags=flags)
		if (northIs0) rgrass7::execGRASS('r.mapcalc', expression='aspectNorthIs0 = (360 - aspectEastIs0) % 360', flags=flags)
	}
	
	# curvatures
	if (profileCurve) rgrass7::execGRASS('r.slope.aspect', elevation=input, pcurvature='profileCurve', flags=flags)
	if (tanCurve) rgrass7::execGRASS('r.slope.aspect', elevation=input, tcurvature='tanCurve', flags=flags)
	
	# first-derivative slopes
	if (eastWestSlope) rgrass7::execGRASS('r.slope.aspect', elevation=input, dx='eastWestSlope', flags=flags)
	if (northSouthSlope) rgrass7::execGRASS('r.slope.aspect', elevation=input, dy='northSouthSlope', flags=flags)

	# return
	if (grassToR) {
	
		out <- rast
	
		if (slope) out <- raster::stack(out, raster::raster(rgrass7::readRAST('slope')))
		if (aspect & northIs0) out <- raster::stack(out, raster::raster(rgrass7::readRAST('aspectNorthIs0')))
		if (aspect & !northIs0) out <- raster::stack(out, raster::raster(rgrass7::readRAST('aspectEastIs0')))
		if (profileCurve) out <- raster::stack(out, raster::raster(rgrass7::readRAST('profileCurve')))
		if (tanCurve) out <- raster::stack(out, raster::raster(rgrass7::readRAST('tanCurve')))
		if (eastWestSlope) out <- raster::stack(out, raster::raster(rgrass7::readRAST('eastWestSlope')))
		if (northSouthSlope) out <- raster::stack(out, raster::raster(rgrass7::readRAST('northSouthSlope')))

		name <- c(
			ifelse(slope, 'slope', NA),
			ifelse(aspect, ifelse(northIs0, 'aspectNorthIs0', 'aspectEastIs0'), NA),
			ifelse(profileCurve, 'profileCurve', NA),
			ifelse(tanCurve, 'tanCurve', NA),
			ifelse(eastWestSlope, 'eastWestSlope', NA),
			ifelse(northSouthSlope, 'northSouthSlope', NA)
		)
		
		name <- stats::na.omit(name)
		
		out <- raster::subset(out, 2:raster::nlayers(out))
		names(out) <- name
		
		out
		
	}
	
}

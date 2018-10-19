#' Calculate raster slope, aspect, curvature, and partial slopes.
#'
#' This function is a potentially faster version of the \code{\link[raster]{terrain}} function in the \pkg{raster} package for calculating slope and aspect of a raster. It can also calculate profile curvature, tangential curvature, and slope in the east-west or north-south directions.
#' @param rast Either a raster or the full path and name of a raster object.
#' @param slope Logical, if \code{TRUE} (default) then calculate slope.
#' @param slopeUnits Character, "units" in which to calculate slope: either \code{degrees} for degrees or \code{percent}.
#' @param aspect Logical, if \code{TRUE} then calculate aspect. Aspect is given in degrees from North going clockwise (0 = north, 90 = east, 180 = south, 270 = west).
#' @param profileCurve Logical, if \code{TRUE}, calculate profile curvature. Default is \code{FALSE}.
#' @param tanCurve Logical, if \code{TRUE}, calculate tangential curvature. Default is \code{FALSE}.
#' @param eastWestSlope Logical, if \code{TRUE}, calculate slope in east-west direction. Default is \code{FALSE}.
#' @param northSouthSlope Logical, if \code{TRUE}, calculate slope in north-south direction. Default is \code{FALSE}.
#' @param grassLoc Either \code{NULL} or a 3-element character vector. If the latter, the first element is the base path to the installation of GRASS, the second the version number, and the third the install type for GRASS.  For example, \code{c('C:/OSGeo4W64/', 'grass-7.4.1', 'osgeo4W')}. See \code{\link[link2GI]{linkGRASS7}} for further help. If \code{NULL} (default) the an installation of GRASS is searched for; this may take several minutes.
#' @param grassInit Logical, if \code{TRUE} (default) then a new GRASS session is initialized. If \code{FALSE} then it is assumed a GRASS session has been initialized using the raster in \code{rast}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session.
#' @param grassToR Logical, if \code{TRUE} (default) then the product of the calculations will be returned to R. If \code{FALSE}, then the product is left in the GRASS session and named \code{slope}, \code{aspect}, \code{profileCurve}, \code{tanCurve}, \code{eastWestSlope}, or \code{northSouthSlope}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param ... Arguments to pass to \code{\link[rgrass7]{execGRASS}} when used for rasterizing (i.e., function \code{r.slope.aspect} in GRASS).
#' @return If \code{grassToR} if \code{TRUE}, then a raster or raster stack with the same extent, resolution, and coordinate reference system as \code{rast}. Otherwise, raster(s) with the name(s) \code{slope}, \code{aspect}, \code{profileCurve}, \code{tanCurve}, \code{eastWestSlope}, or \code{northSouthSlope} are written into the GRASS session.
#' @details See (r.slope.aspect)[https://grass.osgeo.org/grass74/manuals/r.slope.aspect.html] for more details.  Note that if you get an error saying "", then you should add the EPSG code to the beginning of the raster and vector coordinate reference system string (its "proj4string"). For example, \code{proj4string(x) <- CRS('+init=epsg:32738')}. EPSG codes for various projections, datums, and locales can be found at (Spatial Reference)[http://spatialreference.org/].
#' @seealso \code{\link[raster]{terrain}}
#' @examples
#' \dontrun{
#' # change this according to where GRASS 7 is installed on your system
#' grassLoc <- c('C:/OSGeo4W64/', 'grass-7.4.1', 'osgeo4W')
#' 
#' data(madElev)
#' 
#' # could also use terrain() which may be faster
#' # in this example
#' topo <- fasterTerrain(madElev, slope = TRUE, aspect=TRUE, grassLoc=grassLoc)
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
	profileCurve = FALSE,
	tanCurve = FALSE,
	eastWestSlope = FALSE,
	northSouthSlope = FALSE,
	grassLoc = NULL,
	grassInit = TRUE,
	grassToR = TRUE,
	...
) {

	flags <- c('quiet', 'overwrite')
	
	# load raster
	if (class(rast) == 'character') rast <- raster::raster(rast)

	# get CRS
	p4s <- sp::proj4string(rast)
	
	# initialize GRASS
	if (grassInit) link2GI::linkGRASS7(rast, default_GRASS7=grassLoc, gisdbase=raster::tmpDir(), location='temp')

	# export raster to GRASS
	exportRastToGrass(rast, vname='rast')
	
	# slope
	if (slope) rgrass7::execGRASS('r.slope.aspect', elevation='rast', slope='slope', format=slopeUnits, flags=flags)
	
	# aspect (0 = east and goes counter-clockwise, so convert so 0 = north going clockwise)
	if (aspect) {
		rgrass7::execGRASS('r.slope.aspect', elevation='rast', aspect='aspectFromEast', flags=flags)
		rgrass7::execGRASS('r.mapcalc', expr='aspect = (450 - aspectFromEast) % 360', flasg=flags)
	}
	
	# curvatures
	if (profileCurve) rgrass7::execGRASS('r.slope.aspect', elevation='rast', pcurvature='profileCurve', flags=flags)
	if (tanCurve) rgrass7::execGRASS('r.slope.aspect', elevation='rast', tcurvature='tanCurve', flags=flags)
	
	# first-derivative slopes
	if (eastWestSlope) rgrass7::execGRASS('r.slope.aspect', elevation='rast', dx='eastWestSlope', flags=flags)
	if (northSouthSlope) rgrass7::execGRASS('r.slope.aspect', elevation='rast', dy='northSouthSlope', flags=flags)

	# return
	if (grassToR) {
	
		out <- rast
	
		if (slope) out <- raster::stack(out, raster::raster(rgrass7::readRAST('slope')))
		if (aspect) out <- raster::stack(out, raster::raster(rgrass7::readRAST('aspect')))
		if (profileCurve) out <- raster::stack(out, raster::raster(rgrass7::readRAST('profileCurve')))
		if (tanCurve) out <- raster::stack(out, raster::raster(rgrass7::readRAST('tanCurve')))
		if (eastWestSlope) out <- raster::stack(out, raster::raster(rgrass7::readRAST('eastWestSlope')))
		if (northSouthSlope) out <- raster::stack(out, raster::raster(rgrass7::readRAST('northSouthSlope')))

		name <- c(
			ifelse(slope, 'slope', NA),
			ifelse(aspect, 'aspect', NA),
			ifelse(profileCurve, 'profileCurve', NA),
			ifelse(tanCurve, 'tanCurve', NA),
			ifelse(eastWestSlope, 'eastWestSlope', NA),
			ifelse(northSouthSlope, 'northSouthSlope', NA)
		)
		
		name <- stats::na.omit(name)
		
		out <- raster::subset(out, 2:raster::nlayers(out))
		sp::proj4string(out) <- p4s
		names(out) <- name
		
		out
		
	}
	
}

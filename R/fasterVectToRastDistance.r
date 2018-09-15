#' Calculate distance between all cells in a raster and nearest spatial point, polyline, or polygon
#'
#' This function is a potentially faster version of the \code{\link[raster]{distance}} function in the \pkg{raster} package which calculates the distance between each cell and the nearest feature in a spatial points, lines, or polygon object. Alternatively, it can calculate the distance from any cell covered by a vector object and the nearest cell \emph{not} covered by a vector object. Note that the \code{distance} function also calculates distances between rasters, but this functionality does is not reproduced in \code{fasterVectToRastDistance} (just distance between a raster and a vector object).
#' @param rast Either a raster or the full path and name of a raster object. This serves as a template for the new raster.
#' @param vect SpatialPoints, SpatialPointsDataFrame, SpatialLines, SpatialLinesDataFrame, SpatialPolygons, or SpatialPolygonsDataFrame or the full path and name of such an object.
#' @param metric Character, indicates type of distance to calculate:
#' \itemize{
#' \item \code{euclidean}: Euclidean distance
#' \item \code{geodesic} (default): geographic distance (suggested to use with \code{meters = TRUE}).
#' \item \code{squared}: Squared Euclidean distance
#' \item \code{maximum}: Maximum Euclidean distance
#' \item \code{manhattan}: Manhattan distance (i.e., "taxicab" distance, distance along cells going only north-south and east-west and never along a diagonal).
#' }
#' meters Logical, if \code{TRUE} then distance is in meters. If \code{FALSE} then distance is in map units.
#' invert Logical, if \code{TRUE} then calculate distance from any cell covered by a vector object to nearest cell **not** covered by a vector onject.
#' @param grassLoc Either \code{NULL} or a 3-element character vector. If the latter, the first element is the base path to the installation of GRASS, the second the version number, and the third the install type for GRASS.  For example, \code{c('C:/OSGeo4W64/', 'grass-7.4.1', 'osgeo4W')}. See \code{\link[link2GI]{linkGRASS7}} for further help. If \code{NULL} (default) the an installation of GRASS is searched for; this may take several minutes.
#' @param initGrass Logical, if \code{TRUE} (default) then a new GRASS session is initialized. If \code{FALSE} then it is assumed a GRASS session has been initialized using the raster in \code{vect}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session.
#' @param backToR Logical, if \code{TRUE} (default) then the product of the calculations will be returned to R. If \code{FALSE}, then the product is left in the GRASS session and named \code{distToVect}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param ... Arguments to pass to \code{\link[rgrass7]{execGRASS}} when used for rasterizing (i.e., function \code{r.grow.distance} in GRASS).
#' @return If \code{backToR} if \code{TRUE}, then a raster with the same extent, resolution, and coordinate reference system as \code{vect}. Otherwise, a raster with the name of \code{distToVect} is written into the GRASS session.
#' @details See (r.latlong)[https://grass.osgeo.org/grass74/manuals/r.grow.distance.html] for more details.  Note that if you get an error saying "", then you should add the EPSG code to the beginning of the raster and vector coordinate reference system string (its "proj4string"). For example, \code{proj4string(x) <- CRS('+init=epsg:32738')}. EPSG codes for various projections, datums, and locales can be found at (Spatial Reference)[http://spatialreference.org/].
#' @seealso \code{\link[raster]{distance}}
#' @examples
#' \dontrun{
#' library(rgeos)
#' data(madForest)
#' # GRASS location -- change if needed, depending on version number
#' # and location!
#' grassLoc <- c('C:/OSGeo4W64/', 'grass-7.4.1', 'osgeo4W')
#' 
#' ### project raster
#' # could also use projectRaster() which
#' # may be faster in this example
#' elev <- fasterProjectRaster(elev, forest2000, grassLoc=grassLoc)
#' # elev <- projectRaster(elev, forest2000)
#' plot(elev, main='Elevation')
#' plot(mad0, add=TRUE)
#'
#' ### create mask for raster calculations
#' # could also use rasterize() or mask() which may
#' be faster in this example
#' madMask <- fasterRasterize(mad0, elev, grassLoc=grassLoc)
#' elev <- madMask * elev
#' # alternative #1
#' # madMask <- rasterize(mad0, elev)
#' # madMask <- 1 + 0 * madMask
#' # elev <- madMask * elev
#' #
#' # alternative #2
#' # elev <- mask(elev, mad0)
#' plot(elev, main='Elevation (m)')
#' plot(mad0, add=TRUE)
#'
#' ### topography
#' # could also use terrain() which may be faster
#' in this example
#' topo <- fasterTerrain(elev, slope = TRUE, aspect=TRUE, grassLoc=grassLoc)
#' # slp <- terrain(elev, opt='slope', unit='degrees')
#' # asp <- terrain(elev, opt='aspect', unit='degrees')
#' # topo <- stack(slp, asp)
#' # names(topo) <- c('slope', 'aspect')
#' plot(topo)
#'
#' ### distance to coast
#' # could also use distance() function which may be
#' # faster in this example
#' distToCoast <- fasterRastDistance(elev, fillNAs=FALSE, grassLoc=grassLoc)
#' # ocean <- calc(elev, function(x) ifelse(is.na(x), 1, NA))
#' # distToCoast <- raster::distance(ocean)
#' # distToCoast <- madMask * distToCoast
#' plot(distToCoast, main='Distance to Coast (m)')
#'
#' ### distance to nearest river (in the study region)
#' # could also use distance() function
#' # which may be faster in this example
#' distToRiver <- fasterRastToVectDistance(
#' 	elev, madRivers, grassLoc=grassLoc)
#' # naRast <- NA * elev
#' # distToRiver <- distance(naRast, madRivers)
#' # distToRiver <- madMask * distToRiver
#' plot(distToRiver, main='Distance to River (m)'
#' plot(madRivers, col='blue', add=TRUE)
#'
#' ### convert rivers (lines) to raster
#' # could use rasterize() which may be faster in this example
#' riverRast <- fasterRasterize(madRivers, elev)
#' # riverRast <- rasterize(madRivers, elev)
#' # riverRast <- riverRast > 0
#' par(mfrow=c(1, 2))
#' plot(mad0, main='Rivers as Vector')
#' plot(madRivers, col='blue', lwd=2, add=TRUE)
#' plot(riverRast, main='Rivers as Raster', col='blue')
#' plot(mad0, add=TRUE)
#' 
#' ### forest fragmentation
#' # forest = 1, all other is NA so convert NA to 0
#' forest2000 <- raster::calc(forest2000, function(x) ifelse(is.na(x), 0, x))
#' forest2014 <- raster::calc(forest2014, function(x) ifelse(is.na(x), 0, x))
#'
#' # make mask to force ocean to NA
#' # could use fasterRasterize() or rasterize()
#' # rasterize is faster in this example because rasters are small
#' maskRast <- fasterRasterize(mad, forest2000, grassLoc=grassLoc)
#' # maskRast <- raster::rasterize(mad, forest2000)
#' # maskRast <- 1 + 0 * maskRast
#' forest2000 <- maskRast * forest2000
#' forest2014 <- maskRast * forest2014
#' names(forest2000) <- 'forest2000'
#' names(forest2014) <- 'forest2014'
#'
#' fragRasts <- frag(forest2000)
#' change <- sum(forest2000, forest2014)
#' par(mfrow=c(2, 2))
#' plot(change, col=c('gray90', 'red', 'green'), main='Forest Cover')
#' legend('topright', legend=c('Forest', 'Loss'), fill=c('green', 'red'))
#' plot(fragRasts[['density']], main='Density in 2000')
#' plot(fragRasts[['connect']], main='Connectivity in 2000')
#' cols <- c('gray90', 'forestgreen', 'lightgreen', 'orange', 'yellow', 'red')
#' plot(fragRasts[['class']], main='Fragmentation Class', col=cols)
#' legend('topright', fill=cols,
#' 	legend=c('no forest', 'interior', 'patch',
#'		'transitional', 'perforated', 'edge'))
#'
#' ### raster to polygons
#' # convert fragmentation class to polygons
#' # could also use rasterToPolygons() which is
#' probably faster in this example
#' fragPoly <- fasterVectorize(fragRasts[['class']],
#' 	vectType='area', grassLoc=grassLoc)
#' # fragPoly <- rasterToPolygons(fragRasts[['class']], dissolve=TRUE)
#' plot(fragPoly, main='Fragmentation Class Polygon')
#' legend('topright', fill=cols,
#' 	legend=c('no forest', 'interior', 'patch',
#'		'transitional', 'perforated', 'edge'))
#'
#' }
#' @export

fasterVectToRastDistance <- function(
	rast,
	vect,
	metric = 'euclidean',
	meters = TRUE,
	invert = FALSE,
	grassLoc = NULL,
	initGrass = TRUE,
	backToR = TRUE,
	...
) {

	flags <- flags_vToRast <- flags_rGrowDistance <- c('quiet', 'overwrite')
	if (meters) flags_rGrowDistance <- c(flags_rGrowDistance, 'm')
	if (invert) flags_rGrowDistance <- c(flags_rGrowDistance)
	
	# load spatial object and raster
	if (class(rast) == 'character') rast <- raster::raster(rast)
	if (class(vect) == 'character') vect <- raster::shapefile(vect)

	# get CRS
	p4s <- sp::proj4string(rast)
	
	# rasterize vector: creates raster named "distToVect"
	fasterRasterize(vect=vect, rast=rast, use='value', value=1, grassLoc=grassLoc, initGrass=TRUE, backToR=FALSE)
	
	# calculate distance
	rgrass7::execGRASS('r.grow.distance', input='vectAsRast', distance='distToVect', metric=metric, flags=flags_rGrowDistance, ...)

	# return
	if (backToR) {
	
		out <- rgrass7::readRAST('distToVect')
		out <- raster::raster(out)
		sp::proj4string(out) <- p4s
		names(out) <- 'distToVect'
		out
		
	}

}

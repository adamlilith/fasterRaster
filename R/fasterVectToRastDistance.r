#' Calculate distance between all cells in a raster and nearest spatial point, polyline, or polygon
#'
#' This function is a potentially faster version of the \code{\link[raster]{distance}} function in the \pkg{raster} package which calculates the distance between each cell and the nearest feature in a spatial points, lines, or polygon object. Alternatively, it can calculate the distance from any cell covered by a vector object and the nearest cell \emph{not} covered by a vector object. Note that the \code{distance} function also calculates distances between rasters, but this functionality is not reproduced in \code{fasterVectToRastDistance} (just distance between a raster and a vector object).
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
#'
#' meters Logical, if \code{TRUE} then distance is in meters. If \code{FALSE} then distance is in map units.
#' invert Logical, if \code{TRUE} then calculate distance from any cell covered by a vector object to nearest cell **not** covered by a vector onject.
#' @param grassLoc Either \code{NULL} or a 3-element character vector. If the latter, the first element is the base path to the installation of GRASS, the second the version number, and the third the install type for GRASS.  For example, \code{c('C:/OSGeo4W64/', 'grass-7.4.1', 'osgeo4W')}. See \code{\link[link2GI]{linkGRASS7}} for further help. If \code{NULL} (default) the an installation of GRASS is searched for; this may take several minutes.
#' @param grassInit Logical, if \code{TRUE} (default) then a new GRASS session is initialized. If \code{FALSE} then it is assumed a GRASS session has been initialized using the raster in \code{vect}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session.
#' @param grassToR Logical, if \code{TRUE} (default) then the product of the calculations will be returned to R. If \code{FALSE}, then the product is left in the GRASS session and named \code{distToVect}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param ... Arguments to pass to \code{\link[rgrass7]{execGRASS}} when used for rasterizing (i.e., function \code{r.grow.distance} in GRASS).
#' @return If \code{grassToR} if \code{TRUE}, then a raster with the same extent, resolution, and coordinate reference system as \code{vect}. Otherwise, a raster with the name of \code{distToVect} is written into the GRASS session.
#' @details See (r.latlong)[https://grass.osgeo.org/grass74/manuals/r.grow.distance.html] for more details.  Note that if you get an error saying "", then you should add the EPSG code to the beginning of the raster and vector coordinate reference system string (its "proj4string"). For example, \code{proj4string(x) <- CRS('+init=epsg:32738')}. EPSG codes for various projections, datums, and locales can be found at (Spatial Reference)[http://spatialreference.org/].
#' @seealso \code{\link[raster]{distance}}
#' @examples
#' \donttest{
#' # change this according to where GRASS 7 is installed on your system
#' grassLoc <- c('C:/OSGeo4W64/', 'grass-7.4.1', 'osgeo4W')
#' 
#' data(madForest2000)
#' data(madRivers)
#' 
#' # could also use distance() which is
#' # probably faster in this example
#' distToRiver <- fasterVectToRastDistance(madForest2000, madRivers,
#' 	grassLoc=grassLoc)
#' # distToRiver <- distance(madForest2000, madRivers)
#' plot(distToRiver, main='Distance to Rivers (m)')
#' plot(madRivers, col='blue', add=TRUE)
#' }
#' @export

fasterVectToRastDistance <- function(
	rast,
	vect,
	metric = 'euclidean',
	meters = TRUE,
	invert = FALSE,
	grassLoc = NULL,
	grassInit = TRUE,
	grassToR = TRUE,
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
	fasterRasterize(vect=vect, rast=rast, use='value', value=1, grassLoc=grassLoc, grassInit=TRUE, grassToR=FALSE)
	
	# calculate distance
	rgrass7::execGRASS('r.grow.distance', input='vectAsRast', distance='distToVect', metric=metric, flags=flags_rGrowDistance, ...)

	# return
	if (grassToR) {
	
		out <- rgrass7::readRAST('distToVect')
		out <- raster::raster(out)
		sp::proj4string(out) <- p4s
		names(out) <- 'distToVect'
		out
		
	}

}

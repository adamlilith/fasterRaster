#' Distance distance between cells with \code{NA}s and closest non-\code{NA} cells
#'
#' This function is a potentially faster version of the \code{\link[raster]{distance}} function in the \pkg{raster} package which it replaces values in all \code{NA} cells with the distance between them and the closest non-\code{NA} cell. Alternatively, it fills in values of non-\code{NA} cells with the distance between them and the closest \code{NA} cell. Note that the \code{distance} function also calculates distances between a raster and a spatial vector object, but this functionality is reproduced in \code{\link[fasterRaster]{fasterVectToRastDistance}}.
#' @param rast Either a raster or the full path and name of a raster object.
#' @param metric Character, indicates type of distance to calculate:
#' \itemize{
#' \item \code{euclidean} (default): Euclidean distance
#' \item \code{geodesic}: geographic distance (suggested to use with \code{meters = TRUE}).
#' \item \code{squared}: Squared Euclidean distance
#' \item \code{maximum}: Maximum Euclidean distance
#' \item \code{manhattan}: Manhattan distance (i.e., "taxicab" distance, distance along cells going only north-south and east-west and never along a diagonal).
#' }
#' meters Logical, if \code{TRUE} then distance is in meters. If \code{FALSE} then distance is in map units.
#' fillNAs Logical, if \code{TRUE} (default) then fill code{NA} cells with distance between them and closest non-\code{NA}. If \code{TRUE} then replace value in non-{NA} cells with distance between them and nearest \code{NA} cell.
#' @param grassLoc Either \code{NULL} or a 3-element character vector. If the latter, the first element is the base path to the installation of GRASS, the second the version number, and the third the install type for GRASS.  For example, \code{c('C:/OSGeo4W64/', 'grass-7.4.1', 'osgeo4W')}. See \code{\link[link2GI]{linkGRASS7}} for further help. If \code{NULL} (default) the an installation of GRASS is searched for; this may take several minutes.
#' @param grassInit Logical, if \code{TRUE} (default) then a new GRASS session is initialized. If \code{FALSE} then it is assumed a GRASS session has been initialized using the raster in \code{vect}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session.
#' @param grassToR Logical, if \code{TRUE} (default) then the product of the calculations will be returned to R. If \code{FALSE}, then the product is left in the GRASS session and named \code{distance}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param ... Arguments to pass to \code{\link[rgrass7]{execGRASS}} when used for rasterizing (i.e., function \code{r.grow.distance} in GRASS).
#' @return If \code{grassToR} if \code{TRUE}, then a raster with the same extent, resolution, and coordinate reference system as \code{vect}. Otherwise, a raster with the name of \code{distance} is written into the GRASS session.
#' @details See (r.latlong)[https://grass.osgeo.org/grass74/manuals/r.grow.distance.html] for more details.  Note that if you get an error saying "", then you should add the EPSG code to the beginning of the raster and vector coordinate reference system string (its "proj4string"). For example, \code{proj4string(x) <- CRS('+init=epsg:32738')}. EPSG codes for various projections, datums, and locales can be found at (Spatial Reference)[http://spatialreference.org/].
#' @seealso \code{\link[raster]{distance}}, \code{\link[fasterRaster]{fasterVectToRastDistance}}
#' @examples
#' \donttest{
#' # change this according to where GRASS 7 is installed on your system
#' grassLoc <- c('C:/OSGeo4W64/', 'grass-7.4.1', 'osgeo4W')
#' 
#' data(madForest2000)
#' 
#' # could also use distance() function which may be
#' # faster in this example
#' distToForest <- fasterRastDistance(madForest2000, fillNAs=TRUE, grassLoc=grassLoc)
#' # distToForest <- distance(madForest2000)
#' plot(distToForest, main='Distance to Forest (m)')
#' }
#' @export

fasterRastDistance <- function(
	rast,
	metric = 'euclidean',
	meters = TRUE,
	fillNAs = TRUE,
	grassLoc = NULL,
	grassInit = TRUE,
	grassToR = TRUE,
	...
) {

	flags <- c('quiet', 'overwrite')
	if (meters) flags <- c(flags, 'm')
	if (!fillNAs) flags <- c(flags, 'n')
	
	# load raster
	if (class(rast) == 'character') rast <- raster::raster(rast)

	# get CRS
	p4s <- sp::proj4string(rast)
	
	# initialize GRASS
	if (grassInit) link2GI::linkGRASS7(rast, default_GRASS7=grassLoc, gisdbase=raster::tmpDir(), location='temp')

	# export raster to GRASS
	exportRastToGrass(rast, vname='rast')
	
	# calculate distance
	rgrass7::execGRASS('r.grow.distance', input='rast', distance='distance', metric=metric, flags=flags)

	# return
	if (grassToR) {
	
		out <- rgrass7::readRAST('distance')
		out <- raster::raster(out)
		sp::proj4string(out) <- p4s
		names(out) <- 'distance'
		out
		
	}
	
}

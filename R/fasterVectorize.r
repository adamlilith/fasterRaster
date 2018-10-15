#' Convert a raster to a vector (points, lines, or polygons)
#'
#' This function is a potentially faster version of the function \code{\link[raster]{rasterToPolygons}} in the \pkg{raster} package. It can convert a raster to points or polygons (conversion to lines is not yet supported, although it is possible using the \code{r.to.vect} function in GRASS).
#' @param rast Either a raster or the full path and name of a raster object. The raster values must be 1 or 0 (or \code{NA}). The fragmentation index applies to the state of the entity represented by the 1's.
#' @param vectType Character, Indicates type of output: \code{point}, \code{line} (not supported yet), or \code{area}.
#' @param agg Logical, if \code{TRUE} (default) then union all points/lines/polygons with the same value into the same "multipart" polygon. This may or may not be desirable. For example, if the raster is vectorized into a polygons object each cell will become a separate polygon. Using this option will merge cells with the same value (even if they are not spatially adjacent one another).
#' @param smooth Logical, if \code{TRUE} then "round" cell corners by connecting the midpoints of corner cells (which leaves out the corner-most triangle of that cell). This option only applies if \code{vectType} is \code{area}. Default is \code{FALSE}.
#' @param calcDensity Logical, if \code{TRUE} then calculate density in the moving window. This will create a raster named \code{density} in the GRASS environment if \code{grassToR} is \code{FALSE} or return a raster named \code{density} if \code{grassToR} is \code{TRUE}. Default is \code{FALSE}.
#' @param calcConnect Logical, if \code{TRUE} then calculate a connectivity raster (conditional probability a cell with a value of 1 has a value that is also 1) in the moving window. This will create a raster named \code{connect} in the GRASS environment if \code{grassToR} is \code{FALSE} or return a raster named \code{connect} if \code{grassToR} is \code{TRUE}. Default is \code{FALSE}.
#' @param grassLoc Either \code{NULL} or a 3-element character vector. If the latter, the first element is the base path to the installation of GRASS, the second the version number, and the third the install type for GRASS.  For example, \code{c('C:/OSGeo4W64/', 'grass-7.4.1', 'osgeo4W')}. See \code{\link[link2GI]{linkGRASS7}} for further help. If \code{NULL} (default) the an installation of GRASS is searched for; this may take several minutes.
#' @param grassInit Logical, if \code{TRUE} (default) then a new GRASS session is initialized. If \code{FALSE} then it is assumed a GRASS session has been initialized using the raster in \code{rast}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session.
#' @param grassToR Logical, if \code{TRUE} (default) then the product of the calculations will be returned to R. If \code{FALSE}, then the product is left in the GRASS session and named \code{rastToVect}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param ... Arguments to pass to \code{\link[rgrass7]{execGRASS}} when used for converting a raster to a vector (i.e., function \code{r.to.vect} in GRASS).
#' @return If \code{grassToR} if \code{TRUE}, then a SpatialPointsDataFrame, SpatialLinesDataFrame, or a SpatialPolygonsDataFrame with the same coordinate reference system as \code{rast}. The field named \code{value} will have the raster values. Otherwise, vector object named \code{vectToRast} a  will be written into the GRASS session.
#' @details See (r.to.vect)[https://grass.osgeo.org/grass74/manuals/r.to.vect.html] for more details.  Note that if you get an error saying "", then you should add the EPSG code to the beginning of the raster and vector coordinate reference system string (their "proj4string"). For example, \code{proj4string(x) <- CRS('+init=epsg:32738')}. EPSG codes for various projections, datums, and locales can be found at (Spatial Reference)[http://spatialreference.org/].
#' @seealso \code{\link[raster]{rasterToPolygons}}, \code{\link[fasterRaster]{fasterRasterize}} 
#' @examples
#' \dontrun{
#' # change this according to where GRASS 7 is installed on your system
#' grassLoc <- c('C:/OSGeo4W64/', 'grass-7.4.1', 'osgeo4W')
#' 
#' data(madForest2000)
#' 
#' # could also use rasterToPolygons() which is
#' # probably faster in this example
#' forestPoly <- fasterVectorize(madForest2000,
#' 	vectType='area', grassLoc=grassLoc)
#' # forestPoly <- rasterToPolygons(madForest2000, dissolve=TRUE)
#' plot(forestPoly, main='Forest as Polygon')
#' }
#' @export

fasterVectorize <- function(
	rast,
	vectType,
	agg = TRUE,
	smooth = FALSE,
	grassLoc = NULL,
	grassInit = TRUE,
	grassToR = TRUE,
	...
) {

	if (!(vectType %in% c('point', 'line', 'area'))) stop('Argument "vectType" in function fasterVectorize() must be either "point", "line", or "area".')

	flags <- c('quiet', 'overwrite')
	if (smooth & vectType == 'area') flags <- c(flags, 's')
	
	# load spatial object and raster
	if (class(rast) == 'character') rast <- raster::raster(rast)

	# get CRS
	p4s <- sp::proj4string(rast)
	
	# initialize GRASS
	if (grassInit) link2GI::linkGRASS7(rast, default_GRASS7=grassLoc, gisdbase=raster::tmpDir(), location='temp')
	
	exportRastToGrass(rast, vname='rast')

	# vectorize
	rgrass7::execGRASS('r.to.vect', input='rast', output='rastToVect', type=vectType, flags=flags, ...)
	
	# get raster back to R
	if (grassToR) {
	
		out <- rgrass7::readVECT('rastToVect')
		
		# join output with same values
		if (agg) {
			out <- raster::aggregate(out, by='value')
		}
		
		sp::proj4string(out) <- p4s
		out
		
	}

}

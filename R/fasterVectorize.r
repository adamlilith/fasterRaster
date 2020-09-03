#' Convert a raster to a vector (points, lines, or polygons)
#'
#' This function is a potentially faster version of the function \code{\link[raster]{rasterToPolygons}} in the \pkg{raster} package. It can convert a raster to points or polygons (conversion to lines is not yet supported, although it is possible using the \code{r.to.vect} module in GRASS).
#' @param rast Either a raster or the name of a raster in an existing GRASS session.
#' @param vectType Character. Indicates type of output: \code{point}, \code{line} (not supported yet), or \code{area}.
#' @param agg Logical. If \code{TRUE} (default) then union all points/lines/polygons with the same value into the same "multipart" polygon. This may or may not be desirable. For example, if the raster is vectorized into a polygons object each cell will become a separate polygon. Using this option will merge cells with the same value (even if they are not spatially adjacent one another).
#' @param smooth Logical. If \code{TRUE} then "round" cell corners by connecting the midpoints of corner cells (which leaves out the corner-most triangle of that cell). This option only applies if \code{vectType} is \code{area}. Default is \code{FALSE}.
#' @param calcDensity Logical, if \code{TRUE} then calculate density in the moving window. This will create a raster named \code{density} in the GRASS environment if \code{grassToR} is \code{FALSE} or return a raster named \code{density} if \code{grassToR} is \code{TRUE}. Default is \code{FALSE}.
#' @param calcConnect Logical. If \code{TRUE} then calculate a connectivity raster (conditional probability a cell with a value of 1 has a value that is also 1) in the moving window. This will create a raster named \code{connect} in the GRASS environment if \code{grassToR} is \code{FALSE} or return a raster named \code{connect} if \code{grassToR} is \code{TRUE}. Default is \code{FALSE}.
#' @param grassDir Character or \code{NULL} (default). Name of the directory in which GRASS is installed. Example: \code{'C:/Program Files/GRASS GIS 7.8'}. If this is \code{NULL}, R will search for the directory in which GRASS is installed. This usually fails, or if it succeeds, takes several minutes.
#' @param alreadyInGrass Logical. If \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{rast}. If \code{FALSE}, use a raster already in GRASS with the name given by \code{rast}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#' @param grassToR Logical. If \code{TRUE} (default) then the product of the calculations will be returned to R. If \code{FALSE}, then the product is left in the GRASS session and named \code{rastToVect}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param ... Arguments to pass to \code{\link[rgrass7]{execGRASS}} when used for converting a raster to a vector (i.e., function \code{r.to.vect} in GRASS).
#' @return If \code{grassToR} if \code{TRUE}, then a SpatialPointsDataFrame, SpatialLinesDataFrame, or a SpatialPolygonsDataFrame with the same coordinate reference system as \code{rast}. The field named \code{value} will have the raster values. Otherwise, vector object named \code{rastToVect} a  will be written into the GRASS session.
#' @details See \href{r.to.vect}{https://grass.osgeo.org/grass78/manuals/r.to.vect.html} for more details.  Note that if you get an error saying "", then you should add the EPSG code to the beginning of the raster and vector coordinate reference system string (their "proj4string"). For example, \code{proj4string(x) <- CRS('+init=epsg:32738')}. EPSG codes for various projections, datums, and locales can be found at \href{Spatial Reference}{http://spatialreference.org}.
#' @seealso \code{\link[raster]{rasterToPolygons}}, \code{\link[fasterRaster]{fasterRasterize}} 
#' @examples
#' \donttest{
#' # change this according to where GRASS 7 is installed on your system
#' grassDir <- 'C:/Program Files/GRASS GIS 7.8'
#' 
#' data(madForest2000)
#' 
#' # could also use rasterToPolygons() which is
#' # probably faster in this example
#' forestPoly <- fasterVectorize(rast=madForest2000,
#' 	vectType='area', grassDir=grassDir)
#' # forestPoly <- rasterToPolygons(madForest2000, dissolve=TRUE)
#' par(mfrow=c(1, 2))
#' plot(madForest2000, main='Forest as Raster', col='forestgreen')
#' plot(forestPoly, main='Forest as Polygon', col='forestgreen', border=NA)
#' }
#' @export

fasterVectorize <- function(
	rast,
	vectType,
	agg = TRUE,
	smooth = FALSE,
	grassDir = NULL,
	alreadyInGrass = FALSE,
	grassToR = TRUE,
	...
) {

	if (!(vectType %in% c('point', 'line', 'area'))) stop('Argument "vectType" in function fasterVectorize() must be either "point", "line", or "area".')

	flags <- c('quiet', 'overwrite')
	if (smooth & vectType == 'area') flags <- c(flags, 's')
	
	# initialize GRASS
	input <- initGrass(alreadyInGrass, rast=rast, vect=NULL, grassDir=grassDir)

	# vectorize
	rgrass7::execGRASS('r.to.vect', input=input, output='rastToVect', type=vectType, flags=flags, ...)
	
	# get raster back to R
	if (grassToR) {
	
		out <- rgrass7::readVECT('rastToVect')
		
		# join output with same values
		if (agg) {
			out <- raster::aggregate(out, by='value')
		}
		
		out
		
	}

}

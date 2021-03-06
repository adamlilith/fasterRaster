#' Distance between raster cells and nearest spatial point, polyline, or polygon
#'
#' This function is a potentially faster version of the \code{\link[raster]{distance}} function in the \pkg{raster} package which calculates the distance between each cell and the nearest feature in a spatial points, lines, or polygon object. Alternatively, it can calculate the distance from any cell covered by a vector object and the nearest cell \emph{not} covered by a vector object. Note that the \code{distance} function also calculates distances between rasters, but this functionality is not reproduced in \code{fasterVectToRastDistance} (just distance between a raster and a vector object).
#' @param rast Either a raster or the name of a raster in an existing GRASS session. This serves as a template for the new raster.
#' @param vect Either a SpatialPoints, SpatialPointsDataFrame, SpatialLines, SpatialLinesDataFrame, SpatialPolygons, or SpatialPolygonsDataFrame or the name of such a vector data set in an existing GRASS session.
#' @param metric Character, indicates type of distance to calculate:
#' \itemize{
#' \item \code{euclidean}: Euclidean distance
#' \item \code{geodesic} (default): geographic distance (suggested to use with \code{meters = TRUE}).
#' \item \code{squared}: Squared Euclidean distance
#' \item \code{maximum}: Maximum Euclidean distance
#' \item \code{manhattan}: Manhattan distance (i.e., "taxicab" distance, distance along cells going only north-south and east-west and never along a diagonal).
#' } 
#' @param meters Logical, if \code{TRUE} then distance is in meters. If \code{FALSE} then distance is in map units.
#' @param invert Logical, if \code{TRUE} then for cells covered by a vector object calculate distance to nearest cell \emph{not} covered by a vector object.
#' @param grassDir Character or \code{NULL} (default). Name of the directory in which GRASS is installed. Example: \code{'C:/Program Files/GRASS GIS 7.8'}. If this is \code{NULL}, R will search for the directory in which GRASS is installed. This usually fails, or if it succeeds, takes several minutes.
#' @param alreadyInGrass Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{rast}. If \code{FALSE}, use a raster already in GRASS with the name given by \code{rast}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#' @param grassToR Logical, if \code{TRUE} (default) then the product of the calculations will be returned to R. If \code{FALSE}, then the product is left in the GRASS session and named \code{distToVect}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param outGrassName Character. Name of output in GRASS. This is useful if you want to refer to the output object in GRASS later in a session.
#' @param ... Arguments to pass to \code{\link[rgrass7]{execGRASS}} when used for rasterizing (i.e., function \code{r.grow.distance} in GRASS).
#' @return If \code{grassToR} if \code{TRUE}, then a raster with the same extent, resolution, and coordinate reference system as \code{vect}. Regardless, a raster with the name of \code{distToVect} is written into the GRASS session.
#' @details See help for \code{r.grow.distance} at \url{https://grass.osgeo.org/grass78/manuals/r.grow.distance.html} for more details.
#' @seealso \code{\link[raster]{distance}}
#' @examples
#' \donttest{
#' # change this according to where GRASS 7 is installed on your system
#' grassDir <- 'C:/Program Files/GRASS GIS 7.8' # example for a PC
#' grassDir <- "/Applications/GRASS-7.8.app/Contents/Resources" # for a Mac
#' 
#' data(madForest2000)
#' data(madRivers)
#' 
#' distToRiver <- fasterVectToRastDistance(rast=madForest2000,
#' vect=madRivers, grassDir=grassDir)
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
	grassDir = NULL,
	alreadyInGrass = FALSE,
	grassToR = TRUE,
	outGrassName = 'distToVect',
	...
) {

	flags <- flags_vToRast <- flags_rGrowDistance <- c('quiet', 'overwrite')
	if (meters) flags_rGrowDistance <- c(flags_rGrowDistance, 'm')
	if (invert) flags_rGrowDistance <- c(flags_rGrowDistance)
	
	# rasterize vector: creates raster named "distToVect"
	fasterRasterize(vect=vect, rast=rast, use='value', value=1, grassDir=grassDir, alreadyInGrass=alreadyInGrass, grassToR=FALSE, outGrassName='vectToRast', ...)
	
	# calculate distance
	rgrass7::execGRASS('r.grow.distance', input='vectToRast', distance=outGrassName, metric=metric, flags=flags_rGrowDistance, ...)

	# return
	if (grassToR) {
	
		out <- rgrass7::readRAST(outGrassName)
		out <- raster::raster(out)
		names(out) <- outGrassName
		out
		
	}

}

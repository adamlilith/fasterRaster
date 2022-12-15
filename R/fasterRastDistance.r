#' Distance between cells with NAs and closest non-NA cells on a raster
#'
#' This function is a potentially faster version of the \code{\link[terra]{distance}} function in the \pkg{terra} package which it replaces values in all \code{NA} cells with the distance between them and the closest non-\code{NA} cell. Alternatively, it fills in values of non-\code{NA} cells with the distance between them and the closest \code{NA} cell. Note that the \code{distance} function also calculates distances between a raster and a spatial vector object, but this functionality is reproduced in \code{\link[fasterRaster]{fasterVectToRastDistance}}.
#' @param rast Either a raster or the name of a raster in an existing GRASS session.
#' @param metric Character, indicates type of distance to calculate:
#' \itemize{
#' \item \code{euclidean} (default): Euclidean distance
#' \item \code{geodesic}: geographic distance (suggested to use with \code{meters = TRUE}).
#' \item \code{squared}: Squared Euclidean distance
#' \item \code{maximum}: Maximum Euclidean distance
#' \item \code{manhattan}: Manhattan distance (i.e., "taxicab" distance, distance along cells going only north-south and east-west and never along a diagonal).
#' }
#' @param meters Logical, if \code{TRUE} then distance is in meters. If \code{FALSE} then distance is in map units.
#' @param fillNAs Logical, if \code{TRUE} (default) then fill code{NA} cells with distance between them and closest non-\code{NA}. If \code{TRUE} then replace value in non-{NA} cells with distance between them and nearest \code{NA} cell.
#' @param grassDir Character or \code{NULL} (default). Name of the directory in which GRASS is installed. Example for a Windows system: \code{'C:/Program Files/GRASS GIS 8.2'}. If this is \code{NULL}, R will search for the directory in which GRASS is installed. This usually fails, or if it succeeds, takes several minutes.
#' @param alreadyInGrass Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{rast}. If \code{FALSE}, use a raster already in GRASS with the name given by \code{rast}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#' @param grassToR Logical, if \code{TRUE} (default) then the output will be returned to R. If \code{FALSE}, then the output is left in the GRASS session and named the value in \code{outGrassName} \code{distance}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param outGrassName Character. Name of output in GRASS. This is useful if you want to refer to the output object in GRASS later in a session.
#' @param ... Arguments to pass to \code{\link[rgrass]{execGRASS}} when used for rasterizing (i.e., function \code{r.grow.distance} in GRASS).
#' @return If \code{grassToR} if \code{TRUE}, then a raster with the same extent, resolution, and coordinate reference system as \code{vect}. Regardless, a raster with the name given by \code{outGrassName} is written into the GRASS session.
#' @details See the documentation for the GRASS module \code{r.grow.distance}{https://grass.osgeo.org/grass82/manuals/r.grow.distance.html}.
#' @seealso \code{\link[terra]{distance}}, \code{\link[fasterRaster]{fasterVectToRastDistance}}
#' @examples
#' \donttest{
#' # change this according to where GRASS 7 is installed on your system
#' grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
#' grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac
#' 
#' data(madForest2000)
#' 
#' # could also use distance() function from the raster package which is
#' # slower in this example
#' distToForest <- fasterRastDistance(rast=madForest2000,
#' fillNAs=TRUE, grassDir=grassDir)
#' # distToForest <- distance(madForest2000)
#' par(mfrow=c(1, 2))
#' plot(madForest2000, 'Forest', col='forestgreen')
#' plot(distToForest, main='Distance to Forest (m)')
#' }
#' @export

fasterRastDistance <- function(
	rast,
	metric = 'euclidean',
	meters = TRUE,
	fillNAs = TRUE,
	grassDir = options('grassDir'),
	alreadyInGrass = FALSE,
	grassToR = TRUE,
	outGrassName = 'distance',
	...
) {

	flags <- c('quiet', 'overwrite')
	if (meters) flags <- c(flags, 'm')
	if (!fillNAs) flags <- c(flags, 'n')
	
	# initialize GRASS
	input <- initGrass(alreadyInGrass, rast=rast, vect=NULL, grassDir=grassDir)
	
	# calculate distance
	rgrass::execGRASS('r.grow.distance', input='rast', distance=outGrassName, metric=metric, flags=flags)

	# return
	if (grassToR) {
	
		out <- rgrass::read_RAST(outGrassName)
		out <- raster::raster(out)
		names(out) <- outGrassName
		out
		
	}
	
}

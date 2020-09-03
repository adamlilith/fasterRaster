#' Project and resample a raster
#'
#' This function is a potentially faster version of the \code{\link[raster]{projectRaster}} function in the \pkg{raster} package for projecting (and resampling) a raster. It requires the user has GRASS GIS Version 7 installed.
#' @param rast Either a raster or the name of a raster in an existing GRASS session. This raster will be projected.
#' @param template Either a raster or the name of a raster in an existing GRASS session to serve as a template for projecting.
#' @param method Character, method for resampling cells:
#' \itemize{
#' 		\item \code{nearest}: Nearest neighbor (uses value from 1 cell).
#' 		\item \code{bilinear}: Bilinear interpolation (default; uses values from 4 cells).
#' 		\item \code{bilinear_f}: Bilinear interpolation with fallback.
#' 		\item \code{bicubic}: Bicubic interpolation (uses values from 16 cells).
#' 		\item \code{bicubic_f}: Bicubic interpolation with fallback.
#' 		\item \code{lanczos}: Lanczos interpolation (uses values from 25 cells).
#' 		\item \code{lanczos_f}: Lanczos interpolation with fallback.
#' }
#' @param grassDir Character or \code{NULL} (default). Name of the directory in which GRASS is installed. Example: \code{'C:/Program Files/GRASS GIS 7.8'}. If this is \code{NULL}, R will search for the directory in which GRASS is installed. This usually fails, or if it succeeds, takes several minutes.
#' @param alreadyInGrass Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{template} to set the extent, projection, and resolution. If \code{FALSE}, use a raster already in GRASS with the name given by \code{template}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#' @param grassToR Logical, if \code{TRUE} (default) then the product of the calculations will be returned to R. If \code{FALSE}, then the product is left in the GRASS session and named \code{rast}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param ... Arguments to pass to \code{\link[rgrass7]{execGRASS}} when used for rasterizing (i.e., function \code{r.proj} in GRASS).
#' @return If \code{grassToR} if \code{TRUE}, then a raster or raster stack with the same extent, resolution, and coordinate reference system as \code{rast}. Otherwise, a raster with the name \code{resampled} is written into the GRASS session.
#' @details Note that it is not uncommon to get the warning "Projection of dataset does not appear to match the current mapset" (followed by more information). If the coordinate reference systems match, then the cause is likely due to extra information being stored in one of the spatial object's coordinate reference system slot (e.g., an EPSG code in addition to the other proj4string information), in which case it can probably be safely ignored.  
#' See \href{r.proj}{https://grass.osgeo.org/grass78/manuals/r.proj.html} for more details.  Note that if you get an error saying "", then you should add the EPSG code to the beginning of the raster and vector coordinate reference system string (its "proj4string"). For example, \code{proj4string(x) <- CRS('+init=epsg:32738')}. EPSG codes for various projections, datums, and locales can be found at \href{Spatial Reference}{http://spatialreference.org}.  
#' @seealso \code{\link[raster]{projectRaster}}
#' @examples
#' \donttest{
#' # could also use projectRaster() which
#' # may be faster in this example
#' # change this according to where GRASS 7 is installed on your system
#' grassDir <- 'C:/Program Files/GRASS GIS 7.8'
#'
#' data(madElev)
#' data(madForest2000)
#' projection(madElev)
#' projection(madForest2000)
#' 
#' elevResamp <- fasterProjectRaster(rast=madElev,
#' template=madForest2000, grassDir=grassDir)
#' # elevResamp <- projectRaster(elev, madForest2000)
#' par(mfrow=c(1, 2))
#' plot(madElev, main='Original')
#' plot(elevResamp, main='Resampled')
#' }
#' @export

fasterProjectRaster <- function(
	rast,
	template,
	method = 'bilinear',
	grassDir = NULL,
	alreadyInGrass = FALSE,
	grassToR = TRUE,
	...
) {

	flags <- c('quiet', 'overwrite')
	
	# initialize GRASS
	fromRastGrass <- initGrass(alreadyInGrass, rast=rast, location='fromRast', vect=NULL, grassDir=grassDir)
	tempDir <- attr(fromRastGrass, 'tempDir')
	exportRastToGrass(rast, vname='rast')
	toRastGrass <- initGrass(alreadyInGrass, rast=template, vect=NULL, location='default', tempDir=tempDir, grassDir=grassDir)
	exportRastToGrass(template, vname='template')

	# export raster to project to GRASS (projects it automatically)
	
	# export raster to GRASS
	rgrass7::execGRASS('r.proj', location='fromRast', mapset='PERMANENT', input='rast', output='resampled', method=method, flags=flags)

	# return
	if (grassToR) {
	
		out <- rgrass7::readRAST('resampled')
		out <- raster::raster(out)
		names(out) <- 'resampled'
		out
		
	}
	
}

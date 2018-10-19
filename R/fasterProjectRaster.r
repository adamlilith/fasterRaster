#' Project and resample a raster
#'
#' This function is a potentially faster version of the \code{\link[raster]{projectRaster}} function in the \pkg{raster} package for projecting (and resampling) a raster. It requires the user has GRASS GIS Version 7 installed.
#' @param rast Either a raster or the full path and name of a raster object. This raster will be projected.
#' @param template Raster layer \emph{or} full path and file name of a raster to serve as a template for projecting.
#' @param method Character, method for resampling cells:
#' \itemize{
#' 		\item \code{bilinear}: Bilinear interpolation (default; uses values from 4 cells).
#' 		\item \code{bicubic}: Bicubic interpolation (uses data from 16 cells).
#' 		\item \code{nearest}: Nearest neighbor (uses data from 1 cell).
#' 		\item \code{lanczos}: Lanczos interpolation (uses data from 25 cells).
#' }
#' @param grassLoc Either \code{NULL} or a 3-element character vector. If the latter, the first element is the base path to the installation of GRASS, the second the version number, and the third the install type for GRASS.  For example, \code{c('C:/OSGeo4W64/', 'grass-7.4.1', 'osgeo4W')}. See \code{\link[link2GI]{linkGRASS7}} for further help. If \code{NULL} (default) the an installation of GRASS is searched for; this may take several minutes.
#' @param grassInit Logical, if \code{TRUE} (default) then a new GRASS session is initialized. If \code{FALSE} then it is assumed a GRASS session has been initialized using the raster in \code{resampled}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session.
#' @param grassToR Logical, if \code{TRUE} (default) then the product of the calculations will be returned to R. If \code{FALSE}, then the product is left in the GRASS session and named \code{rast}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param ... Arguments to pass to \code{\link[rgrass7]{execGRASS}} when used for rasterizing (i.e., function \code{r.resamp.interp} in GRASS).
#' @return If \code{grassToR} if \code{TRUE}, then a raster or raster stack with the same extent, resolution, and coordinate reference system as \code{rast}. Otherwise, a raster with the name \code{resampled} is written into the GRASS session.
#' @details Note that it is not uncommon to get the warning "Projection of dataset does not appear to match the current mapset" (followed by more information). If the coordinate reference systems match, then the cause is likely due to extra information being stored in one of the spatial object's coordinate reference system slot (e.g., an EPSG code in addition to the other proj4string information), in which case it can probably be safely ignored.  
#' See (r.slope.aspect)[https://grass.osgeo.org/grass74/manuals/r.resamp.interp.html] for more details.  Note that if you get an error saying "", then you should add the EPSG code to the beginning of the raster and vector coordinate reference system string (its "proj4string"). For example, \code{proj4string(x) <- CRS('+init=epsg:32738')}. EPSG codes for various projections, datums, and locales can be found at (Spatial Reference)[http://spatialreference.org/].  
#' **Note that the warning "Projection of dataset does not appear to match current location," will almost always appear. It can be safely ignored.**
#' @seealso \code{\link[raster]{projectRaster}}
#' @examples
#' \donttest{
#' # could also use projectRaster() which
#' # may be faster in this example
#' # change this according to where GRASS 7 is installed on your system
#' grassLoc <- c('C:/OSGeo4W64/', 'grass-7.4.1', 'osgeo4W')
#'
#' data(madElev)
#' data(madForest2000)
#' 
#' elevResamp <- fasterProjectRaster(madElev, madForest2000, grassLoc=grassLoc)
#' # elevResamp <- projectRaster(elev, madForest2000)
#' par(mfrow=c(1, 2))
#' plot(elev, main='Original')
#' plot(elevResamp, main='Resampled')
#' }
#' @export

fasterProjectRaster <- function(
	rast,
	template,
	method = 'bilinear',
	use = 'grass',
	grassLoc = NULL,
	grassInit = TRUE,
	grassToR = TRUE,
	...
) {

	flags <- c('quiet', 'overwrite')
	
	# load raster
	if (class(rast) == 'character') rast <- raster::raster(rast)
	if (class(template) == 'character') template <- raster::raster(template)

	# get template CRS
	p4s <- sp::proj4string(template)
	
	# initialize GRASS
	if (grassInit) link2GI::linkGRASS7(template, default_GRASS7=grassLoc, gisdbase=raster::tmpDir(), location='temp')

	exportRastToGrass(rast, vname='rast')
	
	# export raster to GRASS
	rgrass7::execGRASS('r.resamp.interp', input='rast', output='resampled', method=method, flags=flags, ...)

	# return
	if (grassToR) {
	
		out <- rgrass7::readRAST('resampled')
		out <- raster::raster(out)
		proj4string(out) <- p4s
		names(out) <- 'resampled'
		out
		
	}
	
}

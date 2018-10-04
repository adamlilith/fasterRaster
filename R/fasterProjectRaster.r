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
#' @param initGrass Logical, if \code{TRUE} (default) then a new GRASS session is initialized. If \code{FALSE} then it is assumed a GRASS session has been initialized using the raster in \code{resampled}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session.
#' @param backToR Logical, if \code{TRUE} (default) then the product of the calculations will be returned to R. If \code{FALSE}, then the product is left in the GRASS session and named \code{rast}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param ... Arguments to pass to \code{\link[rgrass7]{execGRASS}} when used for rasterizing (i.e., function \code{r.resamp.interp} in GRASS).
#' @return If \code{backToR} if \code{TRUE}, then a raster or raster stack with the same extent, resolution, and coordinate reference system as \code{rast}. Otherwise, a raster with the name \code{resampled} is written into the GRASS session.
#' @details Note that it is not uncommon to get the warning "Projection of dataset does not appear to match the current mapset" (followed by more information). If the coordinate reference systems match, then the cause is likely due to extra information being stored in one of the spatial object's coordinate reference system slot (e.g., an EPSG code in addition to the other proj4string information), in which case it can probably be safely ignored.  
#' See (r.slope.aspect)[https://grass.osgeo.org/grass74/manuals/r.resamp.interp.html] for more details.  Note that if you get an error saying "", then you should add the EPSG code to the beginning of the raster and vector coordinate reference system string (its "proj4string"). For example, \code{proj4string(x) <- CRS('+init=epsg:32738')}. EPSG codes for various projections, datums, and locales can be found at (Spatial Reference)[http://spatialreference.org/].  
#' **Note that the warning "Projection of dataset does not appear to match current location," will almost always appear. It can be safely ignored.**
#' @seealso \code{\link[raster]{projectRaster}}
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
#' fragRasts <- fragmentation(forest2000)
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

fasterProjectRaster <- function(
	rast,
	template,
	method = 'bilinear',
	grassLoc = NULL,
	initGrass = TRUE,
	backToR = TRUE,
	...
) {

	flags <- c('quiet', 'overwrite')
	
	# load raster
	if (class(rast) == 'character') rast <- raster::raster(rast)
	if (class(template) == 'character') template <- raster::raster(template)

	# get template CRS
	p4s <- sp::proj4string(template)
	
	# initialize GRASS
	if (initGrass) link2GI::linkGRASS7(template, default_GRASS7=grassLoc, gisdbase=raster::tmpDir(), location='temp')

	exportRastToGrass(rast, vname='rast')
	
	# export raster to GRASS
	rgrass7::execGRASS('r.resamp.interp', input='rast', output='resampled', method=method, flags=flags, ...)

	# return
	if (backToR) {
	
		out <- rgrass7::readRAST('resampled')
		out <- raster::raster(out)
		proj4string(out) <- p4s
		names(out) <- 'resampled'
		out
		
	}
	
}

#' Calculate longitude and latitude rasters
#'
#' This function is a potentially faster version of the \code{\link[enmSdm]{longLatRasters}} function in the \pkg{enmSdm} package which calculates two rasters, one with cell values equaling the longitude of their centers and the other with cell values equaling the latitude of their centers. Please note that the raster must be projected (i.e., not in a geographic coordinate system, not in WGS84, NAD83, etc.) for the GRASS7 \code{r.longlat} module, on which this function is based, to work.
#' @param rast Either a raster or the name of a raster in an existing GRASS session. This serves as a template for calculating longitude and latitude rasters.
#' @param mask Raster layer, serves as a mask for the longitude/latitude rasters. Cells that have \code{NA}s in \code{mask} will also have \code{NA}s in the output rasters. Note that the mask is \emph{only} used if the output is sent back to R (\code{grassToR} is \code{TRUE}) and not kept in the GRASS session created for the calculations.
#' @param grassDir Either \code{NULL} or a 3-element character vector. If the latter, the first element is the base path to the installation of GRASS, the second the version number, and the third the install type for GRASS.  For example, \code{c('C:/OSGeo4W64/', 'grass-7.4.1', 'osgeo4W')}. See \code{\link[link2GI]{linkGRASS7}} for further help. If \code{NULL} (default) the an installation of GRASS is searched for; this may take several minutes.
#' @param alreadyInGrass Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{rast}. If \code{FALSE}, use a raster already in GRASS with the name given by \code{rast}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#' @param grassToR Logical, if \code{TRUE} (default) then the product of the calculations will be returned to R. If \code{FALSE}, then the product is left in the GRASS session and named \code{longitude} and \code{latitude}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param ... Arguments to pass to \code{\link[rgrass7]{execGRASS}} when used for rasterizing (i.e., function \code{r.latlong} in GRASS).
#' @return If \code{grassToR} if \code{TRUE}, then a raster stack with the same extent, resolution, and coordinate reference system as \code{rast}. Otherwise, rasters with the names of \code{longitude} and \code{latitude} are written into the GRASS session.
#' @details See \href{r.latlong}{https://grass.osgeo.org/grass78/manuals/r.latlong.html} for more details. Note that if you get an error saying "", then you should add the EPSG code to the beginning of the raster coordinate reference system string (its "proj4string"). For example, \code{proj4string(rast) <- CRS('+init=epsg:32738')}. EPSG codes for various projections, datums, and locales can be found at \href{Spatial Reference}{http://spatialreference.org}.
#' @seealso \code{\link[enmSdm]{longLatRasters}}
#' @examples
#' \donttest{
#' # change this to where GRASS 7 is installed on your system
#' grassDir <- 'C:/Program Files/GRASS GIS 7.8'
#' 
#' # note that in the example below using enmSdm::longLatRasters()
#' # will be *much* faster than using fasterLongLatRasters()
#' # because the template raster is so small
#' 
#' data(madForest)
#' ll1 <- enmSdm::longLatRasters(madForest2000)
#' ll2 <- fasterLongLatRasters(madForest2000, grassDir=grassDir)
#' plot(ll2)
#' }
#' @export

fasterLongLatRasters <- function(
	rast,
	mask = NULL,
	grassDir = NULL,
	alreadyInGrass = FALSE,
	grassToR = TRUE,
	...
) {

	flags <- c('quiet', 'overwrite')
	
	# load spatial object and raster
	if (class(rast) == 'character') rast <- raster::raster(rast)

	# get CRS
	p4s <- sp::proj4string(rast)
	
	# initialize GRASS
	input <- .initGrass(alreadyInGrass, rast=rast, vect=NULL, grassDir=grassDir)
	
	# calculate longitude/latitude
	rgrass7::execGRASS('r.latlong', input=input, output='longitude', flags=c(flags, 'l'), ...)
	rgrass7::execGRASS('r.latlong', input=input, output='latitude', flags=flags, ...)
	
	if (grassToR) {
		long <- rgrass7::readRAST('longitude')
		lat <- rgrass7::readRAST('latitude')
		long <- raster::raster(long)
		lat <- raster::raster(lat)
		out <- raster::stack(long, lat)
		if (!is.null(mask)) out <- (mask * 0L + 1L) * out
		sp::proj4string(out) <- p4s
		names(out) <- c('longitude', 'latitude')
		out
	}

}

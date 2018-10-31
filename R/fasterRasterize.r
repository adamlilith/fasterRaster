#' Rasterize using a call to GRASS GIS.
#'
#' This function is a potentially faster version of the \code{\link[raster]{rasterize}} function in the \pkg{raster} package which converts a spatial points, lines, or polygon into a raster based on a "template" raster. All cells covered by the spatial object can either have values taken from the spatial object or a user-defined.
#' @param vect Either a SpatialPoints, SpatialPointsDataFrame, SpatialLines, SpatialLinesDataFrame, SpatialPolygons, or SpatialPolygonsDataFrame or the name of such a vector dataset already in a GRASS session.
#' @param rast Either a raster or the name of a raster in an existing GRASS session. This serves as a template for the new raster.
#' @param use Character, indicates the types of values to be "burned" to the raster. Options include
#' \itemize{
#' \item \code{value} (default): a user-define value given by \code{value}
#' \item \code{field}: values directly from a field in \code{vect} named by \code{field}
#' \item \code{category}: values according to which polygon is covered by a cell named in \code{field}
#' \item \code{z}: z-coordinate (points or contours only)
#' \item \code{direction}: flow direction (lines only)
#' }
#' @param field Name of column in \code{vect} with values or category labbels to which to burn to the raster.
#' @param value Numeric, value to burn to each cell overlapped by the spatial object in \code{vect}.
#' @param grassDir Either \code{NULL} or a 3-element character vector. If the latter, the first element is the base path to the installation of GRASS, the second the version number, and the third the install type for GRASS.  For example, \code{c('C:/OSGeo4W64/', 'grass-7.4.1', 'osgeo4W')}. See \code{\link[link2GI]{linkGRASS7}} for further help. If \code{NULL} (default) the an installation of GRASS is searched for; this may take several minutes.
#' @param alreadyInGrass Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{rast}. If \code{FALSE}, use a raster already in GRASS with the name given by \code{rast}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#' @param grassToR Logical, if \code{TRUE} (default) then the product of the calculations will be returned to R. If \code{FALSE}, then the product is left in the GRASS session and named \code{vectAsRast}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param ... Arguments to pass to \code{\link[rgrass7]{execGRASS}} when used for rasterizing (i.e., function \code{v.to.rast} in GRASS).
#' @return If \code{grassToR} if \code{TRUE}, then a raster with the same extent, resolution, and coordinate reference system as \code{rast}. Otherwise, a raster with the name of \code{vectAsRast} is written into the GRASS session.
#' @details See (v.to.rast)[https://grass.osgeo.org/grass74/manuals/v.to.rast.html] for more details.  Note that if you get an error saying "", then you should add the EPSG code to the beginning of the raster and vector coordinate reference system string (their "proj4string"). For example, \code{proj4string(x) <- CRS('+init=epsg:32738')}. EPSG codes for various projections, datums, and locales can be found at (Spatial Reference)[http://spatialreference.org/].
#' @seealso \code{\link[raster]{rasterize}}
#' @examples
#' \donttest{
#' # change this according to where GRASS 7 is installed on your system
#' grassDir <- c('C:/OSGeo4W64/', 'grass-7.4.1', 'osgeo4W')
#' 
#' data(mad0)
#' data(madForest2000)
#' 
#' # could also use rasterize() or mask() which may
#' # be faster in this example
#' madMask <- fasterRasterize(mad0, madForest2000, grassDir=grassDir)
#' # madMask <- rasterize(mad0, madForest2000)
#' # madMask <- mask(madForest2000, mad0)
#' plot(madMask, main='Portion of Eastern Madagascar')
#' plot(mad0, add=TRUE)
#' }
#' @export

fasterRasterize <- function(
	vect,
	rast,
	use = 'value',
	value = 1,
	field = NULL,
	grassDir = NULL,
	alreadyInGrass = FALSE,
	grassToR = TRUE,
	...
) {

	flags <- c('quiet', 'overwrite')
	
	# get CRS
	p4s <- sp::proj4string(rast)
	
	# feature type
	featType <- if (class(vect) == 'SpatialPointsDataFrame') {
		'point'
	} else if (class(vect) == 'SpatialLinesDataFrame') {
		'line'
	} else if (class(vect) == 'SpatialPolygonsDataFrame') {
		'area'
	}
	
	# initialize GRASS
	input <- .initGrass(alreadyInGrass, rast=rast, vect=vect, grassDir=grassDir)

	# rasterize
	if (use == 'field') {
		rgrass7::execGRASS('v.to.rast', input=input[['vectNameInGrass']], output='vectAsRast', use='attr', attribute_column=field, type=featType, flags=flags, ...)
	} else if (use == 'category') {
		rgrass7::execGRASS('v.to.rast', input=input[['vectNameInGrass']], output='vectAsRast', use='cat', label_column=field, type=featType, flags=flags, ...)
	} else if (use == 'value') {
		rgrass7::execGRASS('v.to.rast', input=input[['vectNameInGrass']], output='vectAsRast', use='val', value=value, type=featType, flags=flags, ...)
	} else {
		rgrass7::execGRASS('v.to.rast', input=input[['vectNameInGrass']], output='vectAsRast', use=use, flags=flags, type=featType, ...)
	}

	# get raster back to R
	if (grassToR) {
	
		out <- rgrass7::readRAST('vectAsRast')
		out <- raster::raster(out)
		sp::proj4string(out) <- p4s
		names(out) <- 'vectAsRast'
		out
		
	}

}

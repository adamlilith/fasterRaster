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
#' @param burn \code{NULL} or any of \code{'point'}, \code{'line'}, \code{'area'}, \code{'boundary'}, or \code{'centroid'}. This determines the manner in which the vector data is "burned" to the raster. If \code{NULL} (default), then SpatialPoints and SpatialPointsDataFrame objects will rasterized as points, SpatialLines and SpatialLinesDataFrame objects as lines, and SpatialPolygons and SpatialPolygonsDataFrame objects as areas. See (v.to.rast}{https://grass.osgeo.org/grass78/manuals/v.to.rast.html} for more details.
#' @param grassDir Character or \code{NULL} (default). Name of the directory in which GRASS is installed. Example: \code{'C:/Program Files/GRASS GIS 7.8'}. If this is \code{NULL}, R will search for the directory in which GRASS is installed. This usually fails, or if it succeeds, takes several minutes.
#' @param alreadyInGrass Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{rast}. If \code{FALSE}, use a raster already in GRASS with the name given by \code{rast}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#' @param grassToR Logical, if \code{TRUE} (default) then the product of the calculations will be returned to R. If \code{FALSE}, then the product is left in the GRASS session and named \code{vectToRast}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param outGrassName Character. Name of output in GRASS. This is useful if you want to refer to the output object in GRASS later in a session.
#' @param ... Arguments to pass to \code{\link[rgrass7]{execGRASS}} when calculating horizon height (i.e., function \code{r.sun} in GRASS).
#' @param ... Arguments to pass to \code{\link[rgrass7]{execGRASS}} when used for rasterizing (i.e., function \code{v.to.rast} in GRASS).
#' @return If \code{grassToR} if \code{TRUE}, then a raster with the same extent, resolution, and coordinate reference system as \code{rast}. Otherwise, a raster with the name given by \code{outRastName} is written into the GRASS session.
#' @details See (v.to.rast}{https://grass.osgeo.org/grass78/manuals/v.to.rast.html} for more details.  Note that if you get an error saying "", then you should add the EPSG code to the beginning of the raster and vector coordinate reference system string (their "proj4string"). For example, \code{proj4string(x) <- CRS('+init=epsg:32738')}. EPSG codes for various projections, datums, and locales can be found at \href{Spatial Reference}{http://spatialreference.org}.
#' @seealso \code{\link[raster]{rasterize}}
#' @examples
#' \donttest{
#' # change this according to where GRASS 7 is installed on your system
#' grassDir <- 'C:/Program Files/GRASS GIS 7.8'
#' 
#' data(mad0)
#' data(madForest2000)
#' 
#' # could also use rasterize() or mask() from the raster package which may
#' # be faster in this example
#' madMask <- fasterRasterize(vect=mad0, rast=madForest2000, grassDir=grassDir)
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
	burn = NULL,
	grassDir = NULL,
	alreadyInGrass = FALSE,
	grassToR = TRUE,
	outGrassName = 'vectToRast',
	...
) {

	flags <- c('quiet', 'overwrite')
	
	# feature type
	vectClass <- class(vect)
	if (is.null(burn)) {
		
		burn <- if (vectClass %in% c('SpatialPoints', 'SpatialPointsDataFrame')) {
			'point'
		} else if (vectClass %in% c('SpatialLines', 'SpatialLinesDataFrame')) {
			'line'
		} else if (vectClass %in% c('SpatialPolygons', 'SpatialPolygonsDataFrame')) {
			'area'
		}

	} else {
	
		if (!(burn %in% c('point', 'line', 'area', 'boundary', 'centroid'))) {
			stop('Argument "burn" must be NULL or one of "point", "line", "area", "boundary",\nor "centroid" and match the type of argument "vect".')
		}
		
		if (burn %in% c('point', 'centroid') & !(vectClass %in% c('SpatialPoints', 'SpatialPointsDataFrame'))) {
			stop('Argument "burn" must be either "point" or "centroid" if\nargument "vect" is a SpatialPoints or SpatialPointsDataFrame.')
		}
		
	}
		
	# initialize GRASS
	input <- initGrass(alreadyInGrass, rast=rast, vect=vect, grassDir=grassDir)

	# rasterize
	if (use == 'field') {
		rgrass7::execGRASS('v.to.rast', input=input[['vectNameInGrass']], output=outGrassName, use='attr', attribute_column=field, type=burn, flags=flags, ...)
	} else if (use == 'category') {
		rgrass7::execGRASS('v.to.rast', input=input[['vectNameInGrass']], output=outGrassName, use='cat', label_column=field, type=burn, flags=flags, ...)
	} else if (use == 'value') {
		rgrass7::execGRASS('v.to.rast', input=input[['vectNameInGrass']], output=outGrassName, use='val', value=value, type=burn, flags=flags, ...)
	} else {
		rgrass7::execGRASS('v.to.rast', input=input[['vectNameInGrass']], output=outGrassName, use=use, flags=flags, type=burn, ...)
	}

	# get raster back to R
	if (grassToR) {
	
		out <- rgrass7::readRAST(outGrassName)
		out <- raster::raster(out)
		names(out) <- outGrassName
		out
		
	}

}

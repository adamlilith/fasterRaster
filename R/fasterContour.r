#' Calculate contour vectors from a raster
#'
#' This function creates a vector (\code{SpatialLines} or \code{SpatialLinesDataFrame}) object from a raster representing contour lines in the raster. It utilizes the GRASS function \code{r.contour}.
#' @param rast Either a raster or the name of a raster in an existing GRASS session with values representing elevation (typically in meters).
#' @param levels Numeric vector. Levels of values in \code{rast} at which contours should be drawn. You can specify contour levels using this argument or by providing values for \code{step}, \code{minlevel}, and \code{maxlevel}. \code{levels} will override use of \code{step}, if both of them are specified.
#' @param step Numeric. Increment between contour levels.
#' @param minlevel,maxlevel Numeric or \code{NULL} (default). Minimum and maximum contour levels. If \code{NULL} and \code{step} is not \code{NULL}, then the minimum and maximum values in the raster will be used.
#' @param cut Integer >= 0. Minimum number of points necessary to generate a contour line. A value of 0 implies no limit. Default is 2.
#' @param grassDir Character or \code{NULL} (default). Name of the directory in which GRASS is installed. Example: \code{'C:/Program Files/GRASS GIS 7.8'}. If this is \code{NULL}, R will search for the directory in which GRASS is installed. This usually fails, or if it succeeds, takes several minutes.
#' @param alreadyInGrass Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{rast}. If \code{FALSE}, use a raster already in GRASS with the name given by \code{rast}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#' @param grassToR Logical, if \code{TRUE} (default) then the product of the calculations will be returned to R. If \code{FALSE}, then the product is left in the GRASS session and named \code{longitude} and \code{latitude}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param ... Arguments to pass to \code{\link[rgrass7]{execGRASS}} when calculating horizon height (i.e., function \code{r.horizon} in GRASS).
#' @return If \code{grassToR} if \code{TRUE}, then a \code{SpatialLines} or \code{SpatialLinesDataFrame} object with the same coordinate reference system as \code{rast}. Otherwise, a vector is written into the GRASS session. The name of this vector is as \code{contours}.
#' @details See (r.contour)[https://grass.osgeo.org/grass78/manuals/r.contour.html] for more details. Note that if you get an error saying "", then you should add the EPSG code to the beginning of the raster coordinate reference system string (its "proj4string"). For example, \code{proj4string(rast) <- CRS('+init=epsg:32738')}. EPSG codes for various projections, datums, and locales can be found at (Spatial Reference)[http://spatialreference.org/].
#' @seealso
#' @examples
#' \donttest{
#' # change this to where GRASS 7 is installed on your system
#' grassDir <- 'C:/Program Files/GRASS GIS 7.8'
#'
#' data(madElev)
#' conts1 <- fasterContour(madElev, grassDir=grassDir)
#' conts2 <- fasterContour(madElev, levels=c(0, 10, 20, 30, 40, 50, 100, 500),
#' grassDir=grassDir)
#' conts3 <- fasterContour(madElev, minlevel=0, maxlevel=500, step=100,
#' grassDir=grassDir)
#' plot(madElev)
#' plot(conts1, add=TRUE)
#' }
#' @export

fasterContour <- function(
	rast,
	step = NULL,
	minlevel = NULL,
	maxlevel = NULL,
	levels = seq(minValue(rast), maxValue(rast), length.out=5),
	cut = 2,
	grassDir = NULL,
	alreadyInGrass = FALSE,
	grassToR = TRUE,
	...
) {

	flags <- c('quiet', 'overwrite')
	
	# initialize GRASS
	input <- initGrass(alreadyInGrass, rast=rast, vect=NULL, grassDir=grassDir)
	
	if (!is.null(step) & is.null(minlevel)) minlevel <- raster::minValue(rast)
	if (!is.null(step) & is.null(maxlevel)) maxlevel <- raster::maxValue(rast)
	
	# execute
	if (!is.null(levels)) {
		rgrass7::execGRASS('r.contour', input=input, levels=levels, cut=cut, output='contours', flags=flags, ...)
	} else if (!is.null(step) & is.null(minlevel) & is.null(maxlevel)) {
		rgrass7::execGRASS('r.contour', input=input, step=step, minlevel=minlevel, maxlevel=maxlevel, cut=cut, output='contours', flags=flags, ...)
	}
	
	if (grassToR) {

		out <- rgrass7::readVECT('contours')
		out
		
	}
	
}

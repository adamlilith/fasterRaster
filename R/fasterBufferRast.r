#' Add a buffer to a raster.
#'
#' This function is a potentially faster version of the \code{\link[raster]{buffer}} function in the \pkg{raster} package for calculating a buffer around non-\code{NA} cells (or cells with values of 0) in a raster. The output will be a raster.
#' @param rast Either a raster or the name of a raster in an existing GRASS session.
#' @param width Numeric. Maximum distance cells must be from focal cells to be within the buffer. Note that this function can only handle one value of \code{width} (unlike the function \code{r.buffer} in GRASS).
#' @param units Either \code{meters} (default), \code{kilometers}, \code{feet}, \code{miles}, or \code{nautmiles}. Indicates the units of \code{width}.
#' @param ignore Either {NA} (default) or 0. The buffer will be drawn around cells that are not {NA} or 0, depending on this value.
#' @param lowMemory Logical. If \code{FALSE} (default) use faster, memory-intensive procedure. If \code{TRUE} then use the slower, low-memory version. To help decide, consider using the low-memory version on a system with 1 GB of RAM for a raster larger than 32000x32000 cells on a system, or for a system with  with 8 GB of RAM a raster larger than 90000x90000 cells.
#' @param grassDir Either \code{NULL} or a 3-element character vector. If the latter, the first element is the base path to the installation of GRASS, the second the version number, and the third the install type for GRASS.  For example, \code{c('C:/OSGeo4W64/', 'grass-7.4.1', 'osgeo4W')}. See \code{\link[link2GI]{linkGRASS7}} for further help. If \code{NULL} (default) the an installation of GRASS is searched for; this may take several minutes.
#' @param alreadyInGrass Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{rast}. If \code{FALSE}, use a raster already in GRASS with the name given by \code{rast}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#' @param grassToR Logical, if \code{TRUE} (default) then the product of the calculations will be returned to R. If \code{FALSE}, then the product is left in the GRASS session and named \code{slope}, \code{aspect}, \code{profileCurve}, \code{tanCurve}, \code{eastWestSlope}, or \code{northSouthSlope}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param ... Other arguments (unused).
#' @return If \code{grassToR} if \code{TRUE}, then a raster with the same extent, resolution, and coordinate reference system as \code{rast}. Otherwise, a raster with the name \code{rastBuffer} is written into the GRASS session.
#' @details See (r.buffer)[https://grass.osgeo.org/grass74/manuals/r.buffer.html] for more details.  Note that if you get an error saying "", then you should add the EPSG code to the beginning of the raster and vector coordinate reference system string (its "proj4string"). For example, \code{proj4string(x) <- CRS('+init=epsg:32738')}. EPSG codes for various projections, datums, and locales can be found at (Spatial Reference)[http://spatialreference.org/].
#' @seealso \code{\link[raster]{buffer}} in the \pkg{raster} package, \code{\link[rgeos]{gBuffer}} in the \pkg{rgeos} package
#' @examples
#' \donttest{
#' OS Geo version of GRASS:
#' # change this according to where GRASS 7 is installed on your system
#' grassDir <- c('C:/OSGeo4W64/', 'grass-7.4.1', 'osgeo4W')
#'
#' Stand-alone version of GRASS:
#' # change this according to where GRASS 7 is installed on your system
#' grassDir <- NULL
#'
#' data(madForest2000)
#' rastBuff <- fasterBufferRast(madForest2000, width=2, units='kilometers',
#' grassDir=grassDir)
#' plot(rastBuff, col=c('green', 'gray'))
#' legend('topright', legend=c('forest', 'buffer'), fill=c('green', 'gray'))
#' }
#' @export

fasterBufferRast <- function(
	rast,
	width,
	units = 'meters',
	ignore = NA,
	lowMemory = FALSE,
	grassDir = NULL,
	alreadyInGrass = FALSE,
	grassToR = TRUE,
	...
) {

	flags <- c('quiet', 'overwrite')
	if (!is.na(ignore) && ignore == 0) flags <- c(flags, 'z')

	# initialize GRASS and export raster to GRASS
	input <- .initGrass(alreadyInGrass, rast=rast, vect=NULL, grassDir=grassDir)
		
	# buffer
	fx <- if (lowMemory) {
		'r.buffer.lowmem'
	} else {
		'r.buffer'
	}

	rgrass7::execGRASS(fx, input=input, output='rastBuffer', distances=width, units=units, flags=flags)

	# return
	if (grassToR) {

		out <- rgrass7::readRAST('rastBuffer')
		names(out) <- 'rastBuffer'
		if (class(out) != 'RasterLayer') out <- raster(out)
		out

	}

}

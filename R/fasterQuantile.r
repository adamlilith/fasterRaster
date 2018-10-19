#' Compute quantiles for a raster.
#'
#' This function is a potentially faster version of the function \code{quantile(raster, probs)} for calculating the quantiles of a raster. This function will also work on rasters too big to load into memory using the \pkg{raster} package.
#' @param rast Either a raster or the full path and name of a raster object.
#' @param probs A numeric list of quantiles to calculate.
#' @param grassLoc Either \code{NULL} or a 3-element character vector. If the latter, the first element is the base path to the installation of GRASS, the second the version number, and the third the install type for GRASS.  For example, \code{c('C:/OSGeo4W64/', 'grass-7.4.1', 'osgeo4W')}. See \code{\link[link2GI]{linkGRASS7}} for further help. If \code{NULL} (default) the an installation of GRASS is searched for; this may take several minutes.
#' @param grassInit Logical, if \code{TRUE} (default) then a new GRASS session is initialized. If \code{FALSE} then it is assumed a GRASS session has been initialized using the raster in \code{rast}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session.
#' @param grassToR Logical, if \code{TRUE} (default) then the product of the calculations will be returned to R. If \code{FALSE}, then the product is left in the GRASS session and named \code{slope}, \code{aspect}, \code{profileCurve}, \code{tanCurve}, \code{eastWestSlope}, or \code{northSouthSlope}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param ... Arguments to pass to \code{\link[rgrass7]{execGRASS}}.
#' @return A named vector of the values for each quantile named in \code{probs}.
#' @details See (r.buffer)[https://grass.osgeo.org/grass74/manuals/r.buffer.html] for more details.  Note that if you get an error saying "", then you should add the EPSG code to the beginning of the raster and vector coordinate reference system string (its "proj4string"). For example, \code{proj4string(x) <- CRS('+init=epsg:32738')}. EPSG codes for various projections, datums, and locales can be found at (Spatial Reference)[http://spatialreference.org/].
#' @seealso \code{\link[stats]{quantile}} in the \pkg{base} package and \code{\link[raster]{quantile}} in the \pkg{raster} package
#' @examples
#' \donttest{
#' # change this according to where GRASS 7 is installed on your system
#' grassLoc <- c('C:/OSGeo4W64/', 'grass-7.4.1', 'osgeo4W')
#'
#' data(madForest2000)
#'
#' # calculate distance to forest
#' distToForest <- fasterRastDistance(madForest2000, grassLoc=grassLoc)
#'
#' # calculate quantiles
#' probs <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
#' quants <- fasterQuantile(distToForest, probs=probs, grassLoc=grassLoc)
#' }
#' @export

fasterQuantile <- function(
	rast,
	probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
	grassLoc = NULL,
	grassInit = TRUE,
	...
) {

	flags <- c('quiet', 'overwrite')

	probs <- 100 * probs

	# load raster
	if (class(rast) == 'character') rast <- raster::raster(rast)

	# initialize GRASS
	if (grassInit) link2GI::linkGRASS7(rast, default_GRASS7=grassLoc, gisdbase=raster::tmpDir(), location='temp')

	# export raster to GRASS
	exportRastToGrass(rast, vname='rast')

	# temp file for output
	tempFile <- tempfile(pattern = 'file', tmpdir = tempdir(), fileext = '.csv')

	# calculate
	rgrass7::execGRASS('r.quantile', input='rast', file=tempFile, percentiles=probs, flags=flags)

	# get output
	grassQuants <- read.csv(tempFile, header=FALSE)

	out <- rep(NA, length(probs))
	names(out) <- paste0('prob_', probs)
	for (i in 1:nrow(grassQuants)) {
		thisRow <- grassQuants[i, ]
		thisRow <- strsplit(thisRow, split=':')
		out[i] <- as.numeric(thisRow[[1]][3])
	}

	out

}

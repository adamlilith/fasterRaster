#' Quantiles for a raster
#'
#' This function is a potentially faster version of the function \code{quantile(raster, probs)} for calculating the quantiles of a raster. This function will also work on rasters too big to load into memory using the \pkg{terra} package.
#' @param rast Either a raster or the name of a raster in an existing GRASS session.
#' @param probs A numeric list of quantiles to calculate.
#' @param grassDir Character or \code{NULL} (default). Name of the directory in which GRASS is installed. Example for a Windows system: \code{'C:/Program Files/GRASS GIS 8.2'}. If this is \code{NULL}, R will search for the directory in which GRASS is installed. This usually fails, or if it succeeds, takes several minutes.
#' @param alreadyInGrass Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{rast}. If \code{FALSE}, use a raster already in GRASS with the name given by \code{rast}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#' @param grassToR Logical, if \code{TRUE} (default) then the output will be returned to R. If \code{FALSE}, then the output is left in the GRASS session and named the value in \code{outGrassName} \code{slope}, \code{aspect}, \code{profileCurve}, \code{tanCurve}, \code{eastWestSlope}, or \code{northSouthSlope}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param ... Arguments to pass to \code{\link[rgrass]{execGRASS}}.
#' @return A named vector of the values for each quantile named in \code{probs}.
#' @details See the documentation for the GRASS module \code{r.quantile}{https://grass.osgeo.org/grass82/manuals/r.quantile.html}.
#' @seealso \code{\link[stats]{quantile}} in the \pkg{base} package and \code{\link[terra]{quantile}} in the \pkg{terra} package
#' @examples
#' \donttest{
#' # change this according to where GRASS 7 is installed on your system
#' grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
#' grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac
#'
#' data(madElev)
#' probs <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
#' quants <- fasterQuantile(rast=madElev, probs=probs, grassDir=grassDir)
#' quants
#' # in this case, the raster package is faster:
#' quantile(madElev, probs=probs, na.rm=TRUE)
#'
#' }
#' @export

fasterQuantile <- function(
	rast,
	probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
	grassDir = options('grassDir'),
	alreadyInGrass = FALSE,
	...
) {

	flags <- c('quiet', 'overwrite')

	probs <- 100 * probs

	# initialize GRASS
	input <- initGrass(alreadyInGrass, rast=rast, vect=NULL, grassDir=grassDir)

	# temp file for output
	tempFile <- tempfile(pattern = 'file', tmpdir = tempdir(), fileext = '.csv')

	# calculate
	rgrass::execGRASS('r.quantile', input=input, file=tempFile, percentiles=probs, flags=flags)

	# get output
	grassQuants <- read.csv(tempFile, header=FALSE)

	out <- rep(NA, length(probs))
	names(out) <- paste0('prob_', probs / 100)
	for (i in 1:nrow(grassQuants)) {
		thisRow <- grassQuants[i, ]
		thisRow <- strsplit(thisRow, split=':')
		out[i] <- as.numeric(thisRow[[1]][3])
	}

	out

}

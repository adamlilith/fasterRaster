#' Buffer cells of a raster
#'
#' Calculate a buffer around non-\code{NA} cells or cells with values of 0 in a raster. The output will be a raster. This function utilizes the \pkg{GRASS} module \href{https://grass.osgeo.org/grass82/manuals/r.buffer.html}{\code{r.buffer}} and is the same or at least similar to the \pkg{terra} function \code{\link[terra]{buffer}}.

#' This function is a potentially faster version of the \code{\link[terra]{buffer}} function in the \pkg{terra} package for calculating a buffer around non-\code{NA} cells or cells with values of 0 in a raster. The output will be a raster.
#'
#' @param rast Either a \code{SpatRaster} or the name of a raster in an existing GRASS session.
#' @param width Numeric. Maximum distance cells must be from focal cells to be within the buffer. Note that this function can only handle one value of \code{width} (unlike the function \code{r.buffer} in GRASS).
#' @param units Either \code{'meters'} (default), \code{'kilometers'}, \code{'feet'}, \code{'miles'}, or \code{'nautmiles'}. Indicates the units of \code{width}.
#' @param ignore Either {NA} (default) or 0. The buffer will be drawn around cells that are not {NA} or not 0, depending on this value.
#' @param lowMemory Logical. If \code{FALSE} (default) use faster, memory-intensive procedure. If \code{TRUE} then use the slower, low-memory version. To help decide, consider using the low-memory version on a system with 1 GB of RAM for a raster larger than about 32000x32000 cells, or for a system with  with 8 GB of RAM a raster larger than about 90000x90000 cells.
#' @param grassDir Character or \code{NULL} (default). Name of the directory in which GRASS is installed. Example for a Windows system: \code{'C:/Program Files/GRASS GIS 8.2'}. If this is \code{NULL}, R will search for the directory in which GRASS is installed. This usually fails, or if it succeeds, takes several minutes.
#' @param alreadyInGrass Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{rast}. If \code{FALSE}, use a raster already in GRASS with the name given by \code{rast}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#' @param grassToR Logical, if \code{TRUE} (default) then the output will be returned to R. If \code{FALSE}, then the output is left in the GRASS session and named the value in \code{outGrassName}. The latter is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param outGrassName Character. Name of output in GRASS. This is useful if you want to refer to the output object in GRASS later in a session.
#' @param ... Arguments to pass to \code{\link[rgrass]{execGRASS}}.
#'
#' @return If \code{grassToR} if \code{TRUE}, then a raster with the same extent, resolution, and coordinate reference system as \code{rast}. Regardless, a raster with a name given by \code{outGrassName} is written into the GRASS session.
#'
#' @seealso \code{\link[terra]{buffer}} in the \pkg{terra} package
#' @examples
#'
#' \donttest{
#'
#' # change this to where GRASS is installed on your system
#' grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
#' grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac
#'
#' library(terra)
#' rastFile <- system.file('extdata/madForest2000.tif', package='fasterRaster')
#' madForest2000 <- rast(rastFile)
#' rastBuff <- fasterBufferRast(rast=madForest2000, width=2,
#' units='kilometers', grassDir=grassDir)
#'
#' plot(rastBuff, col=c('green', 'gray'))
#' legend('topright', legend=c('forest', 'buffer'), fill=c('green', 'gray'))
#'
#' }
#'
#' @export

fasterBufferRast <- function(
	rast,
	width,
	units = 'meters',
	ignore = NA,
	lowMemory = FALSE,
	grassDir = options('grassDir'),
	alreadyInGrass = FALSE,
	grassToR = TRUE,
	outGrassName = 'rastBuffer',
	...
) {

	flags <- c('quiet', 'overwrite')
	if (!is.na(ignore) && ignore == 0) flags <- c(flags, 'z')

	# initialize GRASS and export raster to GRASS
	input <- initGrass(alreadyInGrass=alreadyInGrass, rast=rast, vect=NULL, grassDir=grassDir)
		
	# buffer
	fx <- if (lowMemory) {
		'r.buffer.lowmem'
	} else {
		'r.buffer'
	}

	rgrass::execGRASS(fx, input=input, output=outGrassName, distances=width, units=units, flags=flags)

	# return
	if (grassToR) {

		out <- rgrass::read_RAST(outGrassName)
		names(out) <- outGrassName
		out

	}

}

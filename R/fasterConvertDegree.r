#' Convert degrees between cases with 0 = north and 0 = east
#' 
#' This function takes either a numeric vector or a raster where values represent degrees (typically in a direction). It then converts between cases where 0 degrees (direction) is intended to point east, with degrees running counterclockwise so 90 is north, 180 is west, and 270 south (this the the default for GRASS 7 modules) and cases where 0 degrees is intended to point north, with degrees running clockwise, so 90 is east, 180 south, and 270 west.
#' @param x Numeric vector or raster. Values should represent degrees. The conversion is symmetric, so \code{x} can represent degrees in the system where 0 is north or 0 is east.
#' @param grassDir Character or \code{NULL} (default). Name of the directory in which GRASS is installed. Example: \code{'C:/Program Files/GRASS GIS 7.8'}. If this is \code{NULL}, R will search for the directory in which GRASS is installed. This usually fails, or if it succeeds, takes several minutes.
#' @param alreadyInGrass Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{x}. If \code{FALSE}, use a raster already in GRASS with the name given by \code{x}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#' @param grassToR Logical, if \code{TRUE} (default) then the output will be a raster or raster stack returned to R. If \code{FALSE}, then the rasters are left in the GRASS session and named \code{degreeConvert_1}, \code{degreeConvert_2}, \code{degreeConvert_3} (one per raster in \code{x}). The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param outGrassName Character. Name of output in GRASS. This is useful if you want to refer to the output object in GRASS later in a session.
#' @param ... Arguments to send to \code{\link[rgrass7]{execGRASS}}.
#' @return Numeric, raster, or raster stack. Regardless, a set of rasters will be written into the current GRASS session. The name(s) of the rasters inn GRASS wil be given by \code{outGrassName} with \code{'_', i} appended, where \code{i} starts at 1 for the first raster and increments by 1. For example, if \code{outGrassName = 'degreeConvert'}, then the raster(s) will be named \code{degreeConvert_1}, \code{degreeConvert_2}, etc.
#' @examples
#' # examples with scalar values
#' fasterConvertDegree(0)
#' fasterConvertDegree(seq(0, 360, 90))
#'
#' \donttest{
#' # change this according to where GRASS 7 is installed on your system
#' grassDir <- 'C:/Program Files/GRASS GIS 7.8'
#'
#' # example with a raster
#' # first, generate an aspect raster from an elevation raster
#' data(madElev)
#' aspect <- fasterTerrain(madElev, slope=FALSE, aspect=TRUE,
#' grassDir=grassDir)
#' names(aspect) <- 'original'
#' aspectNew <- fasterConvertDegree(aspect, grassDir=grassDir)
#' par(mfrow=c(1, 2))
#' plot(aspect, main='0 deg = North')
#' plot(aspectNew, main='0 deg = East')
#'
#' # example with a raster stack
#' aspectStack <- stack(aspect, aspect)
#' aspectStackNew <- fasterConvertDegree(aspectStack, grassDir=grassDir)
#' aspectStackNew
#' }
#' @export

fasterConvertDegree <- function(
	x,
	grassDir = NULL,
	alreadyInGrass = FALSE,
	returnToR = TRUE,
	outGrassName = 'degreeConvert',
	...
) {

	## scalar
	if (class(x) %in% c('numeric', 'integer')) {
		
		out <- (360 - x) %% 360
	
	## raster(s)
	} else {
	
		flags <- c('quiet', 'overwrite')
		
		inNames <- names(x)
		
		# initialize GRASS
		input <- initGrass(alreadyInGrass, rast=x, vect=NULL, grassDir=grassDir)
		
		## raster stack/brick
		if (class(x) %in% c('RasterStack', 'RasterBrick')) {

			for (i in 1:raster::nlayers(x)) {
		
				thisRast <- x[[i]]
				exportRastToGrass(thisRast, grassName=paste0('rast_', i))
			
				# execute
				expression <- paste0(outGrassName, '_', i, ' = (360 - ', paste0('rast_', i), ') % 360')
				rgrass7::execGRASS('r.mapcalc', expression=expression, flags=flags, ...)
				
				if (grassToR) {
					
					thisOut <- rgrass7::readRAST(paste0(outGrassName, '_', i))
					thisOut <- raster::raster(thisOut)
					
					out <- if (exists('out', inherits = FALSE)) {
						raster::stack(out, thisOut)
					} else {
						thisOut
					}
					
				}
				
			} # next raster
			
		## single raster
		} else {
			
			# execute
			expression <- paste0(outGrassName, '_1 = (360 - rast) % 360')
			rgrass7::execGRASS('r.mapcalc', expression=expression, flags=flags, ...)
			
			if (grassToR) {
				
				out <- rgrass7::readRAST(paste0(outGrassName, '_1'))
				out <- raster::raster(out)
				
			}
			
		}
	
		names(out) <- inNames
	
	}
	
	
	if (class(x) %in% c('numeric', 'integer') | returnToR) out
	
}

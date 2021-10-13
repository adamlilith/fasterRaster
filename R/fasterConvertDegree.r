#' Convert degrees between cases with 0 = north and 0 = east
#' 
#' This function takes either a numeric vector or a raster where values represent degrees (typically in a direction). It then converts between cases where 0 degrees (direction) is intended to point east, with degrees running counterclockwise so 90 is north, 180 is west, and 270 south (this the the default for GRASS 7 modules) and cases where 0 degrees is intended to point north, with degrees running clockwise, so 90 is east, 180 south, and 270 west.
#' @param x Numeric vector or raster. Values should represent degrees. The conversion is symmetric, so \code{x} can represent degrees in the system where 0 is north or 0 is east.
#' @param grassDir Character or \code{NULL} (default). Name of the directory in which GRASS is installed. Example: \code{'C:/Program Files/GRASS GIS 7.8'}. If this is \code{NULL}, R will search for the directory in which GRASS is installed. This usually fails, or if it succeeds, takes several minutes.
#' @param alreadyInGrass Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{x}. If \code{FALSE}, use a raster already in GRASS with the name given by \code{x}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#' @param grassToR Logical, if \code{TRUE} (default) then the output will be a raster or raster stack returned to R. If \code{FALSE}, then the rasters are left in the GRASS session and named \code{degreeConvert_1}, \code{degreeConvert_2}, \code{degreeConvert_3} (one per raster in \code{x}). The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param outGrassName Character. Name of output in GRASS. This is useful if you want to refer to the output object in GRASS later in a session.
#' @param ... Arguments to send to \code{\link[rgrass7]{execGRASS}}.
#' @return Numeric, raster, or raster stack. Regardless, a set of rasters will be written into the current GRASS session. The name(s) of the rasters inn GRASS will be given by \code{outGrassName} with \code{'_', i} appended, where \code{i} starts at 1 for the first raster and increments by 1. For example, if \code{outGrassName = 'degreeConvert'}, then the raster(s) will be named \code{degreeConvert_1}, \code{degreeConvert_2}, etc.
#' @examples
#' # Examples with scalar values:
#' fasterConvertDegree(0)
#' fasterConvertDegree(seq(0, 360, by=90))
#'
#' \donttest{
#' # change this according to where GRASS 7 is installed on your system
#' grassDir <- 'C:/Program Files/GRASS GIS 7.8' # example for a PC
#' grassDir <- "/Applications/GRASS-7.8.app/Contents/Resources" # for a Mac
#'
#' # Example with a raster:
#' # First, generate an aspect raster from an elevation raster.
#' # This is a contrived example because we could set the argument northIs0 in
#' # fasterTerrain() to TRUE and get the raster we will name aspNorthIs0 from
#' # that function in one step.
#' data(madElev)
#' aspEastIs0 <- fasterTerrain(madElev, slope=FALSE, aspect=TRUE,
#' northIs0=FALSE, grassDir=grassDir)
#' aspNorthIs0 <- fasterConvertDegree(aspEastIs0, grassDir=grassDir,
#' outGrassName='aspectNorthIs0')
#' par(mfrow=c(1, 2))
#' plot(aspEastIs0, main='0 deg = east')
#' plot(aspNorthIs0, main='0 deg = north')
#'
#' # example with a raster stack
#' aspectStack <- stack(aspEastIs0, aspNorthIs0)
#' aspectStackNew <- fasterConvertDegree(aspectStack, grassDir=grassDir)
#' aspectStackNew
#' }
#' @export

fasterConvertDegree <- function(
	x,
	grassDir = NULL,
	alreadyInGrass = FALSE,
	grassToR = TRUE,
	outGrassName = 'degreeConvert',
	...
) {

	## scalar
	if (class(x) %in% c('numeric', 'integer')) {
		
		out <- ((360 - x) %% 360 + 90) %% 360
	
	## raster(s)
	} else {
	
		flags <- c('quiet', 'overwrite')
		
		# initialize GRASS
		input <- initGrass(alreadyInGrass, rast=x, vect=NULL, grassDir=grassDir)
		
		## raster stack/brick
		if (class(x) %in% c('RasterStack', 'RasterBrick')) {

			for (i in 1:raster::nlayers(x)) {
		
				thisRast <- x[[i]]
				exportRastToGrass(thisRast, grassName=paste0('rast_', i))
			
				# execute
				ex <- paste0(outGrassName, '_', i, ' = ((360 - rast_', i, ') % 360 + 90) % 360')
				fasterMapcalc(paste0('rast_', i), expression=ex, grassDir=grassDir, alreadyInGrass=TRUE, grassToR=FALSE)
				
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
			ex <- if (class(x) == 'character') {
				paste0(outGrassName, ' = ((360 - ', x, ') % 360 + 90) % 360')
			} else {
				paste0(outGrassName, ' = ((360 - rast) % 360 + 90) % 360')
			}
			fasterMapcalc('rast', expression=ex, grassDir=grassDir, alreadyInGrass=TRUE, grassToR=FALSE)
			
			if (grassToR) {
				
				out <- rgrass7::readRAST(paste0(outGrassName))
				out <- raster::raster(out)
				
			}
			
		}
	
		if (grassToR) {
			numOut <- raster::nlayers(out)
			names(out) <- if (numOut == 1) {
				outGrassName
			} else if (length(outGrassName) > numOut) {
				outGrassName[1:numOut]
			} else if (length(outGrassName) < numOut) {
				paste0(rep(outGrassName), '_', 1:numOut)
			} else {
				outGrassName
			}
		}
	
	}
	
	if (class(x) %in% c('numeric', 'integer') | grassToR) out
	
}

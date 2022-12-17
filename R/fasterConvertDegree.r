#' Convert degrees between cases with 0 = north and 0 = east
#' 
#' This function takes either a numeric vector or a raster where values represent degrees (typically in a direction). It then converts between cases where 0 degrees (direction) is intended to point east, with degrees running counterclockwise so 90 is north, 180 is west, and 270 south (this the the default for \code{GRASS} 7 modules) and cases where 0 degrees is intended to point north, with degrees running clockwise, so 90 is east, 180 south, and 270 west.
#'
#' @param x Numeric vector or \code{SpatRaster}. Values should represent degrees. The conversion is symmetric, so \code{x} can represent degrees in the system where 0 is north or 0 is east.
#' @inheritParams .sharedArgs_grassDir_grassToR
#' @inheritParams .sharedArgs_outGrassName
#' @param ... Arguments to send to \code{\link[rgrass]{execGRASS}}.
#' @return Numeric, raster, or raster stack. Regardless, a set of rasters will be written into the current \code{GRASS} session. The name(s) of the rasters inn \code{GRASS} will be given by \code{outGrassName} with \code{'_', i} appended, where \code{i} starts at 1 for the first raster and increments by 1. For example, if \code{outGrassName = 'degreeConvert'}, then the raster(s) will be named \code{degreeConvert_1}, \code{degreeConvert_2}, etc.
#'
#' @examples man/examples/ex_fasterConvertDegree.r
#'
#' @export

fasterConvertDegree <- function(
	x,
	grassDir = options()$grassDir,
	grassToR = TRUE,
	outGrassName = 'degreeConvert',
	...
) {

	## scalar
	if (inherits(x, c('numeric', 'integer'))) {
		
		out <- ((360 - x) %% 360 + 90) %% 360
	
	## raster(s)
	} else {
	
		flags <- c('quiet', 'overwrite')
		
		# initialize GRASS
		input <- initGrass(rast=x, vect=NULL, grassDir=grassDir)
		
		## raster stack/brick
		if (inherits(x, c('SpatRaster'))) {

			for (i in 1L:terra::nlyr(x)) {
		
				thisRast <- x[[i]]
				exportRastToGrass(thisRast, grassName=paste0('rast_', i))
			
				# execute
				ex <- paste0(outGrassName, '_', i, ' = ((360 - rast_', i, ') % 360 + 90) % 360')
				fasterMapcalc(paste0('rast_', i), expression=ex, grassDir=grassDir, grassToR=FALSE)
				
				if (grassToR) {
					
					thisOut <- rgrass::read_RAST(paste0(outGrassName, '_', i))
					thisOut <- terra::rast(thisOut)
					
					out <- if (exists('out', inherits = FALSE)) {
						c(out, thisOut)
					} else {
						thisOut
					}
					
				}
				
			} # next raster
			
		## single raster
		} else {
			
			# execute
			ex <- if (inherits(x, 'character')) {
				paste0(outGrassName, ' = ((360 - ', x, ') % 360 + 90) % 360')
			} else {
				paste0(outGrassName, ' = ((360 - rast) % 360 + 90) % 360')
			}
			fasterMapcalc('rast', expression=ex, grassDir=grassDir, grassToR=FALSE)
			
			if (grassToR) {
				
				out <- rgrass::read_RAST(paste0(outGrassName))
				out <- terra::rast(out)
				
			}
			
		}
	
		if (grassToR) {
			numOut <- terra::nlyr(out)
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
	
	if (inherits(x, c('numeric', 'integer')) | grassToR) out
	
}

#' Convert degrees between cases with 0 = north and 0 = east
#' 
#' This function takes either a numeric vector or a raster where values represent degrees (typically in a direction). It then converts between cases where 0 degrees (direction) is intended to point east, with degrees running counterclockwise so 90 is north, 180 is west, and 270 south (this the the default for \code{GRASS} 7 modules) and cases where 0 degrees is intended to point north, with degrees running clockwise, so 90 is east, 180 south, and 270 west.
#'
#' @param x Numeric vector or \code{SpatRaster}. Values should represent degrees. The conversion is symmetric, so \code{x} can represent degrees in the system where 0 is north or 0 is east.
#' @inheritParams .sharedArgs_grassDir_grassToR
#' @inheritParams .sharedArgs_outGrassName
#'
#' @return Numeric, raster, or raster stack. Regardless, a set of rasters will be written into the current \code{GRASS} session. The name(s) of the rasters in \code{GRASS} will be given by \code{outGrassName} with \code{'_', i} appended, where \code{i} starts at 1 for the first raster and increments by 1. For example, if \code{outGrassName = 'degreeConvert'}, then the raster(s) will be named \code{degreeConvert_1}, \code{degreeConvert_2}, etc.
#'
#' @example man/examples/ex_fasterConvertDegree.r
#'
#' @export

fasterConvertDegree <- function(
	x,
	grassDir = options()$grassDir,
	grassToR = TRUE,
	inRastName = ifelse(is.null(names(rast)), 'rast', names(rast)),
	outGrassName = 'degreeConvertRast'
) {

	## scalar
	if (inherits(x, c('numeric', 'integer'))) {
		
		out <- ((360 - x) %% 360 + 90) %% 360
	
	## raster
	} else {
	
		flags <- c('quiet', 'overwrite')
		
		# initialize GRASS
		input <- initGrass(rast=rast, vect=NULL, inRastName=inRastName, inVectName=NULL, grassDir=grassDir)
		
		# execute
		ex <- paste0(outGrassName, ' = ((360 - ', inRastName, ') % 360 + 90) % 360')
		fasterApp(input, expression=ex, grassDir=grassDir, grassToR=FALSE, outGrassName=outGrassName)

		if (grassToR) {
			
			out <- rgrass::read_RAST(paste0(outGrassName))
			names(out) <- outGrassName
			
		}
		
	}
	
	if (inherits(x, c('numeric', 'integer')) | grassToR) out
	
}
